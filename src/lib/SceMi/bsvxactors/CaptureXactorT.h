// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include <string>
#include <iostream>
#include <fstream>
#include <bitset>
#include <limits>

#include "OutportProxyT.h"
#include "InportProxyT.h"
#include "Thread.h"
#include "BSVCaptureT.h"
#include "VCDWriter.h"

template <typename T> class CaptureXactorT;

class CaptureStatus {
 public:
  BitT<64> m_trigger_cycle ;
  BitT<32> m_trigger_offset ;
  BitT<32> m_num_samples ;

  CaptureStatus ()
    : m_trigger_cycle()
    , m_trigger_offset()
    , m_num_samples()
  {}

  CaptureStatus ( const SceMiMessageData *msg, unsigned int &off )
    : m_trigger_cycle(msg, off)
    , m_trigger_offset(msg, off)
    , m_num_samples(msg, off)
  {}

  unsigned int setMessageData (SceMiMessageData &msg, const unsigned int off=0) const {
    unsigned int running = off;
    running = m_num_samples.setMessageData( msg, running );
    running = m_trigger_offset.setMessageData( msg, running );
    running = m_trigger_cycle.setMessageData( msg, running );
    if (running != off + 96 ) {
      std::cerr << "Mismatch in sizes: " << std::dec <<  running << " vs " << (off + 96) << std::endl;
    }
    return running;
  }

  friend std::ostream & operator<< (std::ostream &os, const CaptureStatus &obj) {
    os << "{" ;
    os << "num_samples " << obj.m_num_samples ;os << " " ;
    os << "trigger_offset " << obj.m_trigger_offset ;os << " " ;
    os << "trigger_cycle " << obj.m_trigger_cycle ;os << "}" ;
    return os;
  }

  std::ostream & getBitString (std::ostream & os) const {
    m_num_samples.getBitString (os);
    m_trigger_offset.getBitString (os);
    m_trigger_cycle.getBitString (os);
  return os;
  }

  static std::ostream & getBSVType (std::ostream & os) {
    os << "CaptureStatus" ;
    return os;
  }

  static unsigned int getBitSize () {
    return 128;
  }

  unsigned int getNumSamples() const { return m_num_samples.get(); }
  unsigned int getTriggerOffset() const { return m_trigger_offset.get(); }
  SceMiU32 getTriggerCycle() const { return m_trigger_cycle.get(); }
};

// Class to capture capture information to a VCD file
// Uses the call back function of the capture outport proxy
// and deletes the message.
template <typename T>
class CaptureToVCDFileT {
 private:
  class VCDWriter & vcd_writer;
  class CaptureXactorT<T> & m_captureTx;
  SceMiU64 c_stamp;
 public:
  CaptureToVCDFileT (VCDWriter & writer, CaptureXactorT<T> &capturex)
    : vcd_writer(writer)
    , m_captureTx(capturex)
  {
    capturex.setCallBack (CaptureToVCDFileT<T>::logCapture, (void *) this);
    writer.registerCapture(&capturex);
    c_stamp = 0;
  }
  ~CaptureToVCDFileT() {
    m_captureTx.setCallBack(NULL, (void *) this);
    vcd_writer.unregisterCapture(&m_captureTx);
    vcd_writer.endVCDFile();
  }
 public:
  unsigned int getBitSize() { return m_captureTx.getBitSize(); }
  unsigned int getID() { return vcd_writer.getID(m_captureTx); }
  void startVCDFile() { vcd_writer.startVCDFile(); }
  void setVCDFileName(const char *n) { vcd_writer.setVCDFile(n); }
  unsigned int updateRemaining(SceMiU32 runlength)
  { return m_captureTx.updateRemaining(runlength); }
  void set_c_stamp(SceMiU64 t) { c_stamp = t; }
  void computeGreatestCaptureInterval() { vcd_writer.computeGreatestCaptureInterval(); }

  static void logCapture (void *vptr, const BSVCaptureT<T> &probemsg) {

    CaptureToVCDFileT<T> * ptof = (CaptureToVCDFileT<T> *) vptr;

    SceMiU32 run_length;

    T msg = probemsg.getCapture();

    ptof->startVCDFile();
    ptof->c_stamp += run_length;
    ptof->addChangeProbe(ptof->c_stamp, ptof->getID(), (BSVType *) & msg, false);

    if (ptof->updateRemaining(run_length) == 1)
      {
        // Goes to Z in next cycle
	ptof->addChangeProbe(ptof->c_stamp+1, ptof->getID(),  (BSVType *) & msg, true);
      }
  }
};


class CaptureXactor {
 public:
  typedef BitT<1>  Bool;

 protected:

  OutportProxyT<CaptureStatus>   m_statusout;
  InportProxyT<Bool>             m_probectrl;
  bool                           m_enabled;
  unsigned int                   num_captured;
  std::string                    capture_name;
  std::string                    vcd_filename;
  unsigned int                   num_samples;
  unsigned int                   samples_captured;

  // Protected constructor
  CaptureXactor ( const std::string & hier, const std::string & inst, const std::string & capture,
		  class SceMi *scemi )
    : m_statusout(hier, inst + "_status_out", scemi)
    , m_probectrl(hier, inst + "_control_in", scemi)
    , m_enabled(false)
    , num_captured(0)
    , capture_name(capture)
    , num_samples(0)
  { }

 private:

  // Disallow default and copy constructors
  CaptureXactor & operator= (const CaptureXactor &);
  CaptureXactor( const CaptureXactor &);

 public:
  virtual ~CaptureXactor() {}
  virtual void setDebug(bool val){
    m_statusout.setDebug(val);
    m_probectrl.setDebug(val);
  }
  // Set how many sets of intended captured data
  void setCapture ( bool mode ) {
    if (mode) ++num_captured;
    else num_captured = 0;
    m_probectrl.sendMessage( mode );
  }
  bool enabled () const {
    return (num_captured != 0) ;
  }
  const char *name() { return capture_name.c_str(); }
  //virtual void setDumpFileName(const char* fn) {}
  virtual void setVCDFileName(const char* fn, VCDWriter *writer) {}
  const char *vcdFileName() { return vcd_filename.c_str(); }
  virtual std::ostream & getBSVType (std::ostream &) const = 0;
  virtual unsigned int getBitSize () const = 0;
  virtual bool done_capturing() const = 0;
  unsigned int sampleInterval() { return num_samples; }
};


template <typename T>
class CaptureXactorT : public CaptureXactor {

  OutportProxyT<BSVCaptureT<T > >   m_captureout;
  CaptureToVCDFileT<T >             *vcd_file;

  bool                     offloading;
  unsigned int             trigger_offset;
  SceMiU32                 trigger_cycle;
  SceMiU32                 start_cycle;
  unsigned int             remaining;

 public:
  CaptureXactorT ( const std::string & hier, const std::string & inst, const std::string & capture,
		   class SceMi *scemi )
    : CaptureXactor(hier, inst, capture, scemi)
    , m_captureout(hier, inst + "_data_out", scemi)
    , vcd_file(NULL)
    {
      m_statusout.setCallBack (CaptureXactorT<T>::initiateOffloading, (void*) this);
      remaining = 0;
      offloading = false;
      trigger_offset = trigger_cycle = start_cycle = 0;
    }
  ~CaptureXactorT() { /* delete capture_file; */ }
  void setDebug(bool val){
    m_captureout.setDebug(val);
    CaptureXactor::setDebug(val);
  }
  void setCallBack( void func(void *, const BSVCaptureT<T> &), void * ptr){
    m_captureout.setCallBack (func, ptr);
  }
  void setVCDWriter(VCDWriter *writer) {
    if (vcd_file)
      delete vcd_file;
    vcd_file = new CaptureToVCDFileT<T> (*writer, *this);
  }
  virtual std::ostream & getBSVType (std::ostream &os) const {
    T x;
    return x.getBSVType(os);
  }
  virtual unsigned int getBitSize () const {
    T x;
    return x.getBitSize() ;
  }
  virtual bool done_capturing() const {
    return (num_captured == 0 && remaining == 0);
  }
  virtual unsigned int updateRemaining(SceMiU32 runlength) {
    if (remaining > 0) {
      remaining -= runlength;
      if (remaining <= 1) {
	offloading = false;
	if (num_captured == 0)
	  m_probectrl.sendMessage( 0 );
      }
    }
    return remaining;
  }

 private:
  static void initiateOffloading ( void * vptr, const CaptureStatus &status) {
    CaptureXactorT<T> * px = (CaptureXactorT<T> *) vptr;

    if (px->offloading)
      return;

    px->num_samples = status.getNumSamples();
    px->trigger_offset = status.getTriggerOffset();
    px->trigger_cycle = status.getTriggerCycle();
    px->start_cycle = px->trigger_cycle - px->trigger_offset;
    px->offloading = true;
    px->remaining = px->num_samples;
    --px->num_captured;
    px->vcd_file->set_c_stamp(px->start_cycle);
    px->vcd_file->computeGreatestCaptureInterval();
  }

};

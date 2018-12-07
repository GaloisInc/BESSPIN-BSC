// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

// Bluespec Probe transactor classes

#include <string>
#include <iostream>
#include <fstream>
#include <bitset>
#include <limits>

#include "OutportProxyT.h"
#include "InportProxyT.h"
#include "Thread.h"
#include "BSVProbeT.h"
#include "VCDWriter.h"

template <typename T> class ProbeXactorT;

// Class to capture probe information to file
// Uses the call back function of the probe outport proxy
// and deletes the message.
template <typename T>
class ProbeToFileT {
 private:
  std::ofstream          m_outfile;
  class ProbeXactorT<T> & m_probeTx;
 public:
  ProbeToFileT (const std::string & filename, ProbeXactorT<T> &probex)
    : m_probeTx(probex)
  {
    m_outfile.open (filename.c_str(), std::ios::out|std::ios::trunc);
    probex.setCallBack (ProbeToFileT<T>::logProbe, (void *) this);
  }
  ~ProbeToFileT()   {
    m_probeTx.setCallBack(NULL, (void *) this);
    m_outfile.close();
  }
 private:
  static void logProbe (void *vptr, const BSVProbeT<T> &msg) {
    SceMiU64 cycle_stamp = msg.getCycleStamp() ;
    char str[32];
    ProbeToFileT<T> * ptof = (ProbeToFileT<T> *) vptr;
    sprintf(str, "%lld ", cycle_stamp);
    ptof->m_outfile << str << msg.getProbe() << std::endl;
  }
};


// Class to capture probe information to a VCD file
// Uses the call back function of the probe outport proxy
// and deletes the message.
template <typename T>
class ProbeToVCDFileT {
 private:
  class VCDWriter & vcd_writer;
  class ProbeXactorT<T> & m_probeTx;
 public:
  ProbeToVCDFileT (VCDWriter & writer, ProbeXactorT<T> &probex)
    : vcd_writer(writer)
    , m_probeTx(probex)
  {
    probex.setCallBack (ProbeToVCDFileT<T>::logProbe, (void *) this);
    writer.registerProbe(&probex);
  }
  ~ProbeToVCDFileT()   {
    m_probeTx.setCallBack(NULL, (void *) this);
    vcd_writer.unregisterProbe(&m_probeTx);
    vcd_writer.endVCDFile();
  }
 private:
  unsigned int getBitSize() { return m_probeTx.getBitSize(); }
  unsigned int getID() { return vcd_writer.getID(m_probeTx); }
  void startVCDFile() { vcd_writer.startVCDFile(); }
  void setVCDFileName(const char *n) { vcd_writer.setVCDFile(n); }
  static void logProbe (void *vptr, const BSVProbeT<T> &probemsg) {

    ProbeToVCDFileT<T> * ptof = (ProbeToVCDFileT<T> *) vptr;
    unsigned int numbits = ptof->getBitSize();
    std::ostringstream os;
    std::string str;
    SceMiU64 cycle_stamp = probemsg.getCycleStamp();
    T msg = probemsg.getProbe();

    ptof->startVCDFile();
    ptof->addChangeProbe(cycle_stamp, ptof->getID(), (BSVType *) & msg, false);
    
  }
};


class ProbeXactor {
 public:
  typedef BitT<1>  Bool;

 protected:

  OutportProxyT<Bool>     m_statusout;
  InportProxyT<Bool>      m_probectrl;
  bool                    m_enabled;
  std::string             probe_name;
  std::string             dump_filename;
  std::string             vcd_filename;

  // Protected constructor
  ProbeXactor ( const std::string & hier, const std::string & inst, const std::string & probe,
		class SceMi *scemi )
    : m_statusout(hier, inst + "_status_out", scemi)
    , m_probectrl(hier, inst + "_control_in", scemi)
    , m_enabled(false)
    , probe_name(probe)
    {}

 private:

  // Disallow default and copy constructors
  ProbeXactor & operator= (const ProbeXactor &);
  ProbeXactor( const ProbeXactor &);
 public:
  virtual ~ProbeXactor() {}
  virtual void setDebug(bool val){
    m_statusout.setDebug(val);
    m_probectrl.setDebug(val);
  }
  void setProbe ( bool mode ) {
    m_probectrl.sendMessage( mode );
  }
  bool enabled () const {
    return m_enabled ;
  }
  const char *name() { return probe_name.c_str(); }
  virtual void setDumpFileName(const char* fn) {} /* TODO */
  virtual void setVCDFileName(const char* fn, VCDWriter *writer) {}
  const char *dumpFileName() { return dump_filename.c_str(); }
  const char *vcdFileName() { return vcd_filename.c_str(); }
  virtual std::ostream & getBSVType (std::ostream &) const = 0;
  virtual unsigned int getBitSize () const = 0;
  virtual BSVType * getProbeObject () const = 0;
};


template <typename T>
class ProbeXactorT : public ProbeXactor {

  OutportProxyT<BSVProbeT<T > >    m_probeout;
  ProbeToFileT<T>        *probe_file;
  ProbeToVCDFileT<T>     *vcd_file;
  T                      dummyObject;

 public:
  ProbeXactorT ( const std::string & hier, const std::string & inst, const std::string & probe,
		 class SceMi *scemi )
    : ProbeXactor(hier, inst, probe, scemi)
    , m_probeout(hier, inst + "_probe_out", scemi)
    , probe_file(NULL)
    , vcd_file(NULL)
    {
      m_statusout.setCallBack (ProbeXactorT<T>::setStatus, (void*) this);
    }
  ~ProbeXactorT() { delete probe_file; }
  void setDebug(bool val){
    m_probeout.setDebug(val);
    ProbeXactor::setDebug(val);
  }
  void setCallBack( void func(void *, const BSVProbeT<T> &), void * ptr){
    m_probeout.setCallBack (func, ptr);
  }
  void setDumpFileName( const char *fn ) {
    dump_filename = fn;
    if (probe_file)
      delete probe_file;
    probe_file = new ProbeToFileT<T> (dump_filename.c_str(), *this);
  }
  void setVCDWriter(VCDWriter *writer) {
    if (vcd_file)
      delete vcd_file;
    vcd_file = new ProbeToVCDFileT<T> (*writer, *this);
  }
  virtual std::ostream & getBSVType (std::ostream &os) const {
    T x;
    return x.getBSVType(os);
  }
  virtual unsigned int getBitSize () const {
    T x;
    return x.getBitSize() ;
  }
  virtual BSVType * getProbeObject () const {
    return (BSVType*) & dummyObject;
  }



 private:
  static void setStatus ( void * vptr, const Bool &en ) {
    ProbeXactorT<T> * px = (ProbeXactorT<T> *) vptr;
    px->m_enabled = en.get();
  }
};


// Copyright © 2003-2007 by Accellera

#ifndef __SCEMI_PROXIES_H__
#define __SCEMI_PROXIES_H__

#include <string>

#include "sized_types.h"
#include "SceMiEC.h"
#include "Link.h"

class SceMiMessageData;

/*
 * struct SceMiMessageInPortBinding
 *
 * Description
 * -----------
 * This structure defines a tray of callback functions that support
 * propagation of message input readiness back to the software.
 *
 * If an input ready callback is registered (optionally) on a given
 * input port, the port will dispatch the callback whenever becomes
 * ready for more input.
 *
 * Note: All callbacks must take their data and return promptly as they
 * are called possibly deep down in a non-preemptive thread. Typically,
 * the callback might to some minor manipulation to the context object
 * then return and let a suspended thread resume and do the main process¬ing
 * of the received transaction.
 */
extern "C" {
    typedef struct {
	/*
	 * This is the user's context object pointer.
	 * The application is free to use this pointer for any purposes it
	 * wishes. Neither the class SceMi nor class MessageInputPortProxy do
	 * anything with this pointer other than store it and pass it when
	 * calling functions.
	 */
	void* Context;

	/*
	 * Notification that the channel is ready to accept additional messages.
	 * This callback acts as a proxy for the message input port of the
	 * transactor.
	 */
	void (*IsReady)(void* context);

	/*
	 * This function is called from the MessageInPortProxy destructor
	 * to notify the user code that the reference to the 'context' pointer
	 * has been deleted.
	 */
        void (*Close)(void* context);

    } SceMiMessageInPortBinding;
}

/*
 * struct SceMiMessageOutPortBinding
 *
 * Description
 * -----------
 * This structure defines a tray of callback functions are passed to the class
 * SceMi when the application model binds to a message output port proxy and
 * which are called on message receipt and close notification. It is the means
 * by which the MessageOutputPort forwards received transactions to the C model.
 *
 * Note: All callbacks must take their data and return promptly as they
 * are called possibly deep down in a non-preemptive thread. Typically,
 * the callback might to some minor manipulation to the context object
 * then return and let a suspended thread resume and do the main process¬ing
 * of the received transaction.
 *
 * Additionally, the message data passed into the receive callback is
 * not guaranteed to remain the same once the callback returns. All
 * data therein then must be processed while inside the callback.
 */

extern "C" {
    typedef struct {
	/*
	 * This is the user's context object pointer.
	 * The application is free to use this pointer for any purposes it
	 * wishes. Neither the class SceMi nor class SceMiMessageOutPortProxy do
	 * anything with this pointer other than store it and pass it when
	 * calling callback functions Receive and Close.
	 */
	void* Context;

	/*
	 * Receive a response transaction. This function is called when data
	 * from the message output port arrives. This callback acts as a proxy
	 * for the message output port of the transactor.
	 */
	void (*Receive)(void* context,
			const SceMiMessageData* data);

	/*
	 * This function is called from the MessageOutPortProxy destructor
	 * to notify the user code that the reference to the 'context' pointer
	 * has been deleted.
	 */
         void (*Close)(void* context);

    } SceMiMessageOutPortBinding;
}

//
// class SceMiMessageInPortProxy
//
// Description

// -----------
// The class SceMiMessageInPortProxy presents a C++ proxy for a transactor
// message input port. The input channel to that transactor is represented
// by the Send() method.
//
class SceMiMessageInPortProxy {

private:

  std::string transactorName;
  std::string portName;
  unsigned int portWidth;
  unsigned int channelId;
  Link* link;

  SceMiMessageInPortBinding binding;

public:
  SceMiMessageInPortProxy(const char* xactor,
			  const char* port,
			  unsigned int width,
			  unsigned int channel,
			  Link* link);
  ~SceMiMessageInPortProxy();

  // ACCESSORS
  const char* TransactorName() const { return transactorName.c_str(); }
  const char* PortName() const { return portName.c_str(); }
  unsigned int PortWidth() const { return portWidth; }
  unsigned int ChannelId() const { return channelId; }
  const SceMiMessageInPortBinding& Binding() const { return binding; }
  Link* LinkPtr() const { return link; }

  //
  // This method sends a message to the transactor input port.
  //
  void Send(const SceMiMessageData &data, // Message payload to be sent.
	    SceMiEC* ec = NULL);

  //
  // This method sends a message to the transactor input port, but
  // does not preserve the message data contents for re-use.
  //
  void SendDestructive(const SceMiMessageData &data, // Message payload to be sent.
		       SceMiEC* ec = NULL);

  //
  // Replace port binding.
  // The binding argument represents a callback function and context
  // pointer tray (see comments in scemicommontypes.h for struct
  // SceMiMessageInPortBinding).
  //
  void ReplaceBinding(const SceMiMessageInPortBinding* new_binding = NULL,
		      SceMiEC* ec = NULL);
};

//
// class SceMiMessageOutPortProxy
//
// Description
// -----------
// The class SceMiMessageOutPortProxy presents a C++ proxy for a transac¬tor
// message output port.
//
class SceMiMessageOutPortProxy {

private:

  std::string transactorName;
  std::string portName;
  unsigned int portWidth;
  unsigned int channelId;
  Link* link;

  SceMiMessageOutPortBinding binding;

public:
  SceMiMessageOutPortProxy(const char* xactor,
			   const char* port,
			   unsigned int width,
			   unsigned int channel,
			   Link* link);
  ~SceMiMessageOutPortProxy();

  // ACCESSORS
  const char* TransactorName() const { return transactorName.c_str(); }
  const char* PortName() const { return portName.c_str(); }
  unsigned int PortWidth() const { return portWidth; }
  unsigned int ChannelId() const { return channelId; }
  const SceMiMessageOutPortBinding& Binding() { return binding; }
  Link* LinkPtr() const { return link; }

  //
  // Replace port binding.
  // The binding argument represents a callback function and context
  // pointer tray (see comments in scemicommontypes.h for struct
  // SceMiMessageOutPortBinding).
  //
  void ReplaceBinding(const SceMiMessageOutPortBinding* new_binding = NULL,
		      SceMiEC* ec = NULL);
};


//
// class SceMiMessageDataInterface
//

// Description
// -----------
// Abstracts just the methods currently used by the automatically generated C++ classes,
// allowing for alternate implementation (e.g., SceMi 2).
//
class SceMiMessageDataInterface {
public:
  virtual ~SceMiMessageDataInterface() {} ;
  virtual void SetBitRange(unsigned int i, unsigned int range, SceMiU32 bits, SceMiEC* ec = 0) =0;
  virtual SceMiU32 GetBitRange(unsigned int i, unsigned int range, SceMiEC* ec = 0) const =0;
  virtual void SetBlockRange(unsigned int i, unsigned int range, const char *data) =0;
  virtual void GetBlockRange(unsigned int i, unsigned int range, char *data) const =0;
 protected:
  //NB: range should be ONE LESS than the number of bits to be set or
  //gotten, according to a confusing point in the SceMi 2.0
  //specification.
  static void uncheckedSetBitRange(unsigned int i,
				   unsigned int range,
				   SceMiU32 bits,
                                   SceMiU32* buffer);
  static SceMiU32 uncheckedGetBitRange(unsigned int i, unsigned int range, const SceMiU32* buffer);
};


//
// class SceMiMessageData
//

// Description
// -----------
// The class SceMiMessageData represents a fixed length array of data which
// is transferred between models.
//

class SceMiMessageData : public SceMiMessageDataInterface {
private:
    Link*           m_link;
    mutable Packet* m_pkt;
    SceMiU64        m_cycle;

public:
    // CREATORS
    //
    // Constructor: The message in port proxy for which
    // this message data object must be suitably sized.
    //
    SceMiMessageData(const SceMiMessageInPortProxy& messageInPortProxy,
		     SceMiEC* ec = NULL)
    {
      m_link = messageInPortProxy.LinkPtr();
      unsigned int width = messageInPortProxy.PortWidth();
      m_pkt = m_link->packet(width);
      m_pkt->channel = messageInPortProxy.ChannelId();
      m_cycle = 0llu;
    }

    // This constructor is not part of the SCE-MI API, but is part of
    // the Bluespec implementation.
    SceMiMessageData(Link* link, Packet* packet, SceMiU64 cycle)
      : m_link(link), m_pkt(packet), m_cycle(cycle)
    {
    }

    ~SceMiMessageData()
    {
      if (m_link)
        m_link->release(m_pkt);
    }

    // Return size of vector in bits
    unsigned int WidthInBits() const { return m_pkt->num_bits; }

    // Return size of array in 32 bit words.
    unsigned int WidthInWords() const { return bits_to_words(m_pkt->num_bits); }

    void Set(unsigned i, SceMiU32 word, SceMiEC* ec = 0);

    void SetBit(unsigned i, int bit, SceMiEC* ec = 0);

    //NB: range should be ONE LESS than the number of bits to be set
    //or gotten, according to a confusing point in the SceMi 2.0
    //specification.
    void SetBitRange(unsigned int i, unsigned int range, SceMiU32 bits, SceMiEC* ec = 0);

    SceMiU32 Get(unsigned i, SceMiEC* ec = 0) const;

    int GetBit(unsigned i, SceMiEC* ec = 0) const;

    SceMiU32 GetBitRange(unsigned int i, unsigned int range, SceMiEC* ec = 0) const;
	
	void SetBlockRange(unsigned int i, unsigned int range, const char *data);
	
	void GetBlockRange(unsigned int i, unsigned int range, char *data) const;

    SceMiU64 CycleStamp() const;

    Packet* copyPacket() const;

    Packet* takePacket() const;

    void print_data() const;
};


#endif /* __SCEMI_PROXIES_H__ */

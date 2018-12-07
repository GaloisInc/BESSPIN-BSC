// Copyright (c) 2008 Bluespec, Inc; all rights reserved
#include <iostream>
#include <iomanip>
#include <cstring>
#include <cstdio>

#include "SceMiTypes.h"
#include "SceMiProxies.h"
#include "scemi.h"

// ****************************************************************
// class SceMiMessageInPortProxy methods

SceMiMessageInPortProxy::SceMiMessageInPortProxy(const char* xactor,
						 const char* port,
						 unsigned int width,
						 unsigned int channel,
						 Link* lnk)
  : transactorName(xactor), portName(port),
      portWidth(width), channelId(channel), link(lnk)
{
  binding.Context = NULL;
  binding.IsReady = NULL;
  binding.Close   = NULL;
}

SceMiMessageInPortProxy::~SceMiMessageInPortProxy()
{
  if (binding.Close) binding.Close(binding.Context);
}

//
// This method sends a message to the transactor input port.
//
void SceMiMessageInPortProxy::Send(const SceMiMessageData &data,
				   SceMiEC* ec)
{
  Packet* pkt = data.copyPacket();
  pkt->channel = channelId;
  link->queue(pkt);
}

//
// This method sends a message to the transactor input port.
// The data will not be the same after this call, which allows
// the implementation to be more efficient.  If you need to reuse
// the message contents, you should call Send() instead.
//
void SceMiMessageInPortProxy::SendDestructive(const SceMiMessageData &data,
					      SceMiEC* ec)
{
  Packet* pkt = data.takePacket();
  pkt->channel = channelId;
  link->queue(pkt);
}

//
// Replace port binding.
// The binding argument represents a callback function and context
// pointer tray (see comments in scemicommontypes.h for struct
// SceMiMessageInPortBinding).
//
void SceMiMessageInPortProxy::ReplaceBinding(const SceMiMessageInPortBinding* new_binding,
					     SceMiEC* ec)
{
  if (new_binding == NULL)
  {
    binding.Context = NULL;
    binding.IsReady = NULL;
    binding.Close   = NULL;
  }
  else
  {
    binding = *new_binding;
  }
}


// ****************************************************************
// class SceMiMessageOutPortProxy methods

SceMiMessageOutPortProxy::SceMiMessageOutPortProxy(const char* xactor,
						   const char* port,
						   unsigned int width,
						   unsigned int channel,
						   Link* lnk)
  : transactorName(xactor), portName(port),
      portWidth(width), channelId(channel), link(lnk)
{
  binding.Context = NULL;
  binding.Receive = NULL;
  binding.Close   = NULL;
}

SceMiMessageOutPortProxy::~SceMiMessageOutPortProxy()
{
  if (binding.Close) binding.Close(binding.Context);
}

//
// Replace port binding.
// The binding argument represents a callback function and context
// pointer tray (see comments in scemicommontypes.h for struct
// SceMiMessageOutPortBinding).
//
void SceMiMessageOutPortProxy::ReplaceBinding(const SceMiMessageOutPortBinding* new_binding,
					      SceMiEC* ec)
{
  if (new_binding == NULL)
  {
    binding.Context = NULL;
    binding.Receive = NULL;
    binding.Close   = NULL;
  }
  else
  {
    binding.Context = new_binding->Context;
    binding.Receive = new_binding->Receive;
    binding.Close   = new_binding->Close;
  }
}

// ****************************************************************
// class SceMiMessageData methods

void SceMiMessageData::Set ( unsigned i, SceMiU32 word, SceMiEC* ec )
{
  char errmsg[256];

  if (i >= bits_to_words(m_pkt->num_bits)) {
    SceMi *scemi = SceMi::Pointer();
    sprintf(errmsg, "index out of bounds on port number %d (%s)", m_pkt->channel,
	    scemi->sceMiMessageInPortProxyName(m_pkt->channel));
    raiseError ("SceMiMessageData::Set", errmsg, SceMiError, ec);
  }
  else
    m_pkt->data[i] = word;
}

void SceMiMessageData::SetBit ( unsigned i, int bit, SceMiEC* ec )
{
  unsigned int nthWord, nthBit;
  SceMiU32 mask;
  char errmsg[256];

  if (i >= m_pkt->num_bits) {
    SceMi *scemi = SceMi::Pointer();
    sprintf(errmsg, "index out of bounds on port number %d (%s)", m_pkt->channel,
	    scemi->sceMiMessageInPortProxyName(m_pkt->channel));
    raiseError ("SceMiMessageData::SetBit", errmsg, SceMiError, ec);
  }

  nthWord = bit_offset_to_word_index(i);
  nthBit = bit_offset_to_word_offset(i);
  mask = 0x00000001 << nthBit;
  if (bit == 0)
    m_pkt->data[nthWord] &= ~mask;
  else
    m_pkt->data[nthWord] |= mask;
}

/*
  For the bit range functions below, the parameter "range" is ONE LESS
  than the number of bits to be set or gotten, according to our
  interpretion of the SceMi specification, which, as of version 2.0
  (draft), never actually specifies what "range" should be, but does
  specify that it is an error if (i+range >= width).  Yes, that is a
  greater-than-or-equals, not just an equals.

  In the e-mail conversation (from: Donald Baltus baltus@bluespec.com
  to: Rene Richter <Rene.Richter@synopsys.com> cc: Heiko Mauersberger
  <Heiko.Mauersberger@synopsys.com>, Michael Posner
  <Michael.Posner@synopsys.com>, Gunnar Scholl
  <Gunnar.Scholl@synopsys.com>, David Park <dpark@synopsys.com>,
  George Harper <gharper@bluespec.com>, Ken Takusagawa
  <ken@bluespec.com>, Charlie Hauck <charlie.hauck@bluespec.com> date:
  Wed, Oct 27, 2010 at 8:22 PM) regarding Synopsys's implementation of
  SceMi, Synopsys seemed to acknowledge the correctness of our
  interpretation, sending us a corrected release of the ChipitPro
  libraries when we pointed out the issue to them (Synopsys had
  initially implemented it incorrectly, at least according to our
  interpretation.)
 */

void SceMiMessageData::SetBitRange(unsigned int i,
				   unsigned int range,
				   SceMiU32 bits,
				   SceMiEC* ec)
{
  char errmsg[256];

  if ((i + range) >= m_pkt->num_bits) {
    SceMi *scemi = SceMi::Pointer();
    sprintf(errmsg, "index out of bounds on port number %d (%s)", m_pkt->channel,
	    scemi->sceMiMessageInPortProxyName(m_pkt->channel));
    raiseError ("SceMiMessageData::SetBitRange", errmsg, SceMiError, ec);
  }
  uncheckedSetBitRange(i,range,bits,m_pkt->data);
}

void SceMiMessageDataInterface::uncheckedSetBitRange(unsigned int i,
                                                     unsigned int range,
                                                     SceMiU32 bits,
                                                     SceMiU32* buffer)
{
  unsigned int nthWordBegin, nthWordEnd, nthBit, numBits;
  SceMiU32 mask;
  nthWordBegin = bit_offset_to_word_index(i);
  nthWordEnd   = bit_offset_to_word_index(i+range);

  nthBit = bit_offset_to_word_offset(i);
  numBits = range+1;
  mask = ((numBits >= 32) ? (~0) : ((1 << numBits) - 1)) << nthBit;
  buffer[nthWordBegin] &= ~mask;
  buffer[nthWordBegin] |= (bits << nthBit);

  if (nthWordEnd > nthWordBegin) {
    numBits -= (32 - nthBit);
    mask = (1 << numBits) - 1;
    buffer[nthWordEnd] &= ~mask;
    buffer[nthWordEnd] |= (bits >> (32-nthBit));
  }
}

SceMiU32 SceMiMessageData::Get(unsigned i, SceMiEC* ec) const
{
  char errmsg[256];

  if (i >= bits_to_words(m_pkt->num_bits)) {
    SceMi *scemi = SceMi::Pointer();
    sprintf(errmsg, "index out of bounds on port number %d (%s)", m_pkt->channel,
	    scemi->sceMiMessageOutPortProxyName(m_pkt->channel));
    raiseError ("SceMiMessageData::Get", errmsg, SceMiError, ec);
    return 0;
  }
  else
    return m_pkt->data[i];
}

int SceMiMessageData::GetBit(unsigned i, SceMiEC* ec) const
{
  unsigned int nthWord, nthBit;
  SceMiU32 data;
  char errmsg[256];

  if (i >= m_pkt->num_bits) {
    SceMi *scemi = SceMi::Pointer();
    sprintf(errmsg, "index out of bounds on port number %d (%s)", m_pkt->channel,
	    scemi->sceMiMessageOutPortProxyName(m_pkt->channel));
    raiseError ("SceMiMessageData::GetBit", errmsg, SceMiError, ec);
  }

  nthWord = bit_offset_to_word_index(i);
  nthBit  = bit_offset_to_word_offset(i);
  data = m_pkt->data[nthWord] >> nthBit;
  return (data & 0x00000001);
}

SceMiU32 SceMiMessageData::GetBitRange(unsigned int i,
				       unsigned int range,
				       SceMiEC* ec) const
{
  char errmsg[256];

  if ((i + range) >= m_pkt->num_bits) {
    SceMi *scemi = SceMi::Pointer();
    sprintf(errmsg, "index out of bounds (%0d+%0d>=%0d) on port number %d (%s)",
	    i, range, m_pkt->num_bits,
	    m_pkt->channel,
	    scemi->sceMiMessageOutPortProxyName(m_pkt->channel));
    raiseError ("SceMiMessageData::GetBitRange", errmsg, SceMiError, ec);
  }
  return uncheckedGetBitRange(i,range,m_pkt->data);
}
SceMiU32 SceMiMessageDataInterface::uncheckedGetBitRange(unsigned int i,
                                                         unsigned int range,
                                                         const SceMiU32 *buffer)
{
  unsigned int nthWordBegin, nthWordEnd, nthBit, numBits;
  UInt32 mask, data;
  nthWordBegin = bit_offset_to_word_index(i);
  nthWordEnd   = bit_offset_to_word_index(i+range);

  nthBit = bit_offset_to_word_offset(i);

  data = buffer[nthWordBegin] >> nthBit;
  if (nthWordEnd > nthWordBegin)
    data |= buffer[nthWordEnd] << (32 - nthBit);

  numBits = range + 1;
  mask = (numBits >= 32) ? (~0) : ((1 << numBits) - 1);

  data &= mask;

  return data;
}

void SceMiMessageData::SetBlockRange(unsigned int i,
					  			     unsigned int range,
									 const char *data)
{
   bool byteAligned = 0 == (i & 0x07);
   bool endAligned = (range != 0) && (0 == ((range+1) & 0x07));
   if (byteAligned && endAligned) {
     char *thisbase = (char *) & m_pkt->data[0];
     unsigned byte0 = i >> 3;
     unsigned byten = (range+1) >> 3;

     memcpy(thisbase + byte0, data, byten);
   }	
}

void SceMiMessageData::GetBlockRange(unsigned int i,
									 unsigned int range,
									 char *data) const
{
  bool byteAligned = 0 == (i & 0x07);
  bool endAligned = (range != 0) && (0 == ((range+1) & 0x07));
  if (byteAligned && endAligned) {
    const char *thisbase = (const char *)&m_pkt->data[0];
    unsigned byte0 = i >> 3;
    unsigned byten = (range+1) >> 3;
    memcpy(data, thisbase + byte0, byten);
  }
}

SceMiU64 SceMiMessageData::CycleStamp () const
{
    return m_cycle;
}

Packet* SceMiMessageData::copyPacket() const
{
  Packet* copy = m_link->packet(m_pkt->num_bits);
  memcpy(copy->data, m_pkt->data, bits_to_bytes(m_pkt->num_bits));
  return copy;
}

Packet* SceMiMessageData::takePacket() const
{
  Packet* taken = m_pkt;
  m_pkt = m_link->packet(taken->num_bits);
  return taken;
}

void SceMiMessageData::print_data() const
{
  if (!m_pkt || !m_pkt->data)
    std::cout << "<EMPTY>";
  else
  {
    unsigned int hex_digits = (m_pkt->num_bits + 3) / 4;
    std::cout << std::dec << m_pkt->num_bits << "<" << std::hex;
    while (hex_digits-- > 0)
    {
      unsigned int word = hex_digits / 8;
      unsigned int shift = 4 * (hex_digits % 8);
      std::cout << ((m_pkt->data[word] >> shift) & 0xF);
      if (shift == 0 && hex_digits != 0)
	std::cout << " ";
    }
    std::cout << ">";
  }
}

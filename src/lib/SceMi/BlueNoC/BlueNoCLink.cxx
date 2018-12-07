/* Copyright (c) 2011 Bluespec, Inc.  All rights reserved. */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <poll.h>

#include "sized_types.h"
#include "BlueNoCLink.h"

#define DEBUG_LINK 0
#define BYTES_PER_BEAT (4)

// Allocate 16 pages (64K bytes) for each buffer.   This is the maximum size which can be send
// to the bluenoc device during each write call.
#define NUM_BUFFER_PAGES 16

// Implementation of SW-side of a BlueNoC-based SCE-MI link layer

static tMsgBuffer* alloc_buf(unsigned int num_pages, bool align, bool straddle)
{
  static unsigned int sz = static_cast<unsigned int>(sysconf(_SC_PAGESIZE));

  unsigned int bufsize = sz * num_pages;
  tMsgBuffer*  buf     = NULL;
  void*        mem_ptr = NULL;

  if (align) {
#ifdef __APPLE__
    // posix_memalign is not available on OS X prior to 10.6
    fprintf(stderr, "Aligned memory allocation is not available on APPLE\n");
#else
    if (posix_memalign(&mem_ptr, sz, bufsize) != 0)
      perror("alloc_buf");
#endif
  } else {
    mem_ptr = malloc(bufsize);
    if (mem_ptr == NULL)
      perror("alloc_buf");
  }

  if (mem_ptr != NULL) {
    buf = (tMsgBuffer*) malloc(sizeof(tMsgBuffer));
    if (buf == NULL) {
      perror("alloc_buf");
      free(mem_ptr);
    } else {
      buf->buffer_ptr  = mem_ptr;
      buf->buffer_size = bufsize;
      buf->data_start  = (char*) mem_ptr;
      if (straddle)
        buf->data_start += sz;
      buf->buffered_bytes = 0;
    }
  }

  return buf;
}

BlueNoCLink::BlueNoCLink( int link_fd
                        , const SceMiParameters* parameters
                        , bool align_buffers
                        , void (*shutdown)(int)
                        )
  : Link("BSC_TRACE_SCEMI")
{
  fd              = link_fd;
  align           = align_buffers;
  bytes_per_beat  = BYTES_PER_BEAT;
  max_msg_size    = 4 + 255;
  if (max_msg_size % bytes_per_beat != 0)
    max_msg_size   += bytes_per_beat - (max_msg_size % bytes_per_beat);
  shutdown_action = shutdown;
  pthread_mutex_init ( &m_write_mutex, NULL );

  if (sigfillset(&block_set) != 0) {
    perror("sigfillset");
  }

  tx_buffer = alloc_buf(NUM_BUFFER_PAGES, align_buffers, false);
  if (align_buffers)
    rx_buffer = alloc_buf(1+NUM_BUFFER_PAGES, true, true);
  else
    rx_buffer = alloc_buf(NUM_BUFFER_PAGES, false, false);

  // initialize the SCE-MI 1.1 state
  last_tx = 0;
  closed = false;
  recv_packet_in_progress = NULL;
  recv_channel_in_progress = 0;
  recv_data_ptr = NULL;
  recv_cyclestamp = 0llu;

  // determine the maximum channel id to setup the pending vector
  unsigned int inportCount = parameters->NumberOfObjects("MessageInPort");
  int maxInportChannelId = 0;
  for (unsigned int i = 0; i < inportCount ; ++i) {
    int chId = parameters->AttributeIntegerValue("MessageInPort", i, "ChannelId");
    maxInportChannelId = std::max(maxInportChannelId, chId);
  }
  pending.resize(1+maxInportChannelId);

  // initialize the SCE-MI 2.1 state

  // determine the maximum pipe number to setup the pipe output buffer vector
  unsigned int inpipeCount = parameters->NumberOfObjects("InputPipe");
  if (inpipeCount != 0) {
    int maxInputPipeNum = 0;
    for (unsigned int i = 0; i < inpipeCount ; ++i) {
      int num = parameters->AttributeIntegerValue("InputPipe", i, "PipeNum");
      maxInputPipeNum = std::max(maxInputPipeNum, num);
    }
    inpipe_output_buffers.resize(1+maxInputPipeNum);
    for (unsigned int i = 0; i < inpipeCount ; ++i) {
      inpipe_output_buffers[i].current_msg_header = NULL;
      inpipe_output_buffers[i].partial_msg_buf = NULL;
    }
  }

  // determine the maximum pipe number to setup the pipe autoflush and credit msg buffers
  unsigned int outpipeCount = parameters->NumberOfObjects("OutputPipe");
  if (outpipeCount != 0) {
    int maxOutputPipeNum = 0;
    for (unsigned int i = 0; i < outpipeCount ; ++i) {
      int num = parameters->AttributeIntegerValue("OutputPipe", i, "PipeNum");
      maxOutputPipeNum = std::max(maxOutputPipeNum, num);
    }
    outpipe_output_buffers.resize(1+maxOutputPipeNum);
    outpipe_autoflush_state.resize(1+maxOutputPipeNum);
    for (unsigned int i = 0; i < outpipeCount ; ++i) {
      outpipe_output_buffers[i] = alloc_buf(NUM_BUFFER_PAGES, align_buffers, false);
      outpipe_autoflush_state[i] = false;
    }
  }

#if DEBUG_LINK
      printf("BlueNoC connection established\n");
    fflush(NULL);
#endif
    if (_logTraffic) {fprintf(_logFile, "BlueNoC connection established\n" ); fflush(_logFile);}
}

// SCE-MI 1.1 support

void BlueNoCLink::queue(Packet* pkt)
{
  if (closed)
  {
    release(pkt);
    return;
  }
  if (pkt->channel >= pending.size()) {
    fprintf(stderr, "incorrect pending size %u > %d\n", pkt->channel, (int) pending.size());
    exit( EXIT_FAILURE);
  }
  if (pending[pkt->channel] == NULL)
  {
    pending[pkt->channel] = pkt;
#if DEBUG_LINK
    printf("Queued packet on channel %d\n", pkt->channel);
    fflush(NULL);
#endif
  }
  else
  {
    fprintf(stderr, "No space to queue packet on channel %d\n", pkt->channel);
    exit(EXIT_FAILURE);
  }
}

void BlueNoCLink::send_ack_packet()
{
  size_t num_channels;
  pthread_mutex_lock ( &m_write_mutex );

  // Determine number of channels to ack
  num_channels = pending_acks.size();

#if DEBUG_LINK
  printf("Sending ack to HW for channels:");
#endif

  std::set<unsigned int>::const_iterator iter = pending_acks.begin();
  char* msg = tx_buffer->data_start;
  while (iter != pending_acks.end()) {
    // construct an ack message
    unsigned int channels_to_ack = std::min((unsigned int)num_channels,127u);
    unsigned int payload_bytes = 2*channels_to_ack;
    if (channels_to_ack % 2 != 0)
      payload_bytes += 2;
    msg[0] = 1;                      /* dst */
    msg[1] = 0;                      /* src */
    msg[2] = static_cast<char>(payload_bytes & 0xff); /* len */
    msg[3] = static_cast<char>(0x2a << 2);            /* SCE-MI 1.1 op */
    for (unsigned int n = 0; n < channels_to_ack; ++n)
    {
      unsigned int channel = *iter++;

      if (_logTraffic) {fprintf (_logFile, "SAP%d\n", channel );fflush(_logFile);}
#if  DEBUG_LINK
      printf(" %d", channel);
#endif

      msg[2*n + 4] = static_cast<char>(channel & 0xff);
      msg[2*n + 5] = static_cast<char>(((channel >> 8) & 0x3) | 0x4);
    }
    if (channels_to_ack % 2 != 0) {
      msg[2 * channels_to_ack + 4] = 0x00;
      msg[2 * channels_to_ack + 5] = 0x00;
    }
    msg[7] |= static_cast<char>(6 << 5); /* 6 => output channel ACK msg */
    num_channels -= channels_to_ack;
    tx_buffer->buffered_bytes += 4 + payload_bytes;
    msg += 4 + payload_bytes;
    // pad to beat boundary
    while (tx_buffer->buffered_bytes % bytes_per_beat != 0) {
      *(msg++) = 0x00;
      tx_buffer->buffered_bytes += 1;
    }
    // if we are nearing the end of the buffer, send the contents
    // and resume at the beginning of the buffer
    if (tx_buffer->buffer_size - tx_buffer->buffered_bytes < max_msg_size) {
      send_buffer(tx_buffer);
      msg = tx_buffer->data_start = (char*) tx_buffer->buffer_ptr;
      tx_buffer->buffered_bytes = 0;
    }
  }

  // if there is message data left in the buffer, send it now
  if (tx_buffer->buffered_bytes != 0) {
    send_buffer(tx_buffer);
    tx_buffer->data_start     = (char*) tx_buffer->buffer_ptr;
    tx_buffer->buffered_bytes = 0;
  }

#if DEBUG_LINK
  printf("\n");
  fflush(NULL);
#endif

  fsync(fd);

  pending_acks.clear();
  pthread_mutex_unlock ( &m_write_mutex );

}

bool BlueNoCLink::ready_to_send(unsigned int channel)
{
  if (closed) return false;
  return (request_rx_slots.find(channel) != request_rx_slots.end());
}

bool BlueNoCLink::send_pkt(unsigned int* channel_ptr)
{
  if (closed) return false;
  pthread_mutex_lock ( &m_write_mutex );

  // Select the next packet to send
  Packet* pkt = NULL;
  for (unsigned int n = 1; n <= pending.size(); ++n)
  {
    size_t ch = (last_tx + n) % pending.size();
    if (pending[ch] == NULL) continue;

    pkt = pending[ch];
    pending[ch] = NULL;
    last_tx = ch;
    break;
  }

  if (pkt == NULL) {
    pthread_mutex_unlock ( &m_write_mutex );
    return false;
  }

  // Mark the request rx slot as empty
  request_rx_slots.erase(pkt->channel);

  if (channel_ptr != NULL)
    *channel_ptr = pkt->channel;

  unsigned int bits_remaining = pkt->num_bits;
  unsigned int bytes_remaining = bits_to_bytes(pkt->num_bits);
  const char* pkt_bytes = (const char*) pkt->data;
  char* msg = tx_buffer->data_start;
  while (bytes_remaining != 0) {
    /* size payload to avoid partial beats in the middle of the data */
    unsigned int bytes_in_msg = ((4 + bytes_remaining) > 255) ? 252 : (4 + bytes_remaining);
    msg[0] = 1;                   /* dst */
    msg[1] = 0;                   /* src */
    msg[2] = static_cast<char>(bytes_in_msg & 0xff); /* len */
    msg[3] = static_cast<char>(0x2a << 2);         /* SCE-MI 1.1 op */
    msg[4] = static_cast<char>(pkt->channel & 0xff);
    msg[5] = static_cast<char>(((pkt->channel >> 8) & 0x3) | ((bits_remaining & 0x3f) << 2));
    msg[6] = static_cast<char>((bits_remaining >> 6) & 0xff);
    if (bytes_remaining == (bytes_in_msg - 4))
      msg[7] = static_cast<char>((1 << 5) | ((bits_remaining >> 14) & 0x1f)); /* 1 => last input channel data */
    else
      msg[7] = static_cast<char>((0 << 5) | ((bits_remaining >> 14) & 0x1f)); /* 0 => input channel data, not complete */
    memcpy(msg + 8, pkt_bytes, bytes_in_msg - 4);
    tx_buffer->buffered_bytes += 4 + bytes_in_msg;
    msg += 4 + bytes_in_msg;
    pkt_bytes += bytes_in_msg - 4;
    bytes_remaining -= (bytes_in_msg - 4);
    bits_remaining -= 8 * (bytes_in_msg - 4);
    // pad to beat boundary
    while (tx_buffer->buffered_bytes % bytes_per_beat != 0) {
      *(msg++) = 0x00;
      tx_buffer->buffered_bytes += 1;
    }
    // if we are nearing the end of the buffer, send the contents
    // and resume at the beginning of the buffer
    if (tx_buffer->buffer_size - tx_buffer->buffered_bytes < max_msg_size) {
      send_buffer(tx_buffer);
      msg = tx_buffer->data_start = (char*) tx_buffer->buffer_ptr;
      tx_buffer->buffered_bytes = 0;
    }
  }

  // if there is message data left in the buffer, send it now
  if (tx_buffer->buffered_bytes != 0) {
    send_buffer(tx_buffer);
    tx_buffer->data_start     = (char*) tx_buffer->buffer_ptr;
    tx_buffer->buffered_bytes = 0;
  }

  if (_logTraffic) report(pkt);

  release(pkt);

  fsync(fd);
  pthread_mutex_unlock ( &m_write_mutex );

  return true;
}

Packet* BlueNoCLink::recv_data_packet(const char* msg, SceMiU64* cycle_ptr)
{
  bool is_complete = ((msg[7] >> 5) & 0x7) == 5;
  unsigned int msg_offset = 8;
  unsigned int payload_bytes = ((unsigned int)(msg[2] & 0xff) - 4);

  if (recv_packet_in_progress == NULL) {
    unsigned int channel = ((unsigned int)msg[4] & 0xff) | (((unsigned int)msg[5] & 0x3) << 8);
    Packet* pkt = new Packet;
    unsigned int bit_count = 0;

    pkt->channel = channel;
    bit_count |= ((unsigned int)msg[5] >> 2) & 0x3f;
    bit_count |= ((unsigned int)msg[6] & 0xff) << 6;
    bit_count |= ((unsigned int)msg[7] & 0x1f) << 14;
    pkt->num_bits = bit_count;
    pkt->data = new SceMiU32[bits_to_words(bit_count)];

    recv_packet_in_progress = pkt;
    recv_channel_in_progress = channel;
    recv_data_ptr = (char*) pkt->data;

    if (payload_bytes < 8) {
      fprintf(stderr, "BlueNoCLink::recv_data_packet(): SCE-MI 1.1 message with short payload\n");
      print_msg(stderr,msg);
      return NULL;
    }

    /* cycle pointer occupies next 8 bytes */
    msg_offset += 8;
    payload_bytes -= 8;

    if (cycle_ptr != NULL) {
      recv_cyclestamp = 0llu;
      recv_cyclestamp |= ((SceMiU64)msg[8]  & 0xffllu);
      recv_cyclestamp |= ((SceMiU64)msg[9]  & 0xffllu) << 8;
      recv_cyclestamp |= ((SceMiU64)msg[10] & 0xffllu) << 16;
      recv_cyclestamp |= ((SceMiU64)msg[11] & 0xffllu) << 24;
      recv_cyclestamp |= ((SceMiU64)msg[12] & 0xffllu) << 32;
      recv_cyclestamp |= ((SceMiU64)msg[13] & 0xffllu) << 40;
      recv_cyclestamp |= ((SceMiU64)msg[14] & 0xffllu) << 48;
      recv_cyclestamp |= ((SceMiU64)msg[15] & 0xffllu) << 56;
      *cycle_ptr = recv_cyclestamp;
    }
  }

  memcpy(recv_data_ptr, msg + msg_offset, payload_bytes);
  if (is_complete)
  {
    Packet* pkt = recv_packet_in_progress;
    unsigned int lastWord = bit_offset_to_word_index(pkt->num_bits);
    unsigned int bitsToKeep = bit_offset_to_word_offset(pkt->num_bits);
    unsigned int mask = (bitsToKeep == 32) ? (~0) : ((1 << bitsToKeep) - 1);
    recv_packet_in_progress = NULL;
    recv_data_ptr           = NULL;

    if (bitsToKeep)
      pkt->data[lastWord] &= mask;

    // record that we need to ack this channel
    pending_acks.insert(pkt->channel);

#if DEBUG_LINK
    printf("Received pkt on channel %d\n", pkt->channel);
    fflush(NULL);
#endif
    if(_logTraffic) report(recv_cyclestamp,pkt);
    return pkt;
  }

  recv_data_ptr += payload_bytes;

  return NULL;
}

void BlueNoCLink::recv_req_packet(const char* msg)
{
  unsigned int channel = ((unsigned int)msg[4] & 0xff) | (((unsigned int)msg[5] & 0x3) << 8);

#if DEBUG_LINK
  printf("Revc Req Packet %d\n", channel);
  fflush(NULL);
#endif
  if (_logTraffic) {fprintf (_logFile, "RRP %d\n", channel);fflush(_logFile);}

  // Mark the rx slot as got a request from HW
  if (request_rx_slots.find(channel) == request_rx_slots.end())
    request_rx_slots.insert(channel);
}

void BlueNoCLink::recv_quit_packet(const char* msg)
{
  closed = true;
}

void BlueNoCLink::recv_pipe_data(const char* msg)
{
  unsigned int pipe_num = (unsigned int)msg[6] & 0xff;
  pipe_num |= ((unsigned int)msg[7] & 0x0f) << 8;

  unsigned int bytes = ((unsigned int)msg[2] & 0xff) - 4;
  const char* data_ptr = msg + 8;

  bool overflow = (msg[7] & 0x40) != 0;
  bool flush    = (msg[7] & 0x20) != 0;
  bool eom      = (msg[7] & 0x10) != 0;

  if (_logTraffic) {
    fprintf(_logFile, "recv_pipe_data(%0d, %0d, %s, %s, %s)\n", pipe_num, bytes, overflow ? "OVERFLOW" : "", flush ? "FLUSH": "", eom ? "EOM" : "");
    fflush(_logFile);
  }

  void* ctx = data_contexts[pipe_num];
  void (*callback)(void*,const char*,unsigned int,bool,bool,bool) = data_callbacks[pipe_num];
  callback(ctx,data_ptr,bytes,overflow,flush,eom);
}

void BlueNoCLink::recv_pipe_credits(const char* msg)
{
  unsigned int pipe_num = (unsigned int)msg[6] & 0xff;
  pipe_num |= ((unsigned int)msg[7] & 0x0f) << 8;

  unsigned int credits = (unsigned int)msg[4] & 0xff;
  credits |= ((unsigned int)msg[5] & 0xff) << 8;

  bool underflow = (msg[7] & 0x40) == 0x40;

  if (_logTraffic) {
    fprintf(_logFile, "recv_pipe_credits(%0d, %0d, %s)\n", pipe_num, credits, underflow ? "UNDERFLOW" : "");
    fflush(_logFile);
  }

  void* ctx = credit_contexts[pipe_num];
  void (*callback)(void*,unsigned int,bool) = credit_callbacks[pipe_num];
  callback(ctx,credits,underflow);
}


// return codes:
// -1 bad packet...
// 0 no data received
// 1 scemi1 packet valid
// 2 other data
int BlueNoCLink::recv_pkt(SceMiU64* cycle_ptr, Packet **scemi1_pkt)
{
  if (closed) return 0;

  // TODO XXX This code should be refactored.
  // All packets should be pulled from bluenoc rather than only 1 for Scemi pipes
  // data should be dispatched to appropriate pipes or ports
  // Acks and credits returned to fpga
  // Notifications processed


  // if we have unacked received packets, ack them now
  if (!pending_acks.empty())
    send_ack_packet();

  int res = is_data_pending();
  if (res <= 0) {
    // no incoming data
    return 0;
  }

  int retcode = 2;
  // read the next message header
  const char* rx_msg_buf = recv_msg();
  if (rx_msg_buf == NULL)
    return 0;

  if (_logTraffic) {
    print_msg(_logFile, rx_msg_buf);
  }


  // dispatch based on the message type
  if (((rx_msg_buf[3] >> 2) & 0x3f) == 0x2a) {
    // This is a SCE-MI 1.1 message
    if (((unsigned int)rx_msg_buf[2] & 0xff) < 4) {
      fprintf(stderr, "BlueNoCLink::recv_pkt(): SCE-MI 1.1 message with short payload\n");
      print_msg(stderr,rx_msg_buf);
      retcode = -1;
    }
    else {
      switch ((rx_msg_buf[7] >> 5) & 0x7) {
        case 2:
          recv_req_packet(rx_msg_buf);
          break;
        case 4:
        case 5:
          *scemi1_pkt = recv_data_packet(rx_msg_buf,cycle_ptr);
          retcode = *scemi1_pkt ? 1 : -1;
          break;
        case 7:
          recv_quit_packet(rx_msg_buf);
          break;
        default:
          fprintf(stderr, "BlueNoCLink::recv_pkt(): unknown SCE-MI 1.1 message type: %0x\n",
                  (rx_msg_buf[7] >> 5) & 0x7);
          print_msg(stderr,rx_msg_buf);
          retcode = -1;
          break;
      }
    }
  }
  else if (((rx_msg_buf[3] >> 2) & 0x3f) == 0x2b) {
    // This is a SCE-MI 2.1 message
    if (((unsigned int)rx_msg_buf[2] & 0xff) < 4) {
      fprintf(stderr, "BlueNoCLink::recv_pkt(): SCE-MI 2.1 message with short payload\n");
      print_msg(stderr, rx_msg_buf);
      retcode = -1;
    }
    else {
      if ((rx_msg_buf[7] & 0x80) == 0x80) {
        recv_pipe_data(rx_msg_buf);
      }
      else {
        recv_pipe_credits(rx_msg_buf);
      }
    }
  }
  else {
    retcode = -1;
    fprintf(stderr, "BlueNoCLink::recv_pkt(): unknown message opcode: %0x\n",
            (rx_msg_buf[3] >> 2) & 0x3f);
    print_msg(stderr, rx_msg_buf);
    if (_logTraffic) {
      fprintf(_logFile, "BlueNoCLink::recv_pkt(): unknown message opcode: %0x\n",
              (rx_msg_buf[3] >> 2) & 0x3f);
      print_msg(_logFile, rx_msg_buf);
    }
  }

  return retcode;
}


void BlueNoCLink::debug (const char *msg)
{
  if (_logTraffic) {
    fprintf(_logFile, "BlueNoCLink::debug(): %s\n", msg );
  }
}

Packet* BlueNoCLink::packet(unsigned int len)
{
  Packet* pkt = new Packet;
  pkt->num_bits = len;
  pkt->data = new SceMiU32[bits_to_words(len)];
  return pkt;
}

void BlueNoCLink::release(Packet* pkt)
{
  delete[] pkt->data;
  delete pkt;
}

void BlueNoCLink::loop()
{
  static int cnt = 0;
  cnt++;
  if (cnt > 1000000) {
    // report a heart beat
    if (_logTraffic) {
      fprintf(_logFile,"BlueNoCLink::loop() %d\n", cnt );
      fflush(_logFile);
    }
    cnt = 0;
  }
  // nothing needed here
}

// SCE-MI 2.1 pipes support

void BlueNoCLink::set_data_handler( unsigned int pipe_num,
                                    void (*callback)(void*,const char*,unsigned int,bool,bool,bool),
                                    void* context
                                  )
{
  if (pipe_num >= data_callbacks.size()) {
    data_callbacks.resize(pipe_num + 1);
    data_contexts.resize(pipe_num + 1);
  }
  data_callbacks[pipe_num] = callback;
  data_contexts[pipe_num] = context;
}

void BlueNoCLink::set_credit_handler( unsigned int pipe_num,
                                      void (*callback)(void*,unsigned int,bool),
                                      void* context
                                    )
{
  if (pipe_num >= credit_callbacks.size()) {
    credit_callbacks.resize(pipe_num + 1);
    credit_contexts.resize(pipe_num + 1);
  }
  credit_callbacks[pipe_num] = callback;
  credit_contexts[pipe_num] = context;
}

void BlueNoCLink::send_credits(unsigned int pipe_num, unsigned int amount, bool underflow)
{
  pthread_mutex_lock ( &m_write_mutex );
  // outpipe output buffers will always be used for a single message a time
  tMsgBuffer* tx_buf = outpipe_output_buffers[pipe_num];
  char* msg = tx_buf->data_start;
  bool autoflush = outpipe_autoflush_state[pipe_num];

  if (_logTraffic) {
    fprintf(_logFile, "send_credits(%0d, %0d, %s)\n", pipe_num, amount, underflow ? "UNDERFLOW" : "");
    fflush(_logFile);
  }

  msg[0] = 1;                        /* dst */
  msg[1] = 0;                        /* src */
  msg[2] = 4;                        /* len */
  msg[3] = static_cast<char>(0x2b << 2);              /* SCE-MI 2.1 op */
  msg[4] = static_cast<char>(amount & 0xff);          /* low byte of credit amount */
  msg[5] = static_cast<char>((amount >> 8) & 0xff);   /* high byte of credit amount */
  msg[6] = static_cast<char>(pipe_num & 0xff);        /* low byte of pipe number */
  msg[7] = static_cast<char>(((pipe_num >> 8) & 0xf)     /* high 4 bits of pipe number */
                             | (autoflush ? 0x20 : 0x00) /* autoflush setting */
                             | (underflow ? 0x40 : 0x00) /* underflow indicator */
                             );
  tx_buf->buffered_bytes = 8;
  send_buffer(tx_buf);
  tx_buf->buffered_bytes = 0;
  pthread_mutex_unlock ( &m_write_mutex );
}

void BlueNoCLink::send_autoflush(unsigned int pipe_num, bool enable)
{
  pthread_mutex_lock ( &m_write_mutex );
  // outpipe output buffers will always be used for a single message a time
  tMsgBuffer* tx_buf = outpipe_output_buffers[pipe_num];
  char* msg = tx_buf->data_start;
  outpipe_autoflush_state[pipe_num] = enable;

  if (_logTraffic) {
    fprintf(_logFile, "send_autoflush(%0d, %s)\n", pipe_num, enable ? "ENABLE" : "DISABLE");
    fflush(_logFile);
  }

  msg[0] = 1;                        /* dst */
  msg[1] = 0;                        /* src */
  msg[2] = 4;                        /* len */
  msg[3] = static_cast<char>(0x2b << 2);              /* SCE-MI 2.1 op */
  msg[4] = 0;                        /* low byte of credit amount */
  msg[5] = 0;                        /* high byte of credit amount */
  msg[6] = static_cast<char>(pipe_num & 0xff);         /* low byte of pipe number */
  msg[7] = static_cast<char>(((pipe_num >> 8) & 0xf)   /* high 4 bits of pipe number */
                             | (enable ? 0x20 : 0x00)  /* autoflush setting */
                             );
  tx_buf->buffered_bytes = 8;
  send_buffer(tx_buf);
  tx_buf->buffered_bytes = 0;
  pthread_mutex_unlock ( &m_write_mutex );

}

/* HOW THIS WORKS:
 *
 * We want to be able to accumulate data into buffers and then send
 * them all across the NoC link at once. The data may be too big to
 * fit into a single message, so it will need to be split up. We don't
 * want the caller to have to know the details of the message format
 * and size restrictions, so we handle that for them.
 *
 * There are two routines: add_message_data and send_data.  The
 * add_message_data routine is used to add data to the buffer,
 * handling all of the details of creating message headers, splitting
 * the data across messages, etc. The send_data routine closes off any
 * message that was in progress and then sends all of the buffered
 * messages across the NoC.
 *
 * Each pipe has a tTxBuffer element in the inpipe_output_buffers
 * vector. This structure keeps track of the state of the output
 * buffers for that pipe, which includes a current partially-full
 * buffer, a sequence of previously filled but not-yet-sent buffers,
 * and pointer the start of a partially completed message in the
 * current partially-full buffer.
 *
 * Initially, the full_msg_bufs list is empty and there is no
 * partial_msg_buf. As data is added to the buffer, a partial_msg_buf
 * is allocated when needed, the current_msg_header is kept pointing
 * to the current message under construction and is NULL when there is
 * no incomplete message in the buffer, and buffers are moved from
 * partial_msg_buf to the full_msg_bufs list as they fill up.
 *
 * Each buffer is a tMsgBuffer object which contains:
 *  buffer_ptr     = pointer to buffer memory area
 *  buffer_size    = number of contiguous bytes in the memory area
 *  data_start     = pointer to first byte of message data,
 *                   which could be different from buffer_ptr
 *  buffered_bytes = count of data bytes currently in the buffer
 *
 * For transmit buffers, the buffer_size should always be one page
 * and the data_start will be initialized to buffer_ptr. Receive buffers
 * use tMsgBuffers differently.
 */

typedef enum { GET_BUFFER, ADD_HEADER, COPY_PAYLOAD, ADD_EOM } tAction;

void BlueNoCLink::add_message_data( unsigned int pipe_num, unsigned int elem_bytes
                                  , unsigned int len, const unsigned char* data
                                    , unsigned int byte_offset, bool eom
                                  )
{
  tMsgBuffer*  buf               = NULL;
  char*        msg               = NULL;
  unsigned int src_bytes_copied  = 0;
  unsigned int bytes_remaining   = len * elem_bytes;
  unsigned int elems_reported    = 0;

  unsigned int space_left_in_msg = 0;
  tAction      next_action       = GET_BUFFER;


  pthread_mutex_lock ( &m_write_mutex );
  tTxBuffers&  tx_bufs           = inpipe_output_buffers[pipe_num];

  while (bytes_remaining != 0) {

    switch (next_action) {
      case GET_BUFFER:
      {
        /* Try to get a buffer, which could be an existing partially
         * full one
         */
        buf = get_inpipe_output_buffer(tx_bufs);
        if (NULL == buf) {
          fprintf (stderr, "Error: failed to allocate an output buffer\n");
          pthread_mutex_unlock ( &m_write_mutex );
          return;
        }
        /* fall through to ADD_HEADER */
      }
      case ADD_HEADER:
      {
        /* Is there enough space in this buffer for a whole message?
         * If not, then close it out and start a new buffer.
         */
        if ((buf->buffered_bytes + 256) > buf->buffer_size) {
          inpipe_output_buffer_is_full(tx_bufs);
          next_action = GET_BUFFER;
          break;
        }
        /* There is space in this buffer, see if we need to add
         * a message header.
         */
        msg = tx_bufs.current_msg_header;
        if (NULL == msg) {
          msg = buf->data_start + buf->buffered_bytes;
          msg[0] = 1;                      /* dst */
          msg[1] = 0;                      /* src */
          msg[2] = 4;                      /* len */
          msg[3] = static_cast<char>(0x2b << 2);            /* SCE-MI 2.1 op */
          msg[4] = 0;                      /* low bits of element count */
          msg[5] = 0;                      /* high bits of element count */
          msg[6] = static_cast<char>(pipe_num & 0xff);        /* low bits of pipe number */
          msg[7] = static_cast<char>(((pipe_num >> 8) & 0xf)  /* high bits of pipe number */
                                     | 0x80                   /* data msg */
                                     );
          tx_bufs.current_msg_header = msg;
          buf->buffered_bytes += 8;
        }
        space_left_in_msg = 252 - ((unsigned int)msg[2] & 0xff);
        /* otherwise fall through to COPY_PAYLOAD */
      }
      case COPY_PAYLOAD:
      {
        /* determine how many bits to copy into the buffer */
        unsigned int bytes_to_copy = std::min (space_left_in_msg, bytes_remaining);
        unsigned int msg_len = (unsigned int) msg[2] & 0xff;

        unsigned int src_byte_idx   = (src_bytes_copied + byte_offset);
        memcpy (msg + 4 + msg_len, data + src_byte_idx, bytes_to_copy);

        src_bytes_copied     += bytes_to_copy;
        msg[2]               =  static_cast<char>(msg[2] + bytes_to_copy);
        buf->buffered_bytes +=  bytes_to_copy;;
        space_left_in_msg   -=  bytes_to_copy;
        bytes_remaining     -= bytes_to_copy;

        // Add the message count to the header
        unsigned int elems_this_payload = (src_bytes_copied / elem_bytes) - elems_reported;
        elems_reported +=  elems_this_payload;

        unsigned cur_payload = ((unsigned) (msg[4]) &0xff) | (((unsigned) msg[5] & 0xff) << 8);
        cur_payload += elems_this_payload;

        msg[4] = static_cast<char>(cur_payload & 0xff) ;
        msg[5] = static_cast<char>((cur_payload >> 8) & 0xff) ;;
        next_action = ADD_HEADER;

        /* if this filled the message, then we have to close off
         * this message and start a new one.
         */
        if (space_left_in_msg == 0 && bytes_remaining != 0) {
          tx_bufs.current_msg_header = NULL;
          next_action = ADD_HEADER;
          break;
        }
        /* otherwise fall through to ADD_EOM */
      }
      case ADD_EOM:
      {
        assert (bytes_remaining == 0);
        if (eom) {
          msg[7] |= 0x10;
          // Close out this buffer   an EOM is expensive!
          inpipe_output_buffer_is_full(tx_bufs);
        }
        break;
      }
      default:
        {
          fprintf (stderr, "Error: unknown action when constructing message (%x)\n", next_action);
          pthread_mutex_unlock ( &m_write_mutex );
          return;
        }
    }

  } /* end: while (bytes_remaining != 0) */
  pthread_mutex_unlock ( &m_write_mutex );

}

void BlueNoCLink::send_data(unsigned int pipe_num, bool overflow, bool flush)
{
  pthread_mutex_lock ( &m_write_mutex );

  tTxBuffers& tx_bufs = inpipe_output_buffers[pipe_num];

  if (overflow || flush) {
    tMsgBuffer* buf = get_inpipe_output_buffer(tx_bufs);

    // if there is no data message in progress, create an empty one
    char* msg = tx_bufs.current_msg_header;
    if (msg == NULL) {
      msg = buf->data_start + buf->buffered_bytes;
      msg[0] = 1;                      /* dst */
      msg[1] = 0;                      /* src */
      msg[2] = 4;                      /* len */
      msg[3] = static_cast<char>(0x2b << 2);            /* SCE-MI 2.1 op */
      msg[4] = 0;                      /* low bits of element count */
      msg[5] = 0;                      /* high bits of element count */
      msg[6] = static_cast<char>(pipe_num & 0xff);        /* low bits of pipe number */
      msg[7] = static_cast<char>(((pipe_num >> 8) & 0xf)  /* high bits of pipe number */
                                 | 0x80                   /* data msg */
                                 );
      buf->buffered_bytes += 8;
    }

    // set the overflow indicator in the SCE-MI header
    if (overflow)
      msg[7] |= 0x40;
    // set the flush indicator in the SCE-MI header
    if (flush)
      msg[7] |= 0x20;
  }

  // send out all messages in this pipe's buffers
  if (_logTraffic) { fprintf(_logFile,"send_data(%d,%d,%d)\n", pipe_num, overflow, flush); fflush (_logFile); }

  if (tx_bufs.partial_msg_buf != NULL) {
    tMsgBuffer* msg_buf = tx_bufs.partial_msg_buf;
    if ((msg_buf->buffered_bytes % bytes_per_beat) != 0) {
      msg_buf->buffered_bytes += (bytes_per_beat - (msg_buf->buffered_bytes % bytes_per_beat));
      if (tx_bufs.current_msg_header != NULL) {
        unsigned int msg_len = tx_bufs.current_msg_header[2] & 0xff;
        while ((msg_len % bytes_per_beat) != 0) {
          tx_bufs.current_msg_header[4 + msg_len] = 0;
          ++msg_len;
        }
      }
    }
    tx_bufs.full_msg_bufs.push_back(msg_buf);
    tx_bufs.partial_msg_buf = NULL;
    tx_bufs.current_msg_header = NULL;
  }

  while (!tx_bufs.full_msg_bufs.empty()) {
    tMsgBuffer* msg_buf = tx_bufs.full_msg_bufs.front();
    tx_bufs.full_msg_bufs.pop_front();
    send_buffer(msg_buf);
    release_inpipe_output_buffer(msg_buf);
  }
  pthread_mutex_unlock ( &m_write_mutex );
}

void BlueNoCLink::send_buffer(tMsgBuffer* buf)
{
  if (buf == NULL) return;

  sigset_t old_set;

  char* src_ptr = buf->data_start;
  unsigned int to_send = buf->buffered_bytes;
  ssize_t res = 0;
  assert(buf->buffered_bytes <= buf->buffer_size);
  //assert(src_ptr + to_send <= (char*)(buf->buffer_ptr) + buf->buffer_size);

  while (1) {
    if (pthread_sigmask(SIG_SETMASK, &block_set, &old_set) != 0) {
      perror("pthread_sigmask");
    }
    res = write(fd, src_ptr, to_send);
    if (pthread_sigmask(SIG_SETMASK, &old_set, NULL) != 0) {
      perror("pthread_sigmask");
    }
    if (res >= 0) {
      to_send -= static_cast<unsigned int>(res);
      if (to_send == 0)
        break;
      if (align) {
        // we need to realign the data before sending the next batch!
        memmove(src_ptr, src_ptr + res, to_send);
      } else {
        src_ptr += res;
      }
    }
    if (errno == EBUSY) {
      sleep(0);
      continue;
    }
    if (errno != EINTR) {
      perror("send_buffer");
      break;
    }
  }

  if (_logTraffic) {
    reportNoC( buf);
  }
}

int BlueNoCLink::is_data_pending()
{
  if (rx_buffer->buffered_bytes > 3) {
    unsigned int msg_len = rx_buffer->data_start[2] & 0xff;
    unsigned int total_bytes = 4 + msg_len;
    if (total_bytes % bytes_per_beat != 0)
      total_bytes += bytes_per_beat - (total_bytes % bytes_per_beat);
    if (rx_buffer->buffered_bytes >= total_bytes)
      return 1;
  }

  struct pollfd pfd;
  int n;

  while (1) {
    pfd.fd = fd;
    pfd.events = POLLIN;
    pfd.revents = 0;

    n = poll(&pfd, 1, 0);
    if ((n < 0) && (errno != EINTR))
    {
#ifdef __APPLE__
      char err[1024];
      if (strerror_r(errno, err, 1024) != 0)
        strcpy(err, "Unknown error");
#else
      char buf[1024];
      char* err = strerror_r(errno, buf, 1024);
#endif
      fprintf (stderr, "Error: poll failed on device %0d\n%s\n", fd, err);
      return -1;
    }
    else if (n >= 0)
      return n;
  }
  return -1; /* control should never reach this statement */
}

const char* BlueNoCLink::recv_msg()
{
  static unsigned int sz = static_cast<unsigned int>(sysconf(_SC_PAGESIZE));

  // check for the existence of a complete message
  if (rx_buffer->buffered_bytes > 3) {
    unsigned int msg_len = rx_buffer->data_start[2] & 0xff;
    unsigned int total_bytes = 4 + msg_len;
    if (total_bytes % bytes_per_beat != 0)
      total_bytes += bytes_per_beat - (total_bytes % bytes_per_beat);
    if (rx_buffer->buffered_bytes >= total_bytes) {
      // there is a complete message in the buffer, so return it
      const char* msg = rx_buffer->data_start;
      rx_buffer->data_start += total_bytes;
      rx_buffer->buffered_bytes -= total_bytes;
      if (rx_buffer->buffered_bytes == 0) {
        // buffer is empty, reset it
        rx_buffer->data_start  = (char*) rx_buffer->buffer_ptr;
        if (align)
          rx_buffer->data_start += sz;
      }
      return msg;
    }
  }

  // If we get here, then we know there is data to be read from the
  // file descriptor, because is_data_pending() returned true and
  // there was not a complete message in the buffer. We want to read
  // that data into the buffer, but we may need to make space and/or
  // realign the existing data before we can read any more in.

  if (rx_buffer->buffered_bytes != 0) {
    std::ptrdiff_t empty_at_start = rx_buffer->data_start - ((char*) rx_buffer->buffer_ptr);
    unsigned int space_in_buffer = static_cast<unsigned int>(rx_buffer->buffer_size - rx_buffer->buffered_bytes - empty_at_start);
    if (align || (space_in_buffer < max_msg_size)) {
      char* dst = (char*) rx_buffer->buffer_ptr;
      if (align)
        dst += sz - rx_buffer->buffered_bytes;
      memmove(dst, rx_buffer->data_start, rx_buffer->buffered_bytes);
      rx_buffer->data_start = dst;
    }
  }

  sigset_t old_set;

  // attempt to read more message data into the buffer
  std::ptrdiff_t empty_at_start = rx_buffer->data_start - ((char*) rx_buffer->buffer_ptr);
  unsigned int space_in_buffer = static_cast<unsigned int>(rx_buffer->buffer_size - rx_buffer->buffered_bytes - empty_at_start);

  if (pthread_sigmask(SIG_SETMASK, &block_set, &old_set) != 0) {
    perror("pthread_sigmask");
  }
  ssize_t res = read(fd, rx_buffer->data_start + rx_buffer->buffered_bytes, space_in_buffer);
  if (pthread_sigmask(SIG_SETMASK, &old_set, NULL) != 0) {
    perror("pthread_sigmask");
  }
  if (res > 0) {
    rx_buffer->buffered_bytes += static_cast<unsigned int>(res);
    // check for the existence of a complete message now
    if (rx_buffer->buffered_bytes > 3) {
      unsigned int msg_len = rx_buffer->data_start[2] & 0xff;
      unsigned int total_bytes = 4 + msg_len;
      if (total_bytes % bytes_per_beat != 0)
        total_bytes += bytes_per_beat - (total_bytes % bytes_per_beat);
      if (rx_buffer->buffered_bytes >= total_bytes) {
        // there is a complete message in the buffer, so return it
        const char* msg = rx_buffer->data_start;
        rx_buffer->data_start += total_bytes;
        rx_buffer->buffered_bytes -= total_bytes;
        if (rx_buffer->buffered_bytes == 0) {
          // buffer is empty, reset it
          rx_buffer->data_start  = (char*) rx_buffer->buffer_ptr;
          if (align)
            rx_buffer->data_start += sz;
        }
        return msg;
      }
    }
  }

  // If control reaches here, we still don't have a complete message.
  return NULL;
}

void BlueNoCLink::print_msg(FILE *dfp, const char* msg)
{
  unsigned int dst       = ((unsigned int)(msg[0])) & 0xff;
  unsigned int src       = ((unsigned int)(msg[1])) & 0xff;
  unsigned int len       = ((unsigned int)(msg[2])) & 0xff;
  unsigned int dont_wait = msg[3] & 0x1;
  unsigned int opcode    = (msg[3] >> 2) & 0x3f;
  fprintf(dfp,"dst: %d src: %d len: %d opcode: %x%s", dst, src, len, opcode, dont_wait ? " DW\n" : "\n");

  char msgbuf[64*10] ;
  int idx = 0;
  for (unsigned int i = 0; i < (len+4); i+=4) {
    unsigned int x0 = (0x0FF) & (unsigned int) msg[i+0];
    unsigned int x1 = (0x0FF) & (unsigned int) msg[i+1];
    unsigned int x2 = (0x0FF) & (unsigned int) msg[i+2];
    unsigned int x3 = (0x0FF) & (unsigned int) msg[i+3];
    idx += sprintf ( & msgbuf[idx], "%02x%02x%02x%02x ", x3,x2,x1,x0);
  }
  msgbuf[idx] = '\0';
  fprintf(dfp,"%s\n", msgbuf );
  fflush(dfp);
}

BlueNoCLink::~BlueNoCLink()
{
  if (!closed)
    closed = true;

  if (shutdown_action)
    shutdown_action(fd);

  for (std::vector<tTxBuffers>::iterator iter = inpipe_output_buffers.begin();
       iter != inpipe_output_buffers.end();
       ++iter)
  {
    if (iter->partial_msg_buf != NULL)
      release_inpipe_output_buffer(iter->partial_msg_buf);
    while (!iter->full_msg_bufs.empty()) {
      release_inpipe_output_buffer(iter->full_msg_bufs.front());
      iter->full_msg_bufs.pop_front();
    }
  }
  while (!free_buffer_pool.empty()) {
    tMsgBuffer* buf = free_buffer_pool.front();
    if (buf != NULL) {
      if (buf->buffer_ptr != NULL)
        free(buf->buffer_ptr);
      free(buf);
    }
    free_buffer_pool.pop_front();
  }
}

tMsgBuffer* BlueNoCLink::get_inpipe_output_buffer(tTxBuffers& tx_bufs)
{
  // if there is a buffer already in place, then return it
  if (tx_bufs.partial_msg_buf != NULL)
    return tx_bufs.partial_msg_buf;

  // otherwise, try to take a buffer from the global free buffer pool
  tMsgBuffer* buf = NULL;
  if (!free_buffer_pool.empty()) {
    buf = free_buffer_pool.front();
    free_buffer_pool.pop_front();
  } else {
    // the pool is empty so we must allocate a new buffer
    buf = alloc_buf(NUM_BUFFER_PAGES, align, false);
    if (buf == NULL)
      return NULL;
  }

  buf->data_start     = (char*) buf->buffer_ptr;
  buf->buffered_bytes = 0;

  tx_bufs.partial_msg_buf = buf;

  return buf;
}

void BlueNoCLink::inpipe_output_buffer_is_full(tTxBuffers& tx_bufs)
{
  tMsgBuffer* buf = tx_bufs.partial_msg_buf;

  if (NULL == buf) return;

  // pad buffered data to a beat boundary
  while ((buf->buffered_bytes % bytes_per_beat) != 0) {
    buf->data_start[buf->buffered_bytes] = 0;
    buf->buffered_bytes += 1;
  }

  // move the active buffer to the full list
  tx_bufs.full_msg_bufs.push_back(buf);
  tx_bufs.partial_msg_buf = NULL;
  tx_bufs.current_msg_header = NULL;
}

void BlueNoCLink::release_inpipe_output_buffer(tMsgBuffer* buf)
{
  // return the buffer to the global free buffer pool
  if (buf != NULL)
    free_buffer_pool.push_front(buf);
}


void BlueNoCLink::reportNoC (const tMsgBuffer* buf)
{
  fprintf(_logFile, "send_buffer %0d bytes: ", buf->buffered_bytes);
  for (unsigned int i = 0 ; i < buf->buffered_bytes ; i += 4) {
    unsigned int x0 = (0x0FF) & (unsigned int) buf->data_start[i+0];
    unsigned int x1 = (0x0FF) & (unsigned int) buf->data_start[i+1];
    unsigned int x2 = (0x0FF) & (unsigned int) buf->data_start[i+2];
    unsigned int x3 = (0x0FF) & (unsigned int) buf->data_start[i+3];
    fprintf (_logFile, "%02x%02x%02x%02x ",
             x3,x2,x1,x0);
  }
  fprintf(_logFile,"\n");
  fflush(_logFile);

}

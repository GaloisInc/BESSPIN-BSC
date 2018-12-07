#include "core_link.h"

static msg_buffer_t *alloc_buf(uint32_t num_pages, bool align, bool straddle)
{
  static uint32_t sz = static_cast<uint32_t>(sysconf(_SC_PAGESIZE));

  uint32_t bufsize = sz * num_pages;
  msg_buffer_t *buf = NULL;
  void * mem_ptr = NULL;

  if (align) {
#ifdef __APPLE__
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
    buf = (msg_buffer_t *)malloc(sizeof(msg_buffer_t));
    if (buf == NULL) {
      perror("alloc_buf");
      free(mem_ptr);
    } else {
      buf->buffer_ptr  = mem_ptr;
      buf->buffer_size = bufsize;
      buf->data_start  = (char*)mem_ptr;
      if (straddle)
	buf->data_start += sz;
      buf->buffered_bytes = 0;
    }
  }

  return buf;
}

core_link::core_link( int link_fd, const bluenoc_parameters *parameters, bool align_buffers, void (*shutdown)(int))
  : bluenoc_link("BSC_TRACE_BLUENOC")
  , closed(false)
{
  fd              = link_fd;
  align           = align_buffers;
  bytes_per_beat  = BYTES_PER_BEAT;
  max_msg_size    = 4 + 255;
  if (max_msg_size % bytes_per_beat != 0)
    max_msg_size += bytes_per_beat - (max_msg_size % bytes_per_beat);
  shutdown_action = shutdown;
  pthread_mutex_init (&m_write_mutex, NULL);

  tx_buffer = alloc_buf(NUM_BUFFER_PAGES, align_buffers, false);
  if (align_buffers)
    rx_buffer = alloc_buf(1+NUM_BUFFER_PAGES, true, true);
  else
    rx_buffer = alloc_buf(NUM_BUFFER_PAGES, false, false);

  // determine the maximum pipe number to setup the pipe output buffer vector
  uint32_t inpipeCount = parameters->NumberOfObjects("InPipe");
  if (inpipeCount != 0) {
    int maxInputPipeNum = 0;
    for (unsigned int i = 0; i < inpipeCount; ++i) {
      int num = parameters->AttributeIntegerValue("InPipe", i, "PipeNum");
      maxInputPipeNum = std::max(maxInputPipeNum, num);
    }
    inpipe_output_buffers.resize(1+maxInputPipeNum);
    for(uint32_t i = 0; i < inpipeCount; ++i) {
      int num = parameters->AttributeIntegerValue("InPipe", i, "PipeNum");
      inpipe_output_buffers[num].current_msg_header = NULL;
      inpipe_output_buffers[num].partial_msg_buf = NULL;
    }
  }

  // determine the maximum pipe number to setup the pipe autoflush and credit msg buffers
  unsigned int outpipeCount = parameters->NumberOfObjects("OutPipe");
  if (outpipeCount != 0) {
    int maxOutputPipeNum = 0;
    for (uint32_t i = 0; i < outpipeCount ; ++i) {
      int num = parameters->AttributeIntegerValue("OutPipe", i, "PipeNum");
      maxOutputPipeNum = std::max(maxOutputPipeNum, num);
    }
    outpipe_output_buffers.resize(1+maxOutputPipeNum);
    outpipe_autoflush_state.resize(1+maxOutputPipeNum);
    for (uint32_t i = 0; i < outpipeCount; ++i) {
      int num = parameters->AttributeIntegerValue("OutPipe", i, "PipeNum");
      outpipe_output_buffers[num] = alloc_buf(NUM_BUFFER_PAGES, align_buffers, false);
      outpipe_autoflush_state[num] = false;
    }
  }

#if DEBUG_LINK
  printf("BlueNoC connection established\n");
  fflush(NULL);
#endif
  if (m_logTraffic) {
    fprintf(m_logFile, "BlueNoC connection established\n");
    fflush(m_logFile);
  }
}

void core_link::recv_pipe_data(const char *msg)
{
  uint32_t nodeid = (uint32_t)msg[1] & 0xFF;
  uint32_t bytes = ((uint32_t)msg[2] & 0xFF);
  const char *data_ptr = msg + 4;
  
  bool overflow = false;
  bool flush = false;
  bool eom = false;

  if (m_logTraffic) {
    fprintf(m_logFile, "recv_pipe_data(%0d, %0d, %s, %s, %s)\n", nodeid, bytes, "", "", "");
    fflush(m_logFile);
  }

  void *ctx = data_contexts[nodeid];
  void (*callback)(void*, const char*, uint32_t, bool, bool, bool) = data_callbacks[nodeid];
  callback(ctx, data_ptr, bytes, overflow, flush, eom);
}

void core_link::recv_pipe_credits(const char *msg)
{
  uint32_t nodeid = (uint32_t)msg[1] & 0xFF;
  uint32_t credits = (uint32_t)msg[4] & 0xFF;
  credits |= ((uint32_t)msg[5] & 0xFF) << 8;

  bool underflow = (msg[7] & 0x80) == 0x80;

  if (m_logTraffic) {
    fprintf(m_logFile, "recv_pipe_credits(%0d, %0d, %s)\n", nodeid, credits, underflow ? "UNDERFLOW" : "");
    fflush(m_logFile);
  }

  void *ctx = credit_contexts[nodeid];
  void (*callback)(void*,uint32_t,bool) = credit_callbacks[nodeid];
  callback(ctx, credits, underflow);
}

// return codes:
// -1 bad packet...
// 0 no data received
// 1 packet valid
// 2 other data
int core_link::recv_pkt(uint64_t *cycle_ptr, Packet **pptr)
{
  if (closed) return 0;

  // TODO XXX This code should be refactored.
  // All packets should be pulled from bluenoc rather than only 1 for Scemi pipes
  // data should be dispatched to appropriate pipes or ports
  // Acks and credits returned to fpga
  // Notifications processed

  int res = is_data_pending();
  if (res <= 0) {
    // no incoming data
    return 0;
  }

  int retcode = 2;
  // read the next message header
  const char *rx_msg_buf = recv_msg();
  if (rx_msg_buf == NULL)
    return 0;

  if (m_logTraffic) {
    print_msg(m_logFile, rx_msg_buf);
  }

  if (((rx_msg_buf[3] >> 2) & 0x3F) == BN_DATA_OPCODE) {
    // This Is A Data Packet
    recv_pipe_data(rx_msg_buf);
  } 
  else if (((rx_msg_buf[3] >> 2) & 0x3F) == BN_CREDIT_OPCODE) {
    // This is a credit packet
    recv_pipe_credits(rx_msg_buf);
  }
  else {
    retcode = -1;
    fprintf(stderr, "core_link::recv_pkt() unknown message opcode %0x\n", (rx_msg_buf[3] >> 2) & 0x3F);
    print_msg(stderr, rx_msg_buf);
    if (m_logTraffic) {
      fprintf(m_logFile, "core_link::recv_pkt() unknown message opcode %0x\n", (rx_msg_buf[3] >> 2) & 0x3F);
      print_msg(m_logFile, rx_msg_buf);
    }
  }
  return retcode;
}

void core_link::debug(const char *msg)
{
  if (m_logTraffic) {
    fprintf(m_logFile, "core_link::debug(): %s\n", msg);
  }
}

void core_link::set_data_handler(uint32_t node,
				 void (*callback)(void *, const char *, uint32_t, bool, bool, bool),
				 void *context)
{
  if (node >= data_callbacks.size()) {
    data_callbacks.resize(node + 1);
    data_contexts.resize(node + 1);
  }
  data_callbacks[node] = callback;
  data_contexts[node] = context;
}

void core_link::set_credit_handler ( uint32_t node,
				     void (*callback)(void *, uint32_t, bool),
				     void *context)
{
  if (node >= credit_callbacks.size()) {
    credit_callbacks.resize(node + 1);
    credit_contexts.resize(node + 1);
  }
  credit_callbacks[node] = callback;
  credit_contexts[node] = context;
}

void core_link::send_credits( uint32_t node, uint16_t amount, bool underflow)
{
  pthread_mutex_lock(&m_write_mutex);

  msg_buffer_t *tx_buf = outpipe_output_buffers[node];
  char *msg = tx_buf->data_start;
  bool autoflush = outpipe_autoflush_state[node];

  if (m_logTraffic) {
    fprintf(m_logFile, "send_credits(%0d, %0d, %s)\n", node, amount, underflow ? "UNDERFLOW" : "");
    fflush(m_logFile);
  }

  msg[0] = static_cast<char>(node & 0xFF);     /* dst */
  msg[1] = 0;                                  /* src */
  msg[2] = 4;                                  /* len */
  msg[3] = static_cast<char>(BN_CREDIT_OPCODE << 2); /* Credit opcode */
  msg[4] = static_cast<char>(amount & 0xFF);   /* low byte of credit amount */
  msg[5] = static_cast<char>((amount >> 8) & 0xFF); /* high byte of credit amount */
  msg[6] = 0;                                  /* unused bits */
  msg[7] = static_cast<char>(
			     (autoflush ? 0x40 : 0x00)  /* autoflush setting */
			   | (underflow ? 0x80 : 0x00)  /* underflow indicator */
			     );

  tx_buf->buffered_bytes = 8;
  send_buffer(tx_buf);
  tx_buf->buffered_bytes = 0;
  pthread_mutex_unlock(&m_write_mutex);
}

void core_link::send_autoflush(uint32_t node, bool enable)
{
  pthread_mutex_lock(&m_write_mutex);

  msg_buffer_t *tx_buf = outpipe_output_buffers[node];
  char *msg = tx_buf->data_start;
  outpipe_autoflush_state[node] = enable;

  if (m_logTraffic) {
    fprintf(m_logFile, "send_autoflush(%0d, %s)\n", node, enable ? "ENABLE" : "DISABLE");
    fflush(m_logFile);
  }

  msg[0] = static_cast<char>(node & 0xFF);     /* dst */
  msg[1] = 0;                                  /* src */
  msg[2] = 4;                                  /* len */
  msg[3] = static_cast<char>(BN_CREDIT_OPCODE << 2); /* Credit opcode */
  msg[4] = 0;                                  /* low byte of credit amount */
  msg[5] = 0;                                  /* high byte of credit amount */
  msg[6] = 0;                                  /* unused bits */
  msg[7] = static_cast<char>(enable ? 0x40 : 0x00);  /* autoflush setting */

  tx_buf->buffered_bytes = 8;
  send_buffer(tx_buf);
  tx_buf->buffered_bytes = 0;
  pthread_mutex_unlock(&m_write_mutex); 
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
 * Each buffer is a msg_buffer_t object which contains:
 *  buffer_ptr     = pointer to buffer memory area
 *  buffer_size    = number of contiguous bytes in the memory area
 *  data_start     = pointer to first byte of message data,
 *                   which could be different from buffer_ptr
 *  buffered_bytes = count of data bytes currently in the buffer
 *
 * For transmit buffers, the buffer_size should always be one page
 * and the data_start will be initialized to buffer_ptr. Receive buffers
 * use msg_buffer_ts differently.
 */

typedef enum { GET_BUFFER, ADD_HEADER, COPY_PAYLOAD, ADD_EOM } action_t;

void core_link::add_message_data(uint32_t node, uint32_t elem_bytes, uint32_t len, 
				 const uint8_t *data, uint32_t byte_offset, bool eom)
{
  msg_buffer_t *buf = NULL;
  char *msg = NULL;
  uint32_t src_bytes_copied = 0;
  uint32_t bytes_remaining = len * elem_bytes;
  uint32_t elems_reported = 0;

  uint32_t space_left_in_msg = 0;
  action_t next_action = GET_BUFFER;

  pthread_mutex_lock(&m_write_mutex);
  tx_buffers_t &tx_bufs = inpipe_output_buffers[node];

  while(bytes_remaining != 0) {
  
    switch(next_action) {
      case GET_BUFFER:
	{
        /* Try to get a buffer, which could be an existing partially
         * full one
         */
	  buf = get_inpipe_output_buffer(tx_bufs);
	  if (buf == NULL) {
	    fprintf(stderr, "Error: failed to allocate an output buffer\n");
	    pthread_mutex_unlock(&m_write_mutex);
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
	  if (msg == NULL) {
	    msg = buf->data_start + buf->buffered_bytes;
	    msg[0] = static_cast<char>(node & 0xFF);  /* dst */
	    msg[1] = 0;                               /* src */
	    msg[2] = 0;                               /* len */
	    msg[3] = static_cast<char>(BN_DATA_OPCODE << 2); /* Data op */
	    
	    tx_bufs.current_msg_header = msg;
	    buf->buffered_bytes += 4;
	  }
	  space_left_in_msg = 252 - ((unsigned int)msg[2] & 0xFF);
	  /* otherwise fall through to COPY_PAYLOAD */
	}
      case COPY_PAYLOAD:
	{
	  /* determine how many bits to copy into the buffer */
	  uint32_t bytes_to_copy = std::min(space_left_in_msg, bytes_remaining);
	  uint32_t msg_len = (uint32_t)msg[2] & 0xFF;

	  uint32_t src_byte_idx = (src_bytes_copied + byte_offset);
	  memcpy(msg + 4 + msg_len, data + src_byte_idx, bytes_to_copy);

	  src_bytes_copied += bytes_to_copy;
	  msg[2] = static_cast<char>(msg[2] + bytes_to_copy);
	  buf->buffered_bytes += bytes_to_copy;
	  space_left_in_msg -= bytes_to_copy;
	  bytes_remaining -= bytes_to_copy;

	  // Add the message count to the header
	  uint32_t elems_this_payload = (src_bytes_copied/elem_bytes) - elems_reported;
	  elems_reported += elems_this_payload;

	  // encode number of elements in packet?
	  //unsigned cur_payload = ((unsigned) (msg[4]) &0xff) | (((unsigned) msg[5] & 0xff) << 8);
	  //cur_payload += elems_this_payload;

	  //msg[4] = static_cast<char>(cur_payload & 0xff) ;
	  //msg[5] = static_cast<char>((cur_payload >> 8) & 0xff) ;;

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
	  assert(bytes_remaining == 0);
	  if (eom) {
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
  }
  pthread_mutex_unlock(&m_write_mutex);
}

void core_link::send_data( uint32_t node, bool overflow, bool flush)
{
  pthread_mutex_lock ( &m_write_mutex );

  tx_buffers_t& tx_bufs = inpipe_output_buffers[node];

  if (overflow || flush) {
    msg_buffer_t* buf = get_inpipe_output_buffer(tx_bufs);

    // if there is no data message in progress, create an empty one
    char* msg = tx_bufs.current_msg_header;
    if (msg == NULL) {
      msg = buf->data_start + buf->buffered_bytes;
      msg[0] = static_cast<char>(node & 0xFF); /* dst */
      msg[1] = 0;                              /* src */
      msg[2] = 0;                              /* len */
      msg[3] = static_cast<char>(BN_DATA_OPCODE << 2);
      buf->buffered_bytes += 4;
    }
  }

  // send out all messages in this pipe's buffers
  if (m_logTraffic) { fprintf(m_logFile,"send_data(%d,%d,%d)\n", node, overflow, flush); fflush (m_logFile); }

  if (tx_bufs.partial_msg_buf != NULL) {
    msg_buffer_t* msg_buf = tx_bufs.partial_msg_buf;
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
    msg_buffer_t* msg_buf = tx_bufs.full_msg_bufs.front();
    tx_bufs.full_msg_bufs.pop_front();
    send_buffer(msg_buf);
    release_inpipe_output_buffer(msg_buf);
  }
  pthread_mutex_unlock ( &m_write_mutex );
}

void core_link::send_buffer(msg_buffer_t* buf)
{
  if (buf == NULL) return;

  char* src_ptr = buf->data_start;
  unsigned int to_send = buf->buffered_bytes;
  ssize_t res = 0;
  assert(buf->buffered_bytes <= buf->buffer_size);
  while (1) {
    res = write(fd, src_ptr, to_send);
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

  if (m_logTraffic) {
    reportNoC( buf);
  }
}

int core_link::is_data_pending()
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

const char* core_link::recv_msg()
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

  // attempt to read more message data into the buffer
  std::ptrdiff_t empty_at_start = rx_buffer->data_start - ((char*) rx_buffer->buffer_ptr);
  unsigned int space_in_buffer = static_cast<unsigned int>(rx_buffer->buffer_size - rx_buffer->buffered_bytes - empty_at_start);
  ssize_t res = read(fd, rx_buffer->data_start + rx_buffer->buffered_bytes, space_in_buffer);
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

void core_link::print_msg(FILE *dfp, const char* msg)
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

core_link::~core_link()
{
  if (!closed)
    closed = true;

  if (shutdown_action)
    shutdown_action(fd);

  for (std::vector<tx_buffers_t>::iterator iter = inpipe_output_buffers.begin();
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
    msg_buffer_t* buf = free_buffer_pool.front();
    if (buf != NULL) {
      if (buf->buffer_ptr != NULL)
        free(buf->buffer_ptr);
      free(buf);
    }
    free_buffer_pool.pop_front();
  }
}

msg_buffer_t* core_link::get_inpipe_output_buffer(tx_buffers_t& tx_bufs)
{
  // if there is a buffer already in place, then return it
  if (tx_bufs.partial_msg_buf != NULL)
    return tx_bufs.partial_msg_buf;

  // otherwise, try to take a buffer from the global free buffer pool
  msg_buffer_t* buf = NULL;
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

void core_link::inpipe_output_buffer_is_full(tx_buffers_t& tx_bufs)
{
  msg_buffer_t* buf = tx_bufs.partial_msg_buf;

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

void core_link::release_inpipe_output_buffer(msg_buffer_t* buf)
{
  // return the buffer to the global free buffer pool
  if (buf != NULL)
    free_buffer_pool.push_front(buf);
}

void core_link::reportNoC (const msg_buffer_t *buf)
{
  fprintf(m_logFile, "send_buffer %0d bytes: ", buf->buffered_bytes);
  for (unsigned int i = 0 ; i < buf->buffered_bytes ; i += 4) {
    unsigned int x0 = (0x0FF) & (unsigned int) buf->data_start[i+0];
    unsigned int x1 = (0x0FF) & (unsigned int) buf->data_start[i+1];
    unsigned int x2 = (0x0FF) & (unsigned int) buf->data_start[i+2];
    unsigned int x3 = (0x0FF) & (unsigned int) buf->data_start[i+3];
    fprintf (m_logFile, "%02x%02x%02x%02x ",
             x3,x2,x1,x0);
  }
  fprintf(m_logFile,"\n");
  fflush(m_logFile);
}

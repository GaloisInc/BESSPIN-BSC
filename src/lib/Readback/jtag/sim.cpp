#include "sim.h"

sim::sim()
  : m_fd(-1)
  , m_port(9000)
  , m_addr("127.0.0.1")
{
  description = string("Simulation JTAG Cable");
}

sim::~sim()
{
  close();
}

int
sim::open()
{
  struct addrinfo hints;
  struct addrinfo *result, *rp;
  int  s;
  char service[12];
  int socket_fd;

  /* Obtain address(es) matching host/port */

  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;  /* TCP/IP socket */
  hints.ai_flags = 0;
  hints.ai_protocol = 0;          /* Any protocol */

  sprintf(service, "%d", m_port);
  s = getaddrinfo(m_addr.c_str(), service, &hints, &result);
  if (s != 0) {
    fprintf(stderr, "Error: getaddrinfo() failed: %s\n", gai_strerror(s));
    return -1;
  }

  /* getaddrinfo() returns a list of address structures.
     Try each address until we successfully connect(2).
     If socket(2) (or connect(2)) fails, we (close the socket
     and) try the next address. */

  for (rp = result; rp != NULL; rp = rp->ai_next) {
    socket_fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
    if (socket_fd == -1)
      continue;

    if (connect(socket_fd, rp->ai_addr, rp->ai_addrlen) != -1)
      break;                  /* Success */

    ::close(socket_fd);
  }

  if (rp == NULL) {               /* No address succeeded */
    fprintf(stderr, "Error: connect() failed\n");
    return -1;
  }

  freeaddrinfo(result);           /* No longer needed */

  // Set NODELAY socket option to disable Nagle algorithm
  int one = 1;
  s = setsockopt(socket_fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));

  m_fd = socket_fd;
  return 0;
}

int
sim::close()
{
  int res;
  char buf[128];

  /* shut down the socket for writing */
  shutdown(m_fd, SHUT_WR);

  /* then, read data from the socket until it returns 0,
   * indicating all data from the other side has been
   * received.
   */
  while (1) {
    res = recv(m_fd, buf, 128, 0);
    if (res == 0)
      break;
    if ((res == -1) && (errno != EINTR))
      break;
  }

  /* finally, close the socket */
  ::close(m_fd);
  m_fd = -1;

  return 0;
}

bool
sim::is_present()
{
  char buffer[2048];
  buffer[0] = 0;

  FILE *fp = popen("netstat --all | grep 9000", "r");
  if (fp == NULL) {
    return false;
  }

  while(fgets(buffer, sizeof(buffer)-1, fp) != NULL);

  pclose(fp);

  return (buffer[0] != 0);
}

void
sim::txrx_block(const uint8_t *in, uint8_t *out, int len, bool last)
{
  char pbuf[1024], *ptr = pbuf;
  int i;

  logfile.Debug(MSG_DEBUG10, "---");
  logfile.Debug(MSG_DEBUG10, "transfer size %d, %s output", len, (out != NULL) ? "with" : "without");
  for(i = 0; i < len; i++) {
    sprintf(ptr++, "%c", (in) ? ((in[i>>3] & (1<<i%8)) ? '1' : '0'): '0');
  }
  sprintf(ptr++, "%s", (last) ? "last" : "");
  logfile.Debug(MSG_DEBUG10, "tdi: %s", pbuf);

  uint8_t *buffer = new uint8_t[len * 3];
  if (buffer == NULL) {
    throw string("failed to allocate buffer!");
  }
  
  // assemble the data, toggling the TCK signal
  int j = 0;
  for(i = 0; i < len; i++) {
    // TCK = 0
    buffer[j] = (in[i>>3] & 1<<(i%8)) ? 1 : 0;
    if (last && (i == len-1)) buffer[j] |= 0x04;
    j++;
    // TCK = 1
    buffer[j] = (in[i>>3] & 1<<(i%8)) ? 1 : 0;
    buffer[j] |= 0x02;
    if (last && (i == len-1)) buffer[j] |= 0x04;
    j++;
    // TCK = 0, sample TDO
    buffer[j] = (in[i>>3] & 1<<(i%8)) ? 1 : 0;
    if (out != NULL) buffer[j] |= 0x40;
    if (last && (i == len-1)) buffer[j] |= 0x04;
    j++;
  }
  
  send_msg(buffer, j);
  memset(buffer, 0, len*3);

  // get back TDO
  if (out != NULL) {
    ptr = pbuf;
    j = recv_msg(buffer, len);
    if (j != len) {
      logfile.Debug(MSG_DEBUG10, "%d",j);
      perror("recv_msg");
      delete[] buffer;
      throw string("recv_msg failed!");
    }

    for(i = 0; i < len; i++) {
      sprintf(ptr++, "%c", (buffer[i] & 0x01) ? '1' : '0');
      if ((i%8) == 0) {
	out[i>>3] = buffer[i] & 0x01;
      } else {
	out[i>>3] |= (buffer[i] & 0x01) << (i%8);
      }
    }
    logfile.Debug(MSG_DEBUG10, "tdo: %s", pbuf);
  }
  delete[] buffer;
}

void
sim::tx_tms(uint8_t *in, int len, int force)
{
  // FORMAT is on a NIBBLE BASIS  <tdo>,<tck>,<tms>,<tdi>.  The tdo
  // bit is not used and is always zero.

  uint8_t *buffer = new uint8_t[len * 3];
  if (buffer == NULL) {
    throw string("failed to allocate buffer!");
  }
  
  // assemble the data, toggling the TCK signal
  int j = 0;
  for(int i = 0; i < len; i++) {
    // TCK=0
    buffer[j++] = (in[i>>3] & 1<<(i%8)) ? 4 : 0;
    // TCK=1
    buffer[j] = (in[i>>3] & 1<<(i%8)) ? 4 : 0;
    buffer[j++] |= 0x2;
    // TCK=0
    buffer[j++] = (in[i>>3] & 1<<(i%8)) ? 4 : 0;
  }

  // j is now the send length.
  char pbuf[1024], *ptr = pbuf;

  logfile.Debug(MSG_DEBUG10, "---");
  logfile.Debug(MSG_DEBUG10, "transfer size %d", len);
  for(int i = 0; i < len; i++) {
    sprintf(ptr++, "%c", (in[i>>3] & 1<<(i%8)) ? '1' : '0');
  }
  logfile.Debug(MSG_DEBUG10, "TMS: %s", pbuf);

  send_msg(buffer, j);
  delete[] buffer;
}

void
sim::send_msg(const uint8_t *buf, int len)
{
  if (buf == NULL) {
    fprintf(stderr, "send_msg: buffer pointer is NULL\n");
    return;
  }

  ssize_t res = 0;
  while(1) {
    res = send(m_fd, buf, len, MSG_NOSIGNAL);
    if (res >= 0)
      break;
    if (errno != EINTR) {
      perror("send_msg");
      break;
    }
  }
}

int
sim::recv_msg(uint8_t *buf, int len)
{
  if (buf == NULL) {
    fprintf(stderr, "recv_msg: buffer pointer is NULL\n");
    return -2;
  }

  ssize_t res;
  ssize_t pos = 0;
  while(1) {
    res = recv(m_fd, buf + pos, len - pos, MSG_WAITALL);
    if (res == (len - pos)) {
      return len;
    } else if (res > 0) {
      pos += res;
    } else if ((res < 0) && (errno != EINTR)) {
      return -1;
    }
  }

  return -3;
}

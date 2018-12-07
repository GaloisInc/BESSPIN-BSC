#include <cstring>
#include <cstdlib>
#include <pthread.h>
#include <ttcl.h>
#include <string>
#include <sstream>
#include <unistd.h>
#include <sys/time.h>
#include "scemi.h"

class InterpPassData {
  public:
  unsigned int       m_port;
  unsigned long long m_long_out;
  unsigned long long m_long_in;
  unsigned long long m_long_recv;
  unsigned long long m_scemi;

 InterpPassData ()
    : m_port(0)
    , m_long_out(0)
    , m_long_in(0)
    , m_long_recv(0)
    , m_scemi(0)
  {}
  ~InterpPassData () {}
};

class InterpGlobalData {
  public:
  bool			m_building ;
  bool			m_initialized ;
  InterpPassData*       m_data;
  Tcl_Interp*           m_interp;
  double                m_real_start;
  clock_t               m_cpu_start;

  // Simple initializer invoked when the extension is loaded
  InterpGlobalData ()
    : m_building(false)
    , m_initialized(false)
    , m_data(NULL)
    , m_interp(NULL)
  {
    m_data = new InterpPassData();

    struct timeval  tv;
    gettimeofday(&tv, NULL);

    m_real_start = (double) tv.tv_usec/1000000 + tv.tv_sec;
    m_cpu_start  = clock();
  }

  ~InterpGlobalData ()
  {
    if (m_initialized) {
      destroy();
    }
  }

  // Destruction -- called from bsdebug::scemi delete
  void destroy () {
    m_building    = false ;
    m_initialized = false ;

  }

} InterpGlobal;


extern "C" {

  void* startInterp(void* vv) {

    InterpPassData* pd = (InterpPassData*) vv;
    unsigned int number = pd->m_port;

    // unsigned long long *v = (unsigned long long*) vv;

    // unsigned int number = (int) *v;

    InterpGlobalData *pglobal = & InterpGlobal;

    Tcl_Interp* theInterp = NULL;

    theInterp = TTcl_CreateInterp();
    pglobal->m_interp = theInterp;

    int r = 0;

    r = TTcl_Eval(theInterp, "set ::tcl_library $::env(BLUESPECDIR)/tcllib/tcl8.5");
    if (r != TCL_OK) {
      const char* msg = TTcl_GetStringResult(theInterp);
      fprintf(stderr, "ERROR: %s\n", msg);
      exit(1);
    }
    
    r = TTcl_Init(theInterp);
    if (r != TCL_OK) {
      const char* msg = TTcl_GetStringResult(theInterp);
      fprintf(stderr, "ERROR: %s\n", msg);
      exit(1);
    }

    r = TTcl_Eval(theInterp, "lappend auto_path $::env(BLUESPECDIR)/tcllib/simtb");
    if (r != TCL_OK) {
      const char* msg = TTcl_GetStringResult(theInterp);
      fprintf(stderr, "ERROR: %s\n", msg);
      exit(1);
    }

    r = TTcl_Eval(theInterp, "package require boot");
    if (r != TCL_OK) {
      const char* msg = TTcl_GetStringResult(theInterp);
      fprintf(stderr, "ERROR: %s\n", msg);
      exit(1);
    }

    std::string cmd = "::sim::set_comm_port ";
    std::stringstream ss;
    ss << number;
    //    *v = 0;
    cmd.append(ss.str());
    r = TTcl_Eval(theInterp, cmd.c_str());
    if (r != TCL_OK) {
      const char* msg = TTcl_GetStringResult(theInterp);
      fprintf(stderr, "ERROR: (attempt to use port %d) %s\n", number, msg);
      exit(1);
    } 

// else {
//       cmd = "puts stdout \"Info: TCP socket for tcl interaction is: ";
//       cmd.append(ss.str());
//       cmd.append("\"");
//       r = TTcl_Eval(theInterp, cmd.c_str());
//     }

    TTcl_LinkVar(theInterp, "channelOut", (char *) &pd->m_long_out, TCL_LINK_WIDE_INT);
    TTcl_LinkVar(theInterp, "channelIn",  (char *) &pd->m_long_in,  TCL_LINK_WIDE_INT);
    TTcl_LinkVar(theInterp, "channelRecv",  (char *) &pd->m_long_recv,  TCL_LINK_WIDE_INT);
    TTcl_LinkVar(theInterp, "scemiVar",   (char *) &pd->m_scemi,    TCL_LINK_WIDE_INT);

    // this needs to be last
    r = TTcl_Eval(theInterp, "vwait forever");
    if (r != TCL_OK) {
      const char* msg = TTcl_GetStringResult(theInterp);
      fprintf(stderr, "ERROR: %s\n", msg);
      exit(1);
    }
    return 0;
  }

  // unsigned int
  // bsvsimtb_interp_message_ready(unsigned int channel)
  // {

  //   InterpGlobalData *pglobal = & InterpGlobal;
  //   unsigned long long current = pglobal->m_data->m_long_out >> 32;
  //   current--;
  //   if (current == channel) {
  //     fprintf(stderr, "Message ready!\n");
  //     return 1;
  //   }

  //   return 0;
  // }

  unsigned long long
  bsvsimtb_interp_message_get(unsigned int channel)
  {

    // Default result is "tagged Invalid"
    unsigned long long invalid = 0;

    InterpGlobalData *pglobal = & InterpGlobal;
    unsigned long long current = pglobal->m_data->m_long_out >> 32;
    if (current == (channel + 1)) {
      unsigned int value = (int) pglobal->m_data->m_long_out;
      pglobal->m_data->m_long_out = 0;
      return (0x100000000ull | (unsigned long long)value);
    }

    return invalid;
  }

  unsigned int
  bsvsimtb_interp_message_send(unsigned int channel, unsigned int value)
  {

    // Default result is "tagged Invalid"
    unsigned int invalid = 0;
    unsigned int   valid = 1;

    InterpGlobalData *pglobal = & InterpGlobal;

    // wait for channel in to be available
    while ((pglobal->m_data->m_long_in >> 32) != 0) {
      usleep(1000);
    }

    unsigned long long current = (unsigned long long) (channel + 1) << 32;
    current = current | (unsigned long long) value;
    pglobal->m_data->m_long_in = current;

    TTcl_UpdateLinkedVar(pglobal->m_interp, "channelIn");

    unsigned int count = 0;
    // wait for someone to grab the data
    while (pglobal->m_data->m_long_recv == 0) {
      usleep(100);
      count++;
      if (count == 2000) {
	break;
      }
    }

    if (count == 2000) {
      fprintf(stderr, "Error: Message %d on channel %d dropped!\n", value, channel);
      return invalid;
    }

    pglobal->m_data->m_long_in   = 0;
    pglobal->m_data->m_long_recv = 0;
    TTcl_UpdateLinkedVar(pglobal->m_interp, "channelIn");
    TTcl_UpdateLinkedVar(pglobal->m_interp, "channelRecv");

    return valid;
  }

  unsigned int
  bsvsimtb_interp_start(unsigned int port)
  {

    InterpGlobalData *pglobal = & InterpGlobal;

    if (pglobal->m_building) {
      while (!pglobal->m_initialized) {
	sleep(1);
      }
      return (0x00000001u);
    }

    pglobal->m_building = true;

    fprintf(stdout, "Info: Starting tcl interpreter.\n");
    fprintf(stdout, "Info: TCP socket for tcl interaction is: %d\n", port);

    InterpPassData*    p_data  = pglobal->m_data;
    p_data->m_port = port;

    pthread_t thread_id=0;

    pthread_create(&thread_id,0,&startInterp, (void*) p_data);
    pthread_detach(thread_id);

    fprintf(stdout, "Info: Initializing tcl interpreter ...\n");

    while (bsvsimtb_interp_message_get(123456) == 0) {
      sleep(1);
    }

    fprintf(stdout, "Info: Initializing tcl interpreter complete.\n");

    SceMiEC* ec = NULL;
    SceMi *scemi = SceMi::Pointer(ec);

    if (p_data->m_scemi == 0) {
      fprintf(stdout, "Warning: No SceMi object was created during Interpreter initialization.\n");
    } else {
      SceMi* scemi_interp = (SceMi*) p_data->m_scemi;
      if (scemi == NULL && p_data->m_scemi != 0) {
	fprintf(stdout, "Info: Synchronizing simulation SceMi object with that of the tcl interpreter.\n");
	SceMi::SetPointer(scemi_interp, ec);
      }
      // fprintf(stderr, "SCEMI POINTER: %llx\n", p_data->m_scemi);
      // fprintf(stderr, "SCEMI CURRENT: %llx\n", (unsigned long long) scemi);
    }

    pglobal->m_initialized = true;

    // return valid
    return (0x00000001u);
  }

  void emu_stop()
  {
    bsvsimtb_interp_message_send(7033, 0);
  }

  void timer_clear()
  {

    InterpGlobalData *pglobal = & InterpGlobal;
    struct timeval  tv;
    gettimeofday(&tv, NULL);

    pglobal->m_real_start = (double) tv.tv_usec/1000000 + tv.tv_sec;
    pglobal->m_cpu_start  = clock();

  }

  void timer_show()
  {

    InterpGlobalData *pglobal = & InterpGlobal;
    struct timeval  tv;
    gettimeofday(&tv, NULL);

    double  now = (double) tv.tv_usec/1000000 + tv.tv_sec;
    clock_t cpu = clock() - pglobal->m_cpu_start;


    fprintf(stdout, "Elapsed real time is %f seconds.\n", now -  pglobal->m_real_start);
    fprintf(stdout, "Elapsed CPU  time is %f seconds.\n",((float)cpu)/CLOCKS_PER_SEC);

  }
}


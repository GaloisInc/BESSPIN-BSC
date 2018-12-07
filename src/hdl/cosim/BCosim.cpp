
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include <string>
#include <bitset>

#include "vpi_user.h"

#include "BCosim.h"
#include "VHandle.h"
#include "BCModule.h"
#include "BCPort.h"
#include "GenModule.h"
#include "VValue.h"
#include "BCCallBack.h"
#include "DataQueue.h"


using namespace std;

// Static DATA member.
BCosim *BCosim::s_singleton = 0;
queue<int> BCosim::_data_in;
PathMap* BCosim::_map = 0;
long long BCosim::_time_current = 0;
long long BCosim::_half = 5;
bool BCosim::m_first = true;
bool BCosim::_cycle_data = false;
bool BCosim::_first_data = false;
Simulator BCosim::_sim   = UNKNOWN;

BCosim::BCosim()
{
  m_inChan   =  0;
  m_outChan  = 0;
  map_file   = "";
  top_module = "";

}

// Static function
// Creates a BCosim object and registers call be for later initialization
PLI_INT32 BCosim::initialize ()
{

  PLI_INT32 ret ;
  vpi_printf ((char *) "Initializing Bluespec cosimulation.\n");
  // Create global object
  if (0 != s_singleton ) {
    fprintf (stderr, "BComsim class already exists.\n");
    exit (1);
  }
  s_singleton = new BCosim;
  ret =s_singleton->registerStartOfSimCallBack();
  //
  return ret;
}

// Scan the argument list and open the channels for communication
PLI_INT32 BCosim::openChannels()
{
  PLI_INT32 ret = 0;
  s_vpi_vlog_info vli;
  char sep;
  char buf[1024] ;
  char inbuf[1032];
  char outbuf[1032] ;
  bool found = false;

  if (0 == vpi_get_vlog_info (&vli) ) {
    fprintf (stderr, "Error: Could not access verilog info.\n" );
    exit(1);
  }

  size_t pos;

  std::string sim_name = vli.product;

  pos = sim_name.find("ModelSim");
  if (pos!=string::npos) {
    _sim = MODELSIM;
  }

  pos = sim_name.find("CVC");
  if (pos!=string::npos) {
    _sim = CVC;
  }

  pos = sim_name.find("VCS");
  if (pos!=string::npos) {
//    _sim = VCS;
    _sim = MODELSIM;
  }

  for (int i = 0 ; i < vli.argc && ! found ; i ++ ) {
    const char *arg = vli.argv[i] ;

    if (2 == sscanf (arg, "+cosim_data%c%s", &sep, (char *) &buf )) {
      if (! (sep == '='  || sep == '+') ) {
        fprintf (stderr, "bad seperator in cosim_data argument: %s\n", arg);
        exit (1);
      }
      found = true;

      strncpy (inbuf, buf, 1032);
      m_inChan = fopen (inbuf, "r" );
      if (0 == m_inChan) {
        fprintf (stderr, "Error: could not open %s\n%s\n", inbuf, strerror(errno));
        exit (1);
      }
      strcat (strncpy (outbuf, buf, 1032),  ".outchan" );
//      m_outChan = fopen (outbuf, "w" );
      m_outChan = stdout;
      if (0 == m_outChan) {
        fprintf (stderr, "Error: could not open %s\n%s\n", outbuf, strerror(errno));
        exit (1);
      }
      vpi_printf ((char *) "Using cosimulation data channel: '%s'.\n", buf );
    }
  }
  if (! found ) {
    fprintf (stderr, "Error: No +cosim_data+ option found.\n");
    ret = 1;
  }
  found = false;
  for (int i = 0 ; i < vli.argc && ! found ; i ++ ) {
    const char *arg = vli.argv[i] ;

    if (2 == sscanf (arg, "+cosim_map%c%s", &sep, (char *) &buf )) {
      if (! (sep == '='  || sep == '+') ) {
        fprintf (stderr, "bad separator in cosim_map argument: %s\n", arg);
        exit (1);
      }
      found = true;

      strncpy (inbuf, buf, 1032);
//      strcat (strncpy (inbuf, buf, 1032),  ".inchan" );
      map_file = inbuf;
      vpi_printf ((char *) "Using cosimulation map file: '%s'.\n\n", buf );
    }
  }
  if (! found ) {
    fprintf (stderr, "Error: No +cosim_map+ option found.\n");
    ret = 1;
  }
  found = false;
  for (int i = 0 ; i < vli.argc && ! found ; i ++ ) {
    const char *arg = vli.argv[i] ;

    if (2 == sscanf (arg, "+cosim_top%c%s", &sep, (char *) &buf )) {
      if (! (sep == '='  || sep == '+') ) {
        fprintf (stderr, "bad separator in cosim_top argument: %s\n", arg);
        exit (1);
      }
      found = true;

      strncpy (inbuf, buf, 1032);
//      strcat (strncpy (inbuf, buf, 1032),  ".inchan" );
      top_module = inbuf;
      vpi_printf ((char *) "Using top module: '%s'.\n\n", buf );
    }
  }
  if (! found ) {
    fprintf (stderr, "Error: No +cosim_top+ option found.\n");
    ret = 1;
  }
  found = false;
  ignore_eof = 0;
  for (int i = 0 ; i < vli.argc && ! found ; i ++ ) {
    const char *arg = vli.argv[i] ;

    if (2 == sscanf (arg, "+ignore_eof%c%s", &sep, (char *) &buf )) {
      if (! (sep == '='  || sep == '+') ) {
        fprintf (stderr, "bad separator in cosim_top argument: %s\n", arg);
        exit (1);
      }
      found = true;

      strncpy (inbuf, buf, 1032);
      ignore_eof = atoi(inbuf);
      if (ignore_eof)
	vpi_printf ((char *) "Ignoring EOF.\n\n", buf );
    }
  }
  return ret;
}

// Called on simulator startup
PLI_INT32 BCosim::registerStartOfSimCallBack()
{
  // Register start of simulation call back
  s_cb_data  cb ;
  cb.reason = cbStartOfSimulation ;
  cb.cb_rtn = generalCallBack ;
  cb.obj = 0;
  cb.time = 0;
  cb.value = 0;
  cb.index = 0;
  cb.user_data = (PLI_BYTE8 *) this ;
  if (0 == vpi_register_cb (&cb )) {
    fprintf(stderr, "Error: BCosim failed to register startOfSimulation call back.\n");
    return 1;
  }
  return 0;
}
// Call back to command loop after a given delay.
PLI_INT32 BCosim::registerDelayCallBack(PLI_UINT32 delay)
{

  if (delay == 0) {
    delay = 1;
  }
  PLI_INT32 ret = 0;
  static s_vpi_time t;
  t.type = vpiSimTime;
  t.high = 0;
  t.low = delay;

  s_cb_data  cb ;
  cb.reason = cbAfterDelay ;
  cb.cb_rtn = generalCallBack ;
  cb.obj = 0;
  cb.time = &t;
  cb.value = 0;
  cb.index = 0;
  cb.user_data = (PLI_BYTE8 *) this ;
  if (0 == vpi_register_cb (&cb )) {
    fprintf(stderr, "Error: Failed to register cbAfterDelay call back.\n");
    ret = 1;
  }
  return ret;
}

// Call back to command loop after a given delay.
PLI_INT32 BCosim::registerReadWriteSynchCallBack(PLI_UINT32 delay)
{

  if (delay == 0) {
    delay = 1;
  }
  PLI_INT32 ret = 0;
  static s_vpi_time t;
  t.type = vpiSimTime;
  t.high = 0;
  t.low = delay;

  s_cb_data  cb ;
  cb.reason = cbReadWriteSynch;
  cb.cb_rtn = generalCallBack ;
  cb.obj = 0;
  cb.time = &t;
  cb.value = 0;
  cb.index = 0;
  cb.user_data = (PLI_BYTE8 *) this ;
  if (0 == vpi_register_cb (&cb )) {
    fprintf(stderr, "Error: Failed to register cbAfterDelay call back.\n");
    ret = 1;
  }
  return ret;
}

// Single entry point for call back
PLI_INT32 BCosim::generalCallBack (s_cb_data *pcb)
{
  PLI_INT32 ret = 0;
  switch (pcb->reason) 
    {
    case cbStartOfSimulation: {
      BCosim *pbc = (BCosim *) pcb->user_data ;
      ret = pbc->openChannels() || pbc->scanDesign();
      ret = ret || pbc->processCommands();
      //	     pbc->registerDelayCallBack(0) 
    } break;
    case cbValueChange: {
      BCCallBack *mycb = (BCCallBack *) pcb->user_data ;
      BCosim *pbc = mycb->getCosim();
      ret = pbc->valueChangeCallBack (pcb, mycb);
    }  break;
    case cbAfterDelay: {
      if (m_first) {
	m_first = false;
	vpiHandle topiter, topiref;

	/* build the iterator for each module */
	topiter = vpi_iterate(vpiModule, NULL);
	for (;;)
	  {
	    if ((topiref = vpi_scan(topiter)) == NULL) break;
	    //	    VHandle h = topiref;
	    //	    GenModule::initializeAllStateValues(h);
	  }
      }
      BCosim *pbc = (BCosim *) pcb->user_data ;
      ret = pbc->processCommands();
    } break;
    case cbReadWriteSynch : {
      printf("ZOW!\n");
    } break;
    default:
      fprintf (stderr, "Unexpected call back reason: %d\n", pcb->reason);
      ret = 1;
      break;
    }
  return ret ;
}


// Top -level entry point to scan the design during the setup of the cosimulation
PLI_INT32 BCosim::scanDesign()
{
  vpi_printf ((PLI_BYTE8*) "Scanning design for cosimulation setup.\n");


  std::string file_name = map_file;
  _map = new PathMap(100);

  VHandle root(NULL);
  GenModule mgen(*this);
  root.map_children (vpiModule, mgen);

  printf("Loading scan map '%s' ...\n", file_name.data());
  _map->Load(file_name.data(), mgen);;
  printf("Loading scan map '%s' complete.\n", file_name.data());

  return 0;
}

// Main command loop processing
// Note that the line processing here sucks. One could write set: (expected) or set or shit or shot:, etc.
// This needs to be cleaned up.
PLI_INT32 BCosim::processCommands ()
{
  PLI_INT32 ret = 0;
  char buf0[1024];
  bool continueLoop = true;
  while(continueLoop) {
    int scanres = fscanf (m_inChan, " %s:", (char *) buf0);
    if (1 == scanres) {
      switch (buf0[0]) {
      case 'D': /*Data*/
        continueLoop = dataCmd();
        break;
      case 'P': 
        continueLoop = pathCmd();
        break;
      case 'R': 
        continueLoop = repeatCmd();
        break;
      case 't': /*time*/
        continueLoop = timeCmd();
        break;
      case '/': /* comment */
	fgets(buf0, 1024, m_inChan);
	if ((strlen(buf0) == 1023) && (buf0[1022] != '\n')) {
	  fprintf (stderr, "Error on input channel: line too long\n");
	  ret = 1;
	  continueLoop = false;
	  break;
	}
	continueLoop = true;
        break;
      case 'f': /*finish*/
        continueLoop = finishCmd();
        break;
      default:
        continueLoop = defaultCmd (buf0);
        break;
      }
    } else /*if (EOF == scanres)*/ {
      continueLoop = false;
      if (ferror (m_inChan) ) {
        fprintf (stderr, "Error on input channel: %s\n", strerror(errno) );
        clearerr (m_inChan);
        ret = 1;
      } else if (feof(m_inChan)) {
	continueLoop = true;
	sleep(1);
	fprintf (stderr, "EOF in channel\n" );

	if (!ignore_eof) {
	  continueLoop = false;
	  // EOF is the same as finish
	  finishCmd();
	  ret = 0;
	}
      } else {
        fprintf (stderr, "EOF ? on in channel\n" );
        ret = 1;
        break ;
      }
    }
  }
  return ret;
}

// Finish command
bool BCosim::finishCmd()
{
  printf ("Cosim finish called.\n");
  vpi_flush();
  vpi_control (vpiFinish);
  return false;
}

bool BCosim::dataCmd()
{
  bool cont ;
  unsigned int value;

  if (1 == (fscanf (m_inChan, " %x", &value))) {
    // only toggle the clock if;
    // - this is the first data  at this time
    // - there was data in the previous cycle
    if (_first_data && _cycle_data && _sim == MODELSIM) {
      _map->PulseClocks(_sim, _half);
    }
    cont = true;
    _cycle_data = true;
    _first_data = false;
    _map->Push(value);
    for (;;) {
      if (!_map->NotEmpty()) { break; }
      StateDesc* desc = _map->GetDesc();
      if (desc) {
	setSignalValue(desc, _map->First());
      }
      _map->Pop();
    }
  } else {
    fprintf (stderr, "Error in input channel.\n");
    cont = false;
    exit(1);
  }
  return cont;
}

void BCosim::setSignalValue(StateDesc* desc, const char* value)
{
  static char val_str[1024];
  if (desc) {
    if(desc->IsMemory()) {
      // printf("(%lld) Setting %s[%d] to %s\n", _time_current, desc->Name(), desc->Index(), _map->First());
    } else {
      // printf("(%lld) Setting %s to %s\n", _time_current, desc->Name(), _map->First());
    }
    VHandle* h = desc->Handle();
    if (h) {
      // printf("HAS HANDLE\n");
      if (desc->IsClock()) {

	// ignore signal since its handled elsewhere

// 	static s_vpi_value val2;
// 	static s_vpi_time t2;
// 	t2.type = vpiSimTime;
// 	t2.high = 0;
// 	t2.low = (2 * _half) - 1;
// 	strcpy(val_str, "1");
// 	val2.format = vpiBinStrVal;
// 	val2.value.str = val_str;
// 	vpi_put_value (h->get(), &val2, &t2, vpiTransportDelay);

// 	static s_vpi_value val3;
// 	static s_vpi_time t3;
// 	t3.type = vpiSimTime;
// 	t3.high = 0;
// 	t3.low = 3 * _half;
// 	strcpy(val_str, "0");
// 	val3.format = vpiBinStrVal;
// 	val3.value.str = val_str;
// 	vpi_put_value (h->get(), &val3, &t3, vpiTransportDelay);

      } else {
	static s_vpi_value val;
	static s_vpi_time t;
	t.type = vpiSimTime;
	t.high = 0;
	t.low  = 0;
	strcpy(val_str, value);
	val.format = vpiBinStrVal;
	val.value.str = val_str;
	vpi_put_value (h->get(), &val, &t, vpiTransportDelay);
      }
    } else {
      // printf("NO HANDLE\n");
    }
  }
}

bool BCosim::pathCmd()
{
  bool cont ;
  unsigned int probe, chain;
  long long time;
  if (3 == (fscanf (m_inChan, " %d %d %lld", &probe, &chain, &time))) {
//    printf("Starting scan: %x\n", num);
    _map->SetPath(probe, chain);
    long long delta;
    time = time * 2 * _half;
    delta = time - _time_current;
    if (delta == 0) {
      //      printf("No delta.\n");
      cont = true;
    } else if (delta > 0) {
      //      printf("Delta is %lld\n", delta);

      if (_cycle_data && _sim == CVC) {
 	_map->PulseClocks(_sim, _half);
      }
      _first_data = true;

      _time_current = time;
      registerDelayCallBack ((PLI_UINT32) delta);

    //   printf("----------------------------------------\n");
//       vpiHandle topiter, topiref;
//       topiter = vpi_iterate(vpiModule, NULL);
//       for (;;)
// 	{
// 	  if ((topiref = vpi_scan(topiter)) == NULL) break;
// 	  VHandle h = topiref;
// //	  GenModule::displayAllValues(h, "");
// 	}
//       printf("----------------------------------------\n");

      cont = false;
    } else {
      fprintf (stderr, "Delta time is negative (setting to %lld when current is %lld).\n", \
	       time, _time_current); 
      cont = false;
      exit(1);
    }
  } else {
    fprintf (stderr, "Error in input channel, path command.\n");
    cont = false;
    exit(1);
  }
  return cont;
}

bool BCosim::repeatCmd()
{
  bool cont ;
  long long time;
  if (1 == (fscanf (m_inChan, " %lld", &time))) {
    _map->RepeatPath();
    long long delta;
    time = time * 2 * _half;
    delta = time - _time_current;
    if (delta == 0) {
//      printf("No delta.\n");
      cont = true;
    } else if (delta > 0) {
//      printf("Delta is %lld.\n", delta);
      _first_data = true;

      _time_current = time;
      registerDelayCallBack ((PLI_UINT32) delta);

   //    printf("----------------------------------------\n");
//       vpiHandle topiter, topiref;
//       topiter = vpi_iterate(vpiModule, NULL);
//       for (;;)
// 	{
// 	  if ((topiref = vpi_scan(topiter)) == NULL) break;
// 	  VHandle h = topiref;
// //	  GenModule::displayAllValues(h, "");
// 	}
//       printf("----------------------------------------\n");

      cont = false;
    } else {
      fprintf (stderr, "Delta time is negative (setting to %lld when current is %lld).\n", \
	       time, _time_current); 
      cont = false;
      exit(1);
    }
  } else {
    fprintf (stderr, "Error in input channel, time command.\n");
    cont = false;
    exit(1);
  }
  return cont;
}

// Continue simulation for n ticks
bool BCosim::timeCmd()
{
  bool cont ;
  PLI_UINT32 time;
  if (1 == (fscanf (m_inChan, " %d", &time))) {
    cont = false;
    registerDelayCallBack (time);
  } else {
    fprintf (stderr, "Error in input channel, time command.\n");
    cont = false;
    exit(1);
  }
  return cont;
}

PLI_INT32 BCosim::valueChangeCallBack (t_cb_data *pcb, BCCallBack *mycb)
{
  VHandle cbnet(pcb->obj);
  string value = VValue::showVal(pcb->value);
  PLI_INT32 time = pcb->time->low ;

  BCModule &module = getModule (mycb->getModIdx());
  BCPort &port = module.getPort (mycb->getPortIdx());

  fprintf (getOutChan(), "%d: %s %s = %s\n", 
           time,  module.getName(), port.getName(), value.c_str() );
  return 0;
}

// Default/error command
bool BCosim::defaultCmd(const char buf[])
{
  fprintf (stderr, "Error: Unexpeceted command in input channel: %s.\n", buf);
  exit(1);
  return false;
}


// Utility -- add a BModule to cosim, and set index properly
BCModule & BCosim::addModule (VHandle mh)
{
  m_modules.push_back (BCModule(mh) );
  BCModule &m = m_modules.back();
  m.setIdx (m_modules.size() - 1);

  return m;
}

// Access data via indexes.
BCModule & BCosim::getModule (unsigned int m)
{
  return m_modules[m];
}

// Get the top module for this cosim
BCModule & BCosim::getModule ()
{

  return  BCosim::lookupModule(top_module.c_str());

}


BCPort & BCosim::getPort (unsigned int m, unsigned int p)
{
  BCModule &mod = getModule(m);
  return mod.getPort(p);
}

// return module reference lookup by name.
BCModule &  BCosim::lookupModule (const char *pn)
{
  for (unsigned int i = 0 ; i < m_modules.size(); i++ ) {
    PLI_BYTE8 *n = m_modules[i].getName();
    if (0 == (strcmp (n, pn))){
      return m_modules[i];
    }
  }
  return BCModule::nullModule;
}

// return port reference lookup by name
BCPort &  BCosim::lookupModulePort (const char *mn, const char *pn)
{
  BCModule & mod = lookupModule(mn);
  return mod.lookupPort(pn);
}


// Destroys all objection
void BCosim::destroy ()
{
  if (0 != BCosim::s_singleton ) {
    delete BCosim::s_singleton;
    BCosim::s_singleton = 0;
   }
}

extern "C" {
  // Entry point for .so and bluespec cosim
  void BCosim_boot ()
  {
    BCosim::initialize();
  }

  // for modelsim
  void (*vlog_startup_routines[])() = { BCosim_boot, 0u };
}

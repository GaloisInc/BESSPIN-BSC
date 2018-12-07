#include <map>
#include <set>
#include <string>
#include <sstream>

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "LuminaPlus.h"

#define BUFSIZE 80

// ----------------------------------------------------------------------
// State and configuration

// The path to the user's DUT
// We replace this with "/top" in the names visible to the user
std::string topinst = "";

// The current setting of the readback on/off switch
bool rdback_enabled = false;

// The list of signals enabled for readback
// The set contains the user-visible names for the signals
std::set<std::string > rdback_signals;

// The currently loaded breakpoint expression
// If this is empty, then breakpoint is off and signals can be edited
std::string bkpoint_expr;

// Breakpoint information for a signal
typedef struct {
  Signal *signal;
  unsigned int val;
  unsigned int mask;
  unsigned int groups;
} BkpointData;

typedef std::map<std::string, BkpointData> BkpointMap;

// The list of signals selected for breakpoint use
BkpointMap bkpoint_signals;

// ----------------------------------------------------------------------
// Utilities

extern "C" {
  int compileBoolExpr (char *buf, uint16_t *lutp);
}

// In this program, we hide the hierarchy above the user's DUT,
// and replace it with "/top", so expect the user to type "/top".
// The following function converts from absolute name to user view.
const char * abs_path_to_user_path(const char *mod)
{
  const char *top = topinst.c_str();
  if (mod[0] == '\0') {
    return top;
  } else {
    int i = 0;
    while ((top[i] != '\0') && (mod[i] == top[i])) {
      i++;
    }
    if (top[i] != '\0') {
      return NULL;
    }
    if (mod[i] == '\0') {
      // don't allow reference to the top instance
      return NULL;
    }
    return &(mod[i]);
  }
}

// In this program, we hide the hierarchy above the user's DUT,
// and replace it with "/top", so expect the user to type "/top".
// The following function converts from user view to absolute name.
const char * user_path_to_abs_path(const char *rel_path)
{
  std::string full_path = topinst;
  if (rel_path[0] == '\0') {
  } else if (strncmp(rel_path,"/top", 4) != 0) {
    return NULL;
  } else {
    full_path.append(&(rel_path[4]));
  }
  return full_path.c_str();
}

void internal_error(const char *msg)
{
  printf("Internal Error: %s\n", msg);
  exit(1);
}

Design *getRdBack()
{
  return LuminaPlus::getOrCreate()->getDesign();
}

LuminaPlusControl *getCtrl()
{
  return LuminaPlus::getOrCreate()->getLuminaPlusControl();
}

void drain_responses()
{
  RdBackStatus s;
  while (getCtrl()->getStatusNonBlocking(s)) {};
}

bool uses_group(std::string s, char c)
{
  return (s.find(c) != std::string::npos);
}

bool group_is_in_use(char c) {
  return uses_group(bkpoint_expr, c);
}

bool group_is_empty(unsigned int group_mask) {
  bool no_signal = true;
  BkpointMap::iterator it;
  for ( it = bkpoint_signals.begin() ;
	no_signal && (it != bkpoint_signals.end()) ;
	++it ) {
    no_signal = ((it->second.groups & group_mask) == 0);
  }
  return no_signal;
}

// ----------------------------------------------------------------------
// Execute commands

void do_help()
{
  printf("Available commands are:\n");
  printf("BASIC\n");
  printf("  help                 - This help message\n");
  printf("  quit                 - Exit the program\n");
  printf("EMULATION CONTROL\n");
  printf("  status\n");
  printf("  run                  - Start the clock free-running\n");
  printf("  run <n>              - Run the clock for 'n' cycles\n");
  printf("  stop                 - Stop the clock\n");
  printf("  resume               - Resume a stopped clock\n");
  printf("  readback_on          - Enable the readback engine\n");
  printf("  readback_off         - Disable the readback engine\n");
  printf("DESIGN HIERARCHY\n");
  printf("  lsinst               - Display the submodule instances of the root\n");
  printf("  lsinst <name>        - Display the submodule instances of a module\n");
  printf("  lsnet <name>         - Display the signals inside a module\n");
  printf("READBACK\n");
  printf("  rdback_add    <net>  - Enable readback for signal 'net'\n");
  printf("  rdback_remove <net>  - Disable readback for signal 'net'\n");
  printf("  rdback_list          - Display the signals enabled for readback\n");
  //printf("  rdback_values        - Retrieve the values of the enabled signals\n");
  printf("BREAKPOINT CONTROL\n");
  printf("  bkpoint_add <net> <val> <mask> <groups>\n");
  printf("                       - Add a signal to the breakpoint table\n");
  printf("  bkpoint_remove <net> - Remove a signal from the breakpoint table\n");
  printf("  bkpoint_list         - Display the signals in the breakpoint table\n");
  printf("  bkpoint_set <e>      - Set and enable the breakpoint expression\n");
  printf("  bkpoint_clear        - Clear/disable the breakpoint expression\n");
}

void do_status()
{
  LuminaPlusControl *ctrl = getCtrl();

  // Drain any accumulated simulation control messages
  // (is this necessary?)
  drain_responses();

  if (ctrl->sendEmuQuery()) {
    RdBackStatus s = ctrl->getStatusBlocking();
    printf("Controlled clock cycle: %" PRIu64 "\n", s.cycle);
    if (s.running) {
      printf("Controlled clock is running.\n");
      if (s.free_running)
	printf("Controlled clock is free running.\n");
      else
	printf("Controlled clock has %u edges remaining.\n", s.edges);
    } else {
      printf("Controlled clock is stopped.\n");
      if (! s.free_running)
	printf("Controlled clock has %u edges remaining.\n", s.edges);
    }
    if (s.rdback_on)
      printf("ReadBack is ON.\n");
    else
      printf("ReadBack is OFF.\n");
    unsigned int frames = getRdBack()->getInUseFrameCount();
    printf("ReadBack frames in use: %d\n", frames);
  } else {
    printf("ERROR: command failed\n");
  }

  // flush the VCD
  getRdBack()->flushVCD();
}

void do_run(unsigned int n)
{
  // Drain any accumulated simulation control messages
  // (is this necessary?)
  drain_responses();

  // XXX Check the status and only do if clock is stopped?

  if (n == 1)
    printf("Running 1 cycle.\n");
  else
    printf("Running %u cycles.\n", n);
  getCtrl()->sendEmuEdges(n);
}

void do_run_free()
{
  // Drain any accumulated simulation control messages
  // (is this necessary?)
  drain_responses();

  // To set the clock free-running, we request the maximum edges.
  // For the LuminaPlus controller, that's 0x1FFFFFFF.

  printf("Running.\n");
  getCtrl()->sendEmuEdges(0x1FFFFFFF);
}

void do_stop()
{
  // Drain any accumulated simulation control messages
  // (is this necessary?)
  drain_responses();

  printf("Stopping.\n");
  getCtrl()->sendEmuStop();

  // wait until the status confirms stop
  drain_responses();
  bool running = true;
  while (running) {
    if (getCtrl()->sendEmuQuery()) {
      RdBackStatus s =getCtrl()->getStatusBlocking();
      running = s.running;
    } else {
      internal_error("query command failed");
    }
  }

  // flush the VCD
  getRdBack()->flushVCD();
}

void do_resume()
{
  // Drain any accumulated simulation control messages
  // (is this necessary?)
  drain_responses();
  
  // XXX Check the status and only do if clock is stopped?

  printf("Resuming.\n");
  getCtrl()->sendEmuResume();
}

void do_readback_on()
{
  if (rdback_enabled) {
    printf("ERROR: ReadBack is already on.\n");
  } else {
    rdback_enabled = true;
    getCtrl()->sendRdBackOn();
  }
}

void do_readback_off()
{
  if (!rdback_enabled) {
    printf("ERROR: ReadBack is already off.\n");
  } else {
    rdback_enabled = false;
    getCtrl()->sendRdBackOff();
  }
}

void do_lsinst(char *rel_path)
{
  const char *s = user_path_to_abs_path(rel_path);
  if (s == NULL) {
    printf("ERROR: Cannot find instance '%s'.\n", rel_path);
    return;
  }
  std::string full_path(s);

  Module *module = getRdBack()->findModule(RTL, full_path);
  if (module == NULL) {
    printf("ERROR: Cannot find instance '%s'.\n", rel_path);
  } else {
    bool ignore_hidden = true;
    const tModuleSet & children = module->getChildren(ignore_hidden);
    for (tModuleSet::const_iterator it = children.begin();
	 it != children.end(); it++)
      {
	Module *child = (*it);
	std::string childname = child->getName();

	// To print just the child name
	//printf("%s\n", childname.c_str());

	// To print the full path
	//std::string childpath = child->getPath();
	//childpath.append("/");
	//childpath.append(childname);
	//printf("%s\n", childpath.c_str());
	
	// To print the full path, relative to the user's DUT
	std::string childpath = child->getPath();
	childpath.append("/");
	childpath.append(childname);
	std::string childrelpath = "/top";
	childrelpath.append(abs_path_to_user_path(childpath.c_str()));
	printf("%s\n", childrelpath.c_str());
      }
  }
}

void do_lsnet(char *rel_path)
{
  const char *s = user_path_to_abs_path(rel_path);
  if (s == NULL) {
    printf("ERROR: Cannot find instance '%s'.\n", rel_path);
    return;
  }
  std::string full_path(s);

  Module *module = getRdBack()->findModule(RTL, full_path);
  if (module == NULL) {
    printf("ERROR: Cannot find instance '%s'.\n", rel_path);
  } else {
    bool ignore_hidden = true;
    const tSignalSet & signals = module->getSignals(ignore_hidden);
    for (tSignalSet::const_iterator it = signals.begin();
	 it != signals.end(); it++)
      {
	Signal *signal = (*it);
	std::string signalname = signal->getName();

	// Choose not to display signals that have no available bits
	bool is_available = (signal->getAvail() != 0);
	if (is_available) {
	  // Note that some bits may still not be available

	  // The Signal class has other information we can query,
	  // but we choose just to display the name.

	  // XXX can signal->isHidden() ever be true?

	  // To print just the signal name
	  //printf("%s\n", signalname.c_str());

	  // To print the full path
	  //std::string signalpath = module->getPath();
	  //signalpath.append("/");
	  //signalpath.append(module->getName())
	  //signalpath.append("/");
	  //signalpath.append(signalname);
	  //printf("%s\n", signalpath.c_str());
	
	  // To print the full path, relative to the user's DUT
	  std::string signalpath = module->getPath();
	  signalpath.append("/");
	  signalpath.append(module->getName());
	  signalpath.append("/");
	  signalpath.append(signalname);
	  std::string signalrelpath = "/top";
	  signalrelpath.append(abs_path_to_user_path(signalpath.c_str()));
	  printf("%s\n", signalrelpath.c_str());
	} // if is_available

      } // for signals
  }
}

void do_rdback_add(char *rel_path)
{
  const char *s = user_path_to_abs_path(rel_path);
  if (s == NULL) {
    printf("ERROR: Cannot find signal '%s'.\n", rel_path);
    return;
  }
  std::string full_path(s);

  // Check that the signal exists and get a pointer to it
  Signal *signal = getRdBack()->findSignal(RTL, full_path);
  if (signal == NULL) {
    printf("ERROR: Cannot find signal '%s'.\n", rel_path);
    return;
  }

  // Check that there are available bits
  if (signal->getAvail() == 0) {
    printf("ERROR: Signal '%s' is unavailable.\n", rel_path);
    return;
  }

  // Check that the signal has not already been enabled
  std::set<std::string >::iterator it = rdback_signals.find(rel_path);
  if (it == rdback_signals.end()) {
    rdback_signals.insert(rel_path);
    getRdBack()->enableSignal(signal);
  } else {
    printf("ERROR: Signal '%s' is already enabled.\n", rel_path);
  }
}

void do_rdback_remove(char *rel_path)
{
  const char *s = user_path_to_abs_path(rel_path);
  if (s == NULL) {
    printf("ERROR: Cannot find signal '%s'.\n", rel_path);
    return;
  }
  std::string full_path(s);

  Signal *signal = getRdBack()->findSignal(RTL, full_path);
  if (signal == NULL) {
    printf("ERROR: Cannot find signal '%s'.\n", rel_path);
    return;
  }

  // Check that the signal is enabled
  std::set<std::string >::iterator it = rdback_signals.find(rel_path);
  if (it == rdback_signals.end()) {
    printf("ERROR: Signal '%s' is not enabled.\n", rel_path);
  } else {
    rdback_signals.erase(it);
    getRdBack()->disableSignal(signal);
  }
}

void do_rdback_list()
{
  std::set<std::string >::iterator it;
  for (it = rdback_signals.begin(); it != rdback_signals.end(); ++it)
    printf("%s\n", it->c_str());
}

/*
void do_rdback_values()
{
  if (rdback_signals.empty()) {
    printf("No signals selected for readback.\n");
  } else {
    LuminaPlus::getOrCreate()->do_readback();
    std::set<std::string >::iterator it;
    for (it = rdback_signals.begin(); it != rdback_signals.end(); ++it) {
      const char *s = user_path_to_abs_path(it->c_str());
      if (s == NULL) {
	throw ("Unknown signal `" + *it + "'");
      }
      std::string full_path(s);
      printf("%s: %s\n", it->c_str(), LuminaPlus::getOrCreate()->query(full_path).c_str());
    }
  }
}
*/

void do_bkpoint_add(char *rel_path, unsigned int val, unsigned int mask, unsigned int groups)
{
  const char *s = user_path_to_abs_path(rel_path);
  if (s == NULL) {
    printf("ERROR: Cannot find signal '%s'.\n", rel_path);
    return;
  }
  std::string full_path(s);

  // Check that the signal exists and get a pointer to it
  Signal *signal = getRdBack()->findSignal(RTL, full_path);
  if (signal == NULL) {
    printf("ERROR: Cannot find signal '%s'.\n", rel_path);
    return;
  }

  // Check that there are available bits
  if (signal->getAvail() == 0) {
    printf("ERROR: Signal '%s' is unavailable.\n", rel_path);
    return;
  }
  // Note that only some bits may be available, while others are not
  // XXX We should warn if the mask tries to use unavailable bits.
  // XXX See Module.cpp for examples of iterating over the bits.

  // Check that the signal has not already been enabled
  BkpointMap::iterator it = bkpoint_signals.find(rel_path);
  if (it == bkpoint_signals.end()) {

    // Check that the groups value is valid
    if (groups > 15) {
      printf("ERROR: The groups value must be 0 to 15.\n");
      return;
    }

    // Check that the signal is not being added to a group in use
    if (((groups & 0x1) != 0) && group_is_in_use('A')) {
      printf("ERROR: Cannot add signals to group A while it is active.\n");
      return;
    }
    if (((groups & 0x2) != 0) && group_is_in_use('B')) {
      printf("ERROR: Cannot add signals to group B while it is active.\n");
      return;
    }
    if (((groups & 0x4) != 0) && group_is_in_use('C')) {
      printf("ERROR: Cannot add signals to group C while it is active.\n");
      return;
    }
    if (((groups & 0x8) != 0) && group_is_in_use('D')) {
      printf("ERROR: Cannot add signals to group D while it is active.\n");
      return;
    }

    BkpointData d;
    d.signal = signal;
    d.val = val;
    d.mask = mask;
    d.groups = groups;
    bkpoint_signals.insert(std::pair<std::string, BkpointData>(rel_path, d));

    // The readback object is not updated.
    // It is only updated when a breakpoint expression is set or cleared.

  } else {
    printf("ERROR: Signal '%s' has already been added.\n", rel_path);
  }
}

void do_bkpoint_remove(char *rel_path)
{
  const char *s = user_path_to_abs_path(rel_path);
  if (s == NULL) {
    printf("ERROR: Cannot find signal '%s'.\n", rel_path);
    return;
  }
  std::string full_path(s);

  // Check that the signal exists and get a pointer to it
  Signal *signal = getRdBack()->findSignal(RTL, full_path);
  if (signal == NULL) {
    printf("ERROR: Cannot find signal '%s'.\n", rel_path);
    return;
  }

  // Check that the signal has previously been added
  BkpointMap::iterator it = bkpoint_signals.find(rel_path);
  if (it != bkpoint_signals.end()) {

    // Check that the signal is not being added to a group in use
    unsigned int groups = it->second.groups;
    if (((groups & 0x1) != 0) && group_is_in_use('A')) {
      printf("ERROR: Cannot remove signals from group A while it is active.\n");
      return;
    }
    if (((groups & 0x2) != 0) && group_is_in_use('B')) {
      printf("ERROR: Cannot remove signals from group B while it is active.\n");
      return;
    }
    if (((groups & 0x4) != 0) && group_is_in_use('C')) {
      printf("ERROR: Cannot remove signals from group C while it is active.\n");
      return;
    }
    if (((groups & 0x8) != 0) && group_is_in_use('D')) {
      printf("ERROR: Cannot remove signals from group D while it is active.\n");
      return;
    }

    bkpoint_signals.erase(it);

    // The readback object is not updated.
    // It is only updated when a breakpoint expression is set or cleared.

  } else {
    printf("ERROR: Signal '%s' has not been added.\n", rel_path);
  }
}

void do_bkpoint_list()
{
  if (bkpoint_expr.empty())
    printf("Expression: <disabled>\n");
  else
    printf("Expression: %s\n", bkpoint_expr.c_str());

  printf("Signals:\n");
  BkpointMap::iterator it;
  for (it = bkpoint_signals.begin(); it != bkpoint_signals.end(); ++it)
    printf("%s %X %X %d\n", it->first.c_str(),
	   it->second.val, it->second.mask, it->second.groups);
}

void do_bkpoint_set(char *expr)
{
  // Check that the groups mentioned in the expression have signals in them.
  //   A group with no signals will always be True.  Since this is not
  //   helpful, we issue an error, so the user can remove it from the
  //   expression or add a signal.
  //
  if (uses_group(expr, 'A') && group_is_empty(1)) {
    printf("ERROR: Group 'A' has no signals.\n");
    return;
  } else if (uses_group(expr, 'B') && group_is_empty(2)) {
    printf("ERROR: Group 'B' has no signals.\n");
    return;
  } else if (uses_group(expr, 'C') && group_is_empty(4)) {
    printf("ERROR: Group 'C' has no signals.\n");
    return;
  } else if (uses_group(expr, 'D') && group_is_empty(8)) {
    printf("ERROR: Group 'D' has no signals.\n");
    return;
  }

  uint16_t lut;
  if (compileBoolExpr(expr, &lut) != 0) {
    // the parser will output its own message
    return;
  }

  getRdBack()->addBreakCode((unsigned int)lut);

  BkpointMap::iterator it;
  for (it = bkpoint_signals.begin(); it != bkpoint_signals.end(); ++it) {
    BkpointData d = it->second;
    std::ostringstream oss;
    oss << std::hex << d.mask;
    std::string maskstr(oss.str());

    // Call "addTerm" for each group that the term belongs to.
    //
    // We use a version of "addTerm" that takes separate value and mask.
    // There is also a version of "addTerm" that takes a single string of bits
    // representing both the value and mask (non 0 or 1 characters are wildcards).
    //
    for (int i = 0; i < 4; i++) {
      if ((d.groups & (1 << i)) != 0) {
	getRdBack()->addTerm(d.signal, d.val, maskstr, i);
      }
    }
  } // for bkpoint_signals

  getRdBack()->refreshHW();

  bkpoint_expr = expr;
}

void do_bkpoint_clear()
{
  getRdBack()->addBreakCode(0);
  getRdBack()->removeTerm(0);

  bkpoint_expr.clear();
}

// ----------------------------------------------------------------------
// Parse command line

bool process_cmd(char *line)
{
  //printf("%s\n", line);

  char cmd[BUFSIZE];
  char *arg;

  // read the first word (the command)
  //
  int i = 0;
  while ((line[i] != '\0') && (isalpha(line[i]) || (line[i] == '_'))) {
    cmd[i] = tolower(line[i]);
    i++;
    if (i == BUFSIZE) {
      internal_error("line without terminal");
    }
  }
  cmd[i] = '\0';

  if ((line[i] != '\0') && !isspace(line[i])) {
    printf("ERROR: Invalid character in command: %c\n", line[i]);
    return false;
  }

  // identify the start of an argument, ignoring whitespace
  //
  while ((line[i] != '\0') && isspace(line[i])) {
    i++;
  }
  arg = &(line[i]);

  // remove space at the end of the argument list
  size_t ptr = strlen(arg);
  while ((ptr > 0) && isspace(arg[ptr])) {
    ptr--;
  }
  arg[ptr] = '\0';

  // process according to the command
  //
  if (strcmp(cmd, "quit") == 0) {

    if (arg[0] != '\0')
      printf("ERROR: Command 'quit' does not take an argument.\n");
    else
      return true;

  } else if (strcmp(cmd, "help") == 0) {

    if (arg[0] != '\0')
      printf("ERROR: Command 'help' does not take an argument.\n");
    do_help();
    return false;

  } else if (strcmp(cmd, "status") == 0) {

    if (arg[0] != '\0')
      printf("ERROR: Command 'status' does not take an argument.\n");
    else
      do_status();
    return false;

  } else if (strcmp(cmd, "run") == 0) {

    if (arg[0] == '\0') {
      do_run_free();
    } else {
      unsigned int n;
      char c;
      if ((sscanf(arg, " %u %c", &n, &c) == 1))
	do_run(n);
      else
	printf("ERROR: Command 'run' expects an unsigned int.\n");
    }
    return false;

  } else if (strcmp(cmd, "stop") == 0) {

    if (arg[0] != '\0')
      printf("ERROR: Command 'stop' does not take an argument.\n");
    else
      do_stop();
    return false;

  } else if (strcmp(cmd, "resume") == 0) {

    if (arg[0] != '\0')
      printf("ERROR: Command 'resume' does not take an argument.\n");
    else
      do_resume();
    return false;

  } else if (strcmp(cmd, "readback_on") == 0) {

    if (arg[0] != '\0')
      printf("ERROR: Command 'readback_on' does not take an argument.\n");
    else
      do_readback_on();
    return false;

  } else if (strcmp(cmd, "readback_off") == 0) {

    if (arg[0] != '\0')
      printf("ERROR: Command 'readback_off' does not take an argument.\n");
    else
      do_readback_off();
    return false;

  } else if (strcmp(cmd, "lsinst") == 0) {

    i = 0;
    while ((arg[i] != '\0') && !isspace(arg[i])) {
      i++;
    }
    if (isspace(arg[i])) {
      printf("ERROR: Command 'lsinst' expects zero or one argument.\n");
      return false;
    }

    do_lsinst(arg);
    return false;

  } else if (strcmp(cmd, "lsnet") == 0) {

    i = 0;
    while ((arg[i] != '\0') && !isspace(arg[i])) {
      i++;
    }
    if ((i==0) || isspace(arg[i])) {
      printf("ERROR: Command 'lsnet' expects one argument.\n");
      return false;
    }

    do_lsnet(arg);
    return false;

  } else if (strcmp(cmd, "rdback_add") == 0) {

    i = 0;
    while ((arg[i] != '\0') && !isspace(arg[i])) {
      i++;
    }
    if ((i==0) || isspace(arg[i])) {
      printf("ERROR: Command 'rdback_add' expects one argument.\n");
      return false;
    }

    do_rdback_add(arg);
    return false;

  } else if (strcmp(cmd, "rdback_remove") == 0) {

    i = 0;
    while ((arg[i] != '\0') && !isspace(arg[i])) {
      i++;
    }
    if ((i==0) || isspace(arg[i])) {
      printf("ERROR: Command 'rdback_remove' expects one argument.\n");
      return false;
    }

    do_rdback_remove(arg);
    return false;

  } else if (strcmp(cmd, "rdback_list") == 0) {

    if (arg[0] != '\0')
      printf("ERROR: Command 'rdback_list' does not take an argument.\n");
    else
      do_rdback_list();
    return false;

/*
  } else if (strcmp(cmd, "rdback_values") == 0) {

    if (arg[0] != '\0')
      printf("ERROR: Command 'rdback_values' does not take an argument.\n");
    else
      do_rdback_values();
    return false;
*/

  } else if (strcmp(cmd, "bkpoint_add") == 0) {

    i = 0;
    while ((arg[i] != '\0') && !isspace(arg[i])) {
      i++;
    }
    if ((i==0) || (arg[i] == '\0')) {
      printf("ERROR: Command 'bkpoint_add' expects four arguments.\n");
      return false;
    }
    arg[i] = '\0';
    char *path = arg;
    arg = &(arg[i+1]);

    unsigned int val, mask, groups;
    char c;
    if (sscanf(arg," %x %x %d %c", &val, &mask, &groups, &c) != 3) {
      printf("ERROR: Incorrect arguments to command 'bkpoint_add'.\n");
      return false;
    }

    do_bkpoint_add(path, val, mask, groups);
    return false;

  } else if (strcmp(cmd, "bkpoint_remove") == 0) {

    i = 0;
    while ((arg[i] != '\0') && !isspace(arg[i])) {
      i++;
    }
    if ((i==0) || isspace(arg[i])) {
      printf("ERROR: Command 'bkpoint_remove' expects one argument.\n");
      return false;
    }

    do_bkpoint_remove(arg);
    return false;

  } else if (strcmp(cmd, "bkpoint_list") == 0) {

    if (arg[0] != '\0')
      printf("ERROR: Command 'bkpoint_list' does not take an argument.\n");
    else
      do_bkpoint_list();
    return false;

  } else if (strcmp(cmd, "bkpoint_set") == 0) {

    if (arg[0] == '\0') {
      printf("ERROR: Command 'bkpoint_set' expects an expression argument.\n");
      return false;
    }

    do_bkpoint_set(arg);
    return false;

  } else if (strcmp(cmd, "bkpoint_clear") == 0) {

    if (arg[0] != '\0')
      printf("ERROR: Command 'bkpoint_clear' does not take an argument.\n");
    else
      do_bkpoint_clear();
    return false;

  } else {

    printf("ERROR: Unrecognized command.\n");
    //do_help();
    return false;

  }

  internal_error("command did not return a 'done' value");
  return false;

} // process_cmd

// ----------------------------------------------------------------------
// Interactive loop

void prompt_loop()
{
  char buf[BUFSIZE];
  bool done = false;

  do {

    printf("> ");
    fflush(stdout);

    if (fgets(buf, BUFSIZE, stdin) != NULL) {

      size_t ln = strlen(buf) - 1;
      if (buf[ln] == '\n') {
	buf[ln] = '\0';
	done = process_cmd(buf);
      } else {
	// ignore any data beyond BUFSIZE
	int ignored;
	do {
	  ignored = fgetc(stdin);
	} while ((ignored != '\n') && (ignored != EOF));
	if (ignored == EOF) {
	  done = true;
	  printf("\n");
	}
	printf("ERROR: Input too long.\n");
      }

    } else { // fgets == NULL
      done = true;
    }

  } while (!done);

} // prompt_loop

// ----------------------------------------------------------------------
// Main

int main(int argc, char *argv[])
{
  if (argc != 2) {
    printf("usage: %s <xrf_file>\n", argv[0]);
    return 1;
  }
  char *xrfname = argv[1];

  // XXX Support specifying the top instance
  //topinst = ...;

  // XXX Support specifying the port
  unsigned int port = 9000;

  try {

    // Initialize the Lumina state before doing anything
    LuminaPlus::getOrCreate()->init(port);

    // Load the xrf file, if provided
    if (getRdBack()->parse_xrf(xrfname) != 0) {
      return 1;
    }
    //if (getRdBack()->syncConfig() != 0) {
    //  return 1;
    //}

    prompt_loop();

  } catch (const std::exception &err) {
    std::cout << err.what() << std::endl;
    return 1;
  } catch (const std::string &err) {
    std::cout << err << std::endl;
    return 1;
  }

  return 0;
}

// ----------------------------------------------------------------------


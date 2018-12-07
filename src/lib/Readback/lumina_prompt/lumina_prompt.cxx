#include <map>
#include <set>
#include <string>

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "Lumina.h"

#define BUFSIZE 80

// ----------------------------------------------------------------------
// State and configuration

// The path to the user's DUT
// We replace this with "/top" in the names visible to the user
std::string topinst = "";

// The list of signals enabled for readback
// The set contains the user-visible names for the signals
std::set<std::string > rdback_signals;

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
  return Lumina::getOrCreate()->getDesign();
}

// ----------------------------------------------------------------------
// Execute commands

void do_help()
{
  printf("Available commands are:\n");
  printf("BASIC\n");
  printf("  help                 - This help message\n");
  printf("  quit                 - Exit the program\n");
  printf("DESIGN HIERARCHY\n");
  printf("  lsinst               - Display the submodule instances of the root\n");
  printf("  lsinst <name>        - Display the submodule instances of a module\n");
  printf("  lsnet <name>         - Display the signals inside a module\n");
  printf("READBACK\n");
  printf("  rdback_add    <net>  - Enable readback for signal 'net'\n");
  printf("  rdback_remove <net>  - Disable readback for signal 'net'\n");
  printf("  rdback_list          - Display the signals enabled for readback\n");
  printf("  rdback_values        - Retrieve the values of the enabled signals\n");
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

void do_rdback_values()
{
  if (rdback_signals.empty()) {
    printf("No signals selected for readback.\n");
  } else {
    Lumina::getOrCreate()->do_readback();
    std::set<std::string >::iterator it;
    for (it = rdback_signals.begin(); it != rdback_signals.end(); ++it) {
      const char *s = user_path_to_abs_path(it->c_str());
      if (s == NULL) {
	throw ("Unknown signal `" + *it + "'");
      }
      std::string full_path(s);
      printf("%s: %s\n", it->c_str(), Lumina::getOrCreate()->query(full_path).c_str());
    }
  }
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

  } else if (strcmp(cmd, "rdback_values") == 0) {

    if (arg[0] != '\0')
      printf("ERROR: Command 'rdback_values' does not take an argument.\n");
    else
      do_rdback_values();
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

#ifdef JTAG_CONTROL
  // XXX Support specifying the cable/device
  std::string cablename = "";
  uint32_t device = 0;
#else
  // XXX Support specifying the port
  unsigned int port = 9000;
#endif

  try {

    // Initialize the Lumina state before doing anything
#ifdef JTAG_CONTROL
    Lumina::getOrCreate()->init(cablename, device);
#else
    Lumina::getOrCreate()->init(port);
#endif

    // Load the xrf file, if provided
    if (getRdBack()->parse_xrf(xrfname) != 0) {
      return 1;
    }

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


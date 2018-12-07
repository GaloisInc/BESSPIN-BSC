#include <iostream>

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "ClkCtrlSocket.hpp"

#define BUFSIZE 80

// ----------------------------------------------------------------------
// Global state

ClkCtrlSocket *glbl_ctrl = NULL;

// ----------------------------------------------------------------------
// Utilities

void internal_error(const char *msg)
{
  printf("Internal Error: %s\n", msg);
  exit(1);
}

ClkCtrl *getCtrl()
{
  ClkCtrl *ctrl = glbl_ctrl;
  return ctrl;
}

void drain_responses()
{
  ClkCtrlStatus s;
  while (getCtrl()->getStatusNonBlocking(s)) {};
}

// ----------------------------------------------------------------------
// Execute commands

void do_help()
{
  printf("Available commands are:\n");
  printf("BASIC\n");
  printf("  help                 - This help message\n");
  printf("  quit                 - Exit the program\n");
  printf("CLOCK CONTROL\n");
  printf("  status\n");
  printf("  run                  - Start the clock free-running\n");
  printf("  run <n>              - Run the clock for 'n' cycles\n");
  printf("  stop                 - Stop the clock\n");
  printf("  resume               - Resume a stopped clock\n");
}

void do_status()
{
  ClkCtrl *ctrl = getCtrl();

  // Drain any accumulated simulation control messages
  // (is this necessary?)
  drain_responses();

  if (ctrl->sendQuery()) {
    ClkCtrlStatus s = ctrl->getStatusBlocking();
    printf("Controlled clock cycle: %" PRIu64 "\n", s.cycle);
    if (s.running) {
      if (s.free_running)
	printf("Controlled clock is free running.\n");
      else {
	if (s.edges > 0)
	  printf("Controlled clock is running.\n");
	else
	  printf("Controlled clock is stopped.\n");
	printf("Controlled clock has %u edges remaining.\n", s.edges);
      }
    } else {
      printf("Controlled clock is stopped.\n");
      if (! s.free_running)
	printf("Controlled clock has %u edges remaining.\n", s.edges);
    }
  } else {
    printf("ERROR: command failed\n");
  }
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
  getCtrl()->sendRunN(n);
}

void do_run_free()
{
  // Drain any accumulated simulation control messages
  // (is this necessary?)
  drain_responses();

  printf("Running.\n");
  getCtrl()->sendRun();
}

void do_stop()
{
  // Drain any accumulated simulation control messages
  // (is this necessary?)
  drain_responses();

  printf("Stopping.\n");
  getCtrl()->sendStop();

  // wait until the status confirms stop
  drain_responses();
  bool running = true;
  while (running) {
    if (getCtrl()->sendQuery()) {
      ClkCtrlStatus s = getCtrl()->getStatusBlocking();
      running = s.running;
    } else {
      internal_error("query command failed");
    }
  }
}

void do_resume()
{
  // Drain any accumulated simulation control messages
  // (is this necessary?)
  drain_responses();
  
  // XXX Check the status and only do if clock is stopped?

  printf("Resuming.\n");
  getCtrl()->sendResume();
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
  if (argc != 1) {
    printf("usage: %s\n", argv[0]);
    return 1;
  }

  // XXX Support specifying the port
  unsigned int port = 9000;

  try {

    // Initialize the ClkCtrlSocket state
    glbl_ctrl = new ClkCtrlSocket(port);

    // Start the service loop
    glbl_ctrl->startServiceLoop();

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


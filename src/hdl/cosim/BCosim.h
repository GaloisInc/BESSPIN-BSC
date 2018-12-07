#pragma once

#include <stdio.h>

#include <vector>
#include <queue>
#include <string>
using namespace std;

#include "vpi_user.h"
#include "PathMap.h"
#include "BCPort.h"

class BCModule;
class BCPort;
class VHandle;

class BCosim {
 private:
  FILE *m_inChan;
  FILE *m_outChan;
  std::string map_file;
  std::string top_module;
  int ignore_eof;

protected:
  vector<class BCModule> m_modules;

protected:
  explicit BCosim (); 

public:
  BCModule & addModule (class VHandle mh);
  FILE *     getOutChan() {return m_outChan;};
  BCModule & getModule (unsigned int m);
  BCModule & getModule ();

private:
  PLI_INT32 openChannels ();
  PLI_INT32 registerStartOfSimCallBack();
  PLI_INT32 registerDelayCallBack(PLI_UINT32 delay);
  PLI_INT32 registerReadWriteSynchCallBack(PLI_UINT32 delay);
  PLI_INT32 scanDesign();

  PLI_INT32 processCommands();
  bool finishCmd();
  bool dataCmd();
  bool pathCmd();
  bool repeatCmd();
  bool timeCmd();
  bool defaultCmd(const char buf[]);
  void setSignalValue(StateDesc* desc, const char* value);
  PLI_INT32 valueChangeCallBack (t_cb_data *pcb, class BCCallBack *);

  BCPort   & getPort (unsigned int m, unsigned int p);
  BCModule & lookupModule (const char *mn);
  BCPort   & lookupModulePort (const char *mn, const char *pn);

  // Static data members
private:
  static BCosim *s_singleton ;
  static queue<int> _data_in;
  static bool m_first;
  static bool _cycle_data;
  static bool _first_data;

  // Static function members
public:
  static PLI_INT32 initialize() ;
  static void destroy() ;
  // Call back function must be static
  static PLI_INT32 generalCallBack (s_cb_data *pcb);
  static queue<int> data_in;
  static PathMap* _map;
  static long long _time_current;
  static long long _half;
  static Simulator _sim;
};

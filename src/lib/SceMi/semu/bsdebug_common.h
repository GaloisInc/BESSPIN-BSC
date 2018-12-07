// Copyright Bluespec Inc. 2009-2010

#pragma once

#include <tcl.h>
#include <vector>
#include "bstcl_base.h"
#include "bsv_scemi.h"

void dumpArguments (Tcl_Interp *interp, const cmd_struct cmds[], const char * given );
void dumpArguments (Tcl_Interp *interp, const cmd_struct_funptr cmds[], const char * given );
void dumpArguments (Tcl_Interp *interp, const cmd_struct_funptr_2 cmds[], const char * given );


//  Top level of emu and capture commands.
// Must use C-style linkagey
extern "C" {
  int Emu_Cmd(ClientData clientData,
		   Tcl_Interp *interp,
		   int objc,
		   Tcl_Obj *const objv[]);

  void Emu_Cmd_Delete(ClientData clientData);

  int RdBk_Cmd(ClientData clientData,
		   Tcl_Interp *interp,
		   int objc,
		   Tcl_Obj *const objv[]);

  void RdBk_Cmd_Delete(ClientData clientData);

  int Capture_Cmd(ClientData clientData,
		   Tcl_Interp *interp,
		   int objc,
		   Tcl_Obj *const objv[]);
  void Capture_Cmd_Delete(ClientData clientData);

  int MemXActor_Cmd(ClientData clientData,
                    Tcl_Interp *interp,
                    int objc,
                    Tcl_Obj *const objv[]);
  void MemXActor_Cmd_Delete(ClientData clientData);


}

////////////////////////////////////////////////////////////////////////////////////
//  Set of overloaded function to add objects to a Tcl list.
////////////////////////////////////////////////////////////////////////////////////
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, Tcl_Obj *o);
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *s);
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const int & i );
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const long & i );
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const Tcl_WideInt & i );
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *fmt, const int & i );
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *fmt, const long & i );
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *fmt, const long long & i );
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *fmt, const unsigned int & i );
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *fmt, const unsigned long & i );
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *fmt, const unsigned long long & i );

template <unsigned int N> inline int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *fmt, const BitT<N> & bitx )
{
  SceMiU64 mask = (~0ll) >> (64 - N);
  SceMiU64  x = (bitx.get64()) & mask;

  char buf[127];
  snprintf(buf, 127, fmt, x);
  return addTclList(interp, l, buf);
}
template <unsigned int N> inline int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const BitT<N> & bitx )
{
  return addTclList(interp, l, "%lld", bitx);
}

template <class T> inline int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const std::vector<T> & vT )
{
  Tcl_Obj *newl = Tcl_NewListObj(0,0);
  for (unsigned int i = 0; i < vT.size() ; ++i ) {
    const T & elem = vT[i];
    addTclList(interp, newl, elem);
  }
  return addTclList(interp, l, newl);
}
template <class T> inline int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *fmt, const std::vector<T> & vT )
{
  Tcl_Obj *newl = Tcl_NewListObj(0,0);
  for (unsigned int i = 0; i < vT.size() ; ++i ) {
    const T & elem = vT[i];
    addTclList(interp, newl, fmt, elem);
  }
  return addTclList(interp, l, newl);
}

template <unsigned int V, class T> inline int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const BSVVectorT<V, T > & vT )
{
  Tcl_Obj *newl = Tcl_NewListObj(0,0);
  for (unsigned int i = 0; i < V ; ++i ) {
    const T & elem = vT.v[i];
    addTclList(interp, newl, elem);
  }
  return addTclList(interp, l, newl);
}

template <unsigned int V, class T> inline int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *fmt, const BSVVectorT<V, T > & vT )
{
  Tcl_Obj *newl = Tcl_NewListObj(0,0);
  for (unsigned int i = 0; i < V ; ++i ) {
    const T & elem = vT.v[i];
    addTclList(interp, newl, fmt, elem);
  }
  return addTclList(interp, l, newl);
}


///////////////////////////////////////////////////////////////////////////////
// Memory Xactor interface
///////////////////////////////////////////////////////////////////////////////
class MemXactor {
public:
  typedef unsigned int       MemAddr;
  typedef unsigned long long MemData;


private:
  typedef local_RAM_Request_Bit_31_Bit_64  Request_T;
  typedef BitT<64>                         Response_T;

  class QueueData {
  public:
    MemAddr  addr;
    MemAddr words;
  };


private:
  // local xactors
  InportQueueT< Request_T >                  m_request;
  OutportQueueT< Response_T >                m_response;
  std::deque<class MemXactor::QueueData>     m_outstanding;
  std::vector<long long>                     m_data;

public:                         // Constructor/destructor
  MemXactor (const std::string & hier
             ,const std::string & instname
             ,SceMi *scemi);

  ~MemXactor();
private:                        // Hide copy constructors
  MemXactor (const MemXactor &);

 public:
  // reactive interface -- does not block
  // all activity is queues
  void readQ(const MemAddr addr);
  void writeQ(const MemAddr addr, const MemData data);
  void fillQ (const MemAddr lo_addr,
              const MemAddr words,
              const MemData data,
              const MemData increment);
  void dumpQ (const MemAddr lo_addr,
              const MemAddr words);

  bool getResponseQ(MemAddr &addr, std::vector<MemData> &vdata);

private:
  void readCore (const MemAddr addr);


};
///////////////////////////////////////////////////////////////////////////////
//
///////////////////////////////////////////////////////////////////////////////

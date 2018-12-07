#pragma once

#include "VHandle.h"
#include "BCModule.h"
#include "BCosim.h"
#include "BCCallBack.h"
#include "BCPort.h"
#include "vpi_user.h"
#include "utils.h"

typedef  tStringList tPath;

// Port generator
class GenPorts : public GenFunctionClass
{
 private:
  BCosim & m_cosim;
  BCModule &m_module;
 public:
  GenPorts( BCosim &bcs, BCModule &mod)
    : m_cosim (bcs)
    , m_module (mod)
  {};

  virtual IterateStatus iterate (VHandle &ph)
  {

    IterateStatus ret ;
    VHandle net = ph.getHandle(vpiLowConn);
    BCPort & port = m_module.addPort( ph, net);

   /*  fprintf (m_cosim.getOutChan(), "%s Port: %s (%d) [%d]\n", */
/*              port.getDirection(), port.getName(), port.getSize(), port.getIdx() ); */

    // register callback on port
    if (port.getDir() == vpiOutput) {
      BCCallBack *mycb = new BCCallBack( m_cosim, m_module.getIdx(), port.getIdx() );

      static t_vpi_time t ;
      t.type = vpiSimTime;
      t.high = 0;
      t.low = 0;

      static t_vpi_value v;
      v.format = vpiHexStrVal;
      v.value.str = NULL;

      static t_cb_data cb;
      cb.reason = cbValueChange ;
      cb.cb_rtn = BCosim::generalCallBack ;
      cb.obj = net.get();
      cb.time = &t;
      cb.value = &v;
      cb.index = 0;
      cb.user_data = (PLI_BYTE8 *) mycb;
      if (0 == vpi_register_cb (&cb)) {
        fprintf(stderr, "Error: BCosim failed to register startOfSimulation call back.\n");
        ret = CONT_ERROR;
      }

    printf ( "%s Port: %s (%d) [%d]\n", 
             port.getDirection(), port.getName(), port.getSize(), port.getIdx() );


    }
    return ret ;
  }
};


/* // Inst generator */
/* class GenInsts : public GenFunctionClass */
/* { */
/*  private: */
/*   BCosim & m_cosim; */
/*   BCModule &m_module; */
/*  public: */
/*   GenInsts( BCosim &bcs, BCModule &mod) */
/*     : m_cosim (bcs) */
/*     , m_module (mod) */
/*   {}; */

/*   virtual IterateStatus iterate (VHandle &ph) */
/*   { */
/*     IterateStatus ret ; */

/*     fprintf (m_cosim.getOutChan(), "INSIDE %s\n",  vpi_get_str(vpiFullName, ph.get())); */
/*     fprintf (m_cosim.getOutChan(), "INSIDE %s\n",  vpi_get_str(vpiName, ph.get())); */

/* //    fprintf (m_cosim.getOutChan(), "%s Port: %s (%d) [%d]\n", */
/* //             port.getDirection(), port.getName(), port.getSize(), port.getIdx() ); */

/*     return ret ; */
/*   } */
/* }; */


// Module generator
class GenModule : public GenFunctionClass
{
private:
  BCosim & m_cosim;
public:
 GenModule( BCosim & bcs)
   :  m_cosim(bcs)
  {
  };

 virtual IterateStatus iterate (VHandle &mh)
  {
    

    BCModule & mod = m_cosim.addModule (mh);
    fprintf (m_cosim.getOutChan(), "Registering top module: %s [%d]\n",
             mh.getStr(vpiName), mod.getIdx());

    GenPorts portgen(m_cosim, mod);
    IterateStatus ret = mh.map_children (vpiPort, portgen);

    return ret;
  }


  VHandle* getInst(std::string full_name);
  VHandle* getInst(tPath path);
  VHandle* getInst(VHandle module, std::string full_name);
  VHandle* getInst(VHandle module, tPath path);
  static tPath toPath(std::string full_name);
  static std::string fromPath(tPath & path);
  static VHandle* getSignal(VHandle module, std::string name);
  static VHandle* getSignal(VHandle module, std::string name, unsigned index);
  static void  initializeAllStateValues(VHandle module);
  static void  initializeStateValues(VHandle module);
  static void  displayAllValues(VHandle module, std::string prefix);
  static void  displayValues(VHandle module, std::string prefix);
 private:
  


};

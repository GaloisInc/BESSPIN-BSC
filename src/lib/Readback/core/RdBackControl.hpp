// Copyright (c) 2013-2016, Bluespec, Inc.   ALL RIGHTS RESERVED
#pragma once

class RdBackControl
{
 protected:
  // The Readback core will register a function to be called
  // with each word of data that arrives from the hardware
  //
  unsigned int (*m_rdbackCallback) (void *, uint32_t);
  void * m_rdbackCallbackParm;

 private:
  // disable these constructors
  //
  RdBackControl( const RdBackControl & );
  RdBackControl & operator= ( const RdBackControl & );

 public:
  RdBackControl()
   : m_rdbackCallback(NULL)
    {
    }

  virtual ~RdBackControl()
    {
    }

  // The method by which the Readback core registers its function
  //
  void registerRdbackCallback(unsigned int f (void *, uint32_t), void *p)
  {
    m_rdbackCallback = f;
    m_rdbackCallbackParm = p;
  }

  // Methods used by the Readback core to program the hardware:

  // Clear the instruction table
  virtual bool sendRdBackClear() = 0;

  // Store a value in the Readback instruction table
  virtual bool sendRdBackStore(unsigned int code) = 0;

  // Indicate end of writing the table
  virtual bool sendRdBackFinish(unsigned int config) = 0;
  
  // Breakpoint lookup table (encoded in 16 bits)
  virtual bool sendRdBackBreakCode(unsigned int code) = 0;
};


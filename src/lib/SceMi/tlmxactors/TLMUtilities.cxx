#include "TLMUtilities.h"
#include "ValidRequest.h"
#include "TLMRequest.h"
#include "../bsvxactors/XactorLog.h"
#include "dllexport.h"

// A namespace for non-templated functions
namespace TLMUtilities {

  /// \brief returns the TLMBSize enum for the provided data width
  ///
  /// Used to set the bsize field in the header.
  DLLEXPORT TLMBSize::E_TLMBSize getBSize(unsigned dsize) {
    switch (dsize) {
    case 8: return TLMBSize::e_BITS8 ;
    case 16: return TLMBSize::e_BITS16 ;
    case 32: return TLMBSize::e_BITS32 ;
    case 64: return TLMBSize::e_BITS64 ;
    case 128: return TLMBSize::e_BITS128 ;
    case 256: return TLMBSize::e_BITS256 ;
    case 512: return TLMBSize::e_BITS512 ;
    case 1024: return TLMBSize::e_BITS1024 ;
    default : {
      XactorLog logfile;
      logfile.Error ( "UNKNOWN DATA size in TLMRequest %d",dsize);
    }
    }
    return TLMBSize::e_BITS32 ;
  }

  DLLEXPORT unsigned lsbZeros (TLMBSize bsize) {
    return lsbZeros (bsize.m_val);
  }
  DLLEXPORT unsigned lsbZeros (TLMBSize::E_TLMBSize bsize) {
    unsigned zeros = 0;
    switch (bsize) {
      case TLMBSize::e_BITS8: 	zeros = 0; break;
      case TLMBSize::e_BITS16:	zeros = 1; break;
      case TLMBSize::e_BITS32: 	zeros = 2; break;
      case TLMBSize::e_BITS64: 	zeros = 3; break;
      case TLMBSize::e_BITS128: 	zeros = 4; break;
      case TLMBSize::e_BITS256: 	zeros = 5; break;
      case TLMBSize::e_BITS512: 	zeros = 6; break;
      case TLMBSize::e_BITS1024: 	zeros = 7; break;
      default : {
        XactorLog logfile;
        logfile.Error ("UNKNOWN BSize in lsbZeros");
      }
    }
    return zeros;
  }

  DLLEXPORT unsigned addrIncr (TLMBSize bsize) {
    return addrIncr(bsize.m_val);
  }
  DLLEXPORT unsigned addrIncr (TLMBSize::E_TLMBSize bsize) {
    unsigned addrIncr = 0;
    switch (bsize) {
      case TLMBSize::e_BITS8: 	addrIncr = 1; break;
      case TLMBSize::e_BITS16:	addrIncr = 2; break;
      case TLMBSize::e_BITS32: 	addrIncr = 4; break;
      case TLMBSize::e_BITS64: 	addrIncr = 8; break;
      case TLMBSize::e_BITS128: 	addrIncr = 16; break;
      case TLMBSize::e_BITS256: 	addrIncr = 32; break;
      case TLMBSize::e_BITS512: 	addrIncr = 64; break;
      case TLMBSize::e_BITS1024: 	addrIncr = 128; break;
    default : {
      XactorLog logfile;
      logfile.Error ("UNKNOWN BSize in addrIncr");
    }
    }
    return addrIncr;
  }

  DLLEXPORT const char * getXactorTypeName ( const E_XactorType xtype) {
    switch (xtype) {
    case e_UNKNOWN:  return "UNKNOWN" ;
    case e_AHB:  return "AHB" ;
    case e_APB:  return "APB";
    case e_AXI3: return "AXI3";
    case e_AXI4: return "AXI4";
    }
    return "UNKNOWN Xactortype";
  }


  template <unsigned int A, unsigned int D, unsigned int U>
  DLLEXPORT unsigned isValid (TLMUtilities::E_XactorType xtype, const TLMRequest<A,D,U> &req, TLMUtilities::ErrorList_t &errs) {
    return ValidRequest<A,D,U>::isValid(xtype, req, errs);
  }

  template <unsigned int A, unsigned int D, unsigned int U>
  DLLEXPORT void templateGenerator() {
    TLMRequest<A,D,U> req;
    ErrorList_t errs;
    isValid<A,D,U>( e_AHB, req, errs);
  }

  template DLLEXPORT void templateGenerator<32,8,0> ();
  template DLLEXPORT void templateGenerator<32,16,0> ();
  template DLLEXPORT void templateGenerator<32,32,0> ();
  template DLLEXPORT void templateGenerator<32,64,0> ();
  template DLLEXPORT void templateGenerator<32,128,0> ();
  template DLLEXPORT void templateGenerator<32,256,0> ();
  template DLLEXPORT void templateGenerator<32,512,0> ();
  template DLLEXPORT void templateGenerator<32,1024,0> ();

  template DLLEXPORT void templateGenerator<64,8,0> ();
  template DLLEXPORT void templateGenerator<64,16,0> ();
  template DLLEXPORT void templateGenerator<64,32,0> ();
  template DLLEXPORT void templateGenerator<64,64,0> ();
  template DLLEXPORT void templateGenerator<64,128,0> ();
  template DLLEXPORT void templateGenerator<64,256,0> ();
  template DLLEXPORT void templateGenerator<64,512,0> ();
  template DLLEXPORT void templateGenerator<64,1024,0> ();


  template DLLEXPORT void templateGenerator<32,8,32> ();
  template DLLEXPORT void templateGenerator<32,16,32> ();
  template DLLEXPORT void templateGenerator<32,32,32> ();
  template DLLEXPORT void templateGenerator<32,64,32> ();
  template DLLEXPORT void templateGenerator<32,128,32> ();
  template DLLEXPORT void templateGenerator<32,256,32> ();
  template DLLEXPORT void templateGenerator<32,512,32> ();
  template DLLEXPORT void templateGenerator<32,1024,32> ();

  template DLLEXPORT void templateGenerator<64,8,32> ();
  template DLLEXPORT void templateGenerator<64,16,32> ();
  template DLLEXPORT void templateGenerator<64,32,32> ();
  template DLLEXPORT void templateGenerator<64,64,32> ();
  template DLLEXPORT void templateGenerator<64,128,32> ();
  template DLLEXPORT void templateGenerator<64,256,32> ();
  template DLLEXPORT void templateGenerator<64,512,32> ();
  template DLLEXPORT void templateGenerator<64,1024,32> ();



  template DLLEXPORT void templateGenerator<32,8,64> ();
  template DLLEXPORT void templateGenerator<32,16,64> ();
  template DLLEXPORT void templateGenerator<32,32,64> ();
  template DLLEXPORT void templateGenerator<32,64,64> ();
  template DLLEXPORT void templateGenerator<32,128,64> ();
  template DLLEXPORT void templateGenerator<32,256,64> ();
  template DLLEXPORT void templateGenerator<32,512,64> ();
  template DLLEXPORT void templateGenerator<32,1024,64> ();

  template DLLEXPORT void templateGenerator<64,8,64> ();
  template DLLEXPORT void templateGenerator<64,16,64> ();
  template DLLEXPORT void templateGenerator<64,32,64> ();
  template DLLEXPORT void templateGenerator<64,64,64> ();
  template DLLEXPORT void templateGenerator<64,128,64> ();
  template DLLEXPORT void templateGenerator<64,256,64> ();
  template DLLEXPORT void templateGenerator<64,512,64> ();
  template DLLEXPORT void templateGenerator<64,1024,64> ();



  
}






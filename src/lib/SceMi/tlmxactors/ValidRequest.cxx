#include "ValidRequest.h"
#include "dllexport.h"

using TLMUtilities::E_XactorType ;

template<unsigned A, unsigned D, unsigned U>
unsigned ValidRequest<A,D,U>::isValid (TLMUtilities::E_XactorType xtype
                                     , const TLMRequest<A,D,U> &req
                                     , TLMUtilities::ErrorList_t &errs) {
    ValidRequest<A,D,U> checker( xtype, req, errs);
    return checker.errCount();
  }

template<unsigned A, unsigned D, unsigned U>
ValidRequest<A,D,U>::ValidRequest ( TLMUtilities::E_XactorType xtype
                                  , const TLMRequest<A,D,U> &req
                                  , TLMUtilities::ErrorList_t &errs)
  : m_type(xtype)
  , m_request(req)
  , m_errors(errs)
  , m_errCnt(0)
{
  check();
}


template<unsigned A, unsigned D, unsigned U>
void ValidRequest<A,D,U>::check() {
  checkLock();
  switch (m_type) {
  case  TLMUtilities::e_UNKNOWN: break;
  case  TLMUtilities::e_AHB: checkAhb(); break;
  case  TLMUtilities::e_APB: checkApb(); break;
  case  TLMUtilities::e_AXI3: checkAxi3(); break;
  case  TLMUtilities::e_AXI4: checkAxi4(); break;
  }
}

template<unsigned A, unsigned D, unsigned U>
void ValidRequest<A,D,U>::checkAddressAligned() {
  unsigned zeros = TLMUtilities::lsbZeros (m_request.m_header.m_b_size);
  long long addr = (long long) m_request.m_address;
  long long mask = ~((-1LL) << zeros);
  if ((mask & addr) != 0) {
    errorAddressAlignment();
  }
}

template<unsigned A, unsigned D, unsigned U>
void ValidRequest<A,D,U>::checkBurstCross(unsigned boundary) {

  long long baseAddr = (long long) m_request.m_address;
  // only incr burst can cross boundary
  if (m_request.m_header.m_burst_mode == TLMBurstMode::e_INCR) {
    unsigned bursts = (unsigned) m_request.m_b_length ;
    unsigned a_incr = TLMUtilities::addrIncr (m_request.m_header.m_b_size) * bursts ;
    long long topAddr = baseAddr + (long long) a_incr ;

    long long mask = (boundary - 1);
    if ((baseAddr & ~mask) != (topAddr & ~mask)) {
      errorBurstCrossing (boundary);
    }
  }
}

/// \brief Check that burst lenght is valid. wrap must be a power of 2 (-1)
template<unsigned A, unsigned D, unsigned U>
void ValidRequest<A,D,U>::checkBurstLenght() {
  if (m_request.m_header.m_burst_mode == TLMBurstMode::e_WRAP) {
    unsigned blen = (unsigned) m_request.m_b_length;
    if (!isPowerOfTwo (blen))  {
      errorBurstLength();
    }
  }
}


/// \brief Check that that exclusive access restrictions are met for AXI transactions
///
/// Address must be aligned withe total byte transfered
/// number of byte must be power of 2 upto 128
template<unsigned A, unsigned D, unsigned U>
void ValidRequest<A,D,U>::checkExclusizeAccessRestrictions( ){
  bool ok = true;
  if (m_request.m_header.m_lock == TLMLock::e_EXCLUSIVE) {
    unsigned blen = (unsigned) m_request.m_b_length;
    unsigned oneXferBytes = TLMUtilities::addrIncr (m_request.m_header.m_b_size);
    unsigned totalBytes = blen * oneXferBytes;

    if ( !isPowerOfTwo (totalBytes) ) {
      errorExclusiveSize(totalBytes);
      ok = false;
    }
    if ((totalBytes > 128) ) {
      errorExclusiveTooBig(totalBytes);
      ok = false;
    } 

    if (ok) {
      // This will error if the above condition is false
      unsigned zeros = TLMUtilities::lsbZeros(TLMUtilities::getBSize(totalBytes*8));
      long long addr = (long long) m_request.m_address;
      long long mask = ~((-1LL) << zeros);
      if ((mask & addr) != 0) {
        errorExclusiveUnaligned(totalBytes);
      }
    }
  }

}

/// \brief Check that the lock field is supported for the xtype
template<unsigned A, unsigned D, unsigned U>
void ValidRequest<A,D,U>::checkLock() {
  bool err = false;
  switch (m_type) {
  case TLMUtilities::e_UNKNOWN:
    break;
  case TLMUtilities::e_AHB:
    err = (m_request.m_header.m_lock != TLMLock::e_NORMAL);
    break;
  case TLMUtilities::e_APB: 
    err = (m_request.m_header.m_lock != TLMLock::e_NORMAL);
    break;
  case TLMUtilities::e_AXI3:
    err =  (m_request.m_header.m_lock == TLMLock::e_RESERVED);
    break;
  case TLMUtilities::e_AXI4:
    err = ( (m_request.m_header.m_lock != TLMLock::e_NORMAL) &&
            (m_request.m_header.m_lock != TLMLock::e_EXCLUSIVE) ) ;
    break;
  }
  if (err) {
    errorWrongLockMode ();
  }

}

template<unsigned A, unsigned D, unsigned U>
void ValidRequest<A,D,U>::checkBurstSize(unsigned maxb) {
  unsigned bursts = (unsigned) m_request.m_b_length ;
  if (bursts > maxb) {
    errorBurstSize(maxb);
  }
}

template class DLLEXPORT ValidRequest<32,8,0>;
template class DLLEXPORT ValidRequest<32,16,0>;
template class DLLEXPORT ValidRequest<32,32,0>;
template class DLLEXPORT ValidRequest<32,64,0>;
template class DLLEXPORT ValidRequest<32,128,0>;
template class DLLEXPORT ValidRequest<32,256,0>;
template class DLLEXPORT ValidRequest<32,512,0>;
template class DLLEXPORT ValidRequest<32,1024,0>;

template class DLLEXPORT ValidRequest<64,8,0>;
template class DLLEXPORT ValidRequest<64,16,0>;
template class DLLEXPORT ValidRequest<64,32,0>;
template class DLLEXPORT ValidRequest<64,64,0>;
template class DLLEXPORT ValidRequest<64,128,0>;
template class DLLEXPORT ValidRequest<64,256,0>;
template class DLLEXPORT ValidRequest<64,512,0>;
template class DLLEXPORT ValidRequest<64,1024,0>;



template class DLLEXPORT ValidRequest<32,8,32>;
template class DLLEXPORT ValidRequest<32,16,32>;
template class DLLEXPORT ValidRequest<32,32,32>;
template class DLLEXPORT ValidRequest<32,64,32>;
template class DLLEXPORT ValidRequest<32,128,32>;
template class DLLEXPORT ValidRequest<32,256,32>;
template class DLLEXPORT ValidRequest<32,512,32>;
template class DLLEXPORT ValidRequest<32,1024,32>;

template class DLLEXPORT ValidRequest<64,8,32>;
template class DLLEXPORT ValidRequest<64,16,32>;
template class DLLEXPORT ValidRequest<64,32,32>;
template class DLLEXPORT ValidRequest<64,64,32>;
template class DLLEXPORT ValidRequest<64,128,32>;
template class DLLEXPORT ValidRequest<64,256,32>;
template class DLLEXPORT ValidRequest<64,512,32>;
template class DLLEXPORT ValidRequest<64,1024,32>;



template class DLLEXPORT ValidRequest<32,8,64>;
template class DLLEXPORT ValidRequest<32,16,64>;
template class DLLEXPORT ValidRequest<32,32,64>;
template class DLLEXPORT ValidRequest<32,64,64>;
template class DLLEXPORT ValidRequest<32,128,64>;
template class DLLEXPORT ValidRequest<32,256,64>;
template class DLLEXPORT ValidRequest<32,512,64>;
template class DLLEXPORT ValidRequest<32,1024,64>;

template class DLLEXPORT ValidRequest<64,8,64>;
template class DLLEXPORT ValidRequest<64,16,64>;
template class DLLEXPORT ValidRequest<64,32,64>;
template class DLLEXPORT ValidRequest<64,64,64>;
template class DLLEXPORT ValidRequest<64,128,64>;
template class DLLEXPORT ValidRequest<64,256,64>;
template class DLLEXPORT ValidRequest<64,512,64>;
template class DLLEXPORT ValidRequest<64,1024,64>;


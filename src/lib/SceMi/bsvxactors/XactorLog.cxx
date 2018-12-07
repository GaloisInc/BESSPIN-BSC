//-*- C++ -*-x
#include <cstdio>
#include <cstdarg>
#include "XactorLog.h"


// HACK
unsigned GetUniversalTickCount() { return 0; }
unsigned GetUniversalTimeDiff(unsigned x) { return 1;}

#define BUFSIZE 511
static char tmpBuf [BUFSIZE+1] ;

int XactorLog::m_iIndentLevel    = 0;
int XactorLog::m_iReportLogLevel = XACTOR_MSG_DEBUG10;

tXactorMessageCallBack  XactorLog::m_pMessageCallback  = XactorLog::DefaultCallback;

void* XactorLog::m_pContext = NULL;
std::string XactorLog::m_strPrefix("XACTOR");

XactorLog::XactorLog()
{
  m_piRetVal     = NULL;
  m_pbRetVal     = NULL;
  m_psRetMessage = NULL;
  m_iLogLevel    = XACTOR_MSG_NONE;
  m_iIndentLevel++;
  m_bInitialized = false;
  m_ulTimeStart  = GetUniversalTickCount();
  m_psFuncName = new std::string();
}

XactorLog::XactorLog(const char* pcFuncName, int iLogLevel, int* piRetVal, std::string* psRetMessage)
{
  m_pbRetVal     = NULL;
  m_piRetVal     = piRetVal;
  m_psRetMessage = psRetMessage;
  m_psFuncName  =  new std::string(pcFuncName);
  m_iLogLevel    = iLogLevel;

  /* print the function header */
  std::string strOutput = *m_psFuncName;
  strOutput += " {";
  Debug(m_iLogLevel, strOutput.c_str());

  m_iIndentLevel++;
  m_ulTimeStart  = GetUniversalTickCount();
  m_bInitialized = true;
}

XactorLog::XactorLog(const char* pcFuncName, int iLogLevel, bool* pbRetVal, std::string* psRetMessage)
{
  m_pbRetVal     = pbRetVal;
  m_piRetVal     = NULL;
  m_psRetMessage = psRetMessage;
  m_psFuncName  =  new std::string(pcFuncName);
  m_iLogLevel    = iLogLevel;

  /* print the function header */
  std::string strOutput = *m_psFuncName;
  strOutput += " {";
  Debug(m_iLogLevel, strOutput.c_str());

  m_iIndentLevel++;
  m_ulTimeStart  = GetUniversalTickCount();
  m_bInitialized = true;
}

XactorLog::~XactorLog()
{
  ULONG32 ulTimeDiff = GetUniversalTimeDiff(m_ulTimeStart);

  if (m_iIndentLevel > 0)
    {
      m_iIndentLevel--;
    }

  std::string strOutput = *m_psFuncName;
  strOutput += " }";

  if (m_piRetVal)
    {
      Debug(m_iLogLevel, "%s:%i (%ldms)", strOutput.c_str(), *m_piRetVal, ulTimeDiff);
    }
  else if (m_pbRetVal)
    {
      Debug(m_iLogLevel, "%s:%s (%ldms)", strOutput.c_str(), *m_pbRetVal ? "true" : "false", ulTimeDiff);
    }
  else if (m_bInitialized)
    {
      Debug(m_iLogLevel, "%s (%ldms)", strOutput.c_str(), ulTimeDiff);
    }
  if(m_psFuncName)
    delete m_psFuncName;
}

void XactorLog::Debug(const char* fmt, ...)
{
  va_list argptr;
  va_start(argptr, fmt);
  Send    (XACTOR_MSG_DEBUG1, fmt, argptr);
  va_end  (argptr);
}

void XactorLog::Debug(int iLogLevel, const char* fmt, ...)
{
  va_list argptr;
  va_start(argptr, fmt);
  Send    (iLogLevel, fmt, argptr);
  va_end  (argptr);
}

void XactorLog::Error(const char* fmt, ...)
{
  va_list argptr;
  va_start(argptr, fmt);
  Send    (XACTOR_MSG_ERROR, fmt, argptr);
  va_end  (argptr);
}

void XactorLog::Warn(const char* fmt, ...)
{
  va_list argptr;
  va_start(argptr, fmt);
  Send    (XACTOR_MSG_WARN, fmt, argptr);
  va_end  (argptr);
}

void XactorLog::Info(const char* fmt, ...)
{
  va_list argptr;
  va_start(argptr, fmt);
  Send    (XACTOR_MSG_INFO, fmt, argptr);
  va_end  (argptr);
}

void XactorLog::Detail(const char* fmt, ...)
{
  va_list argptr;
  va_start(argptr, fmt);
  Send    (XACTOR_MSG_DETAIL, fmt, argptr);
  va_end  (argptr);
}

void XactorLog::VaLog(int iLevel, const char* fmt, va_list ap)
{
#ifndef _WIN32
  va_list argptr;
  va_copy (argptr, ap);
  Send    (iLevel, fmt, argptr);
  va_end  (argptr);
#else
  Send    (iLevel, fmt, ap);
#endif
}


void XactorLog::Send(int iLogLevel, const char* fmt, va_list ap)
{
  if (m_iReportLogLevel < iLogLevel)
    return;

  if (fmt == NULL)
    return;

  std::string strOutput;
  std::string strTmp;

  if (iLogLevel >= XACTOR_MSG_DEBUG)
    {
      if (m_strPrefix.empty() == false)
	{
	  //strOutput.sprintf("%s %02d", m_strPrefix.c_str(), m_iIndentLevel);
#ifdef _WIN32
	  _snprintf_s( tmpBuf, BUFSIZE, _TRUNCATE, "%s:", m_strPrefix.c_str() );
#else
	  snprintf( tmpBuf, BUFSIZE, "%s:", m_strPrefix.c_str() );
#endif
	  strOutput += tmpBuf;
	}
      for (int i = 0; i < m_iIndentLevel; i++)
	{
	  strOutput += "    ";
	}
    }

  //strTmp.vsnprintf(fmt, ap);
  //strOutput += strTmp;
#ifdef _WIN32
  vsnprintf_s(tmpBuf, BUFSIZE, _TRUNCATE, fmt, ap);
#else
  vsnprintf(tmpBuf, BUFSIZE, fmt, ap);
#endif
  strOutput += tmpBuf;

  if ((iLogLevel == XACTOR_MSG_ERROR) && m_psRetMessage)
    {
      strTmp = strOutput;

      if (m_psRetMessage->empty() == false)
	{
	  strTmp  += "\n";
	  strTmp  += *m_psRetMessage;
	}

      *m_psRetMessage = strTmp;
    }

  if (m_pMessageCallback != NULL)
    {
      /* indent ?? */
      m_pMessageCallback(iLogLevel, strOutput, m_pContext);
    }
  else
    {
      printf("%s\n", strOutput.c_str());
    }
}

void XactorLog::SetOutputCallback(tXactorMessageCallBack pMessageCallback, int iLevel, const char* pcPrefix, void* pContext)
{
  m_pMessageCallback = pMessageCallback;
  m_iReportLogLevel  = iLevel;
  m_pContext         = pContext;
  m_strPrefix        = pcPrefix;
}

void XactorLog::DefaultCallback(int iLevel, std::string sOutbuf, void* pContext)
{
  if (iLevel == XACTOR_MSG_ERROR)
    fprintf(stderr, "ERROR  : %s\n", sOutbuf.c_str());
  else if (iLevel == XACTOR_MSG_WARN)
    fprintf(stdout, "WARNING: %s\n", sOutbuf.c_str());
  else if (iLevel < XACTOR_MSG_DEBUG)
    fprintf(stdout, "INFO   : %s\n", sOutbuf.c_str());
  else if (iLevel > XACTOR_MSG_NONE)
    fprintf(stdout, "DEBUG  : %s\n", sOutbuf.c_str());
}

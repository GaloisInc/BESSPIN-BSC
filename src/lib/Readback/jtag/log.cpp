#include "log.h"

// HACK
unsigned GetUniversalTickCount() { return 0; }
unsigned GetUniversalTimeDiff(unsigned x) { return 1;}

#define BUFSIZE 511
static char tmpBuf [BUFSIZE+1] ;

int Log::m_iIndentLevel    = 0;
int Log::m_iReportLogLevel = MSG_NORMAL;

tMessageCallBack  Log::m_pMessageCallback  = Log::DefaultCallback;

void* Log::m_pContext = NULL;
std::string Log::m_strPrefix("");

Log::Log()
{
    m_piRetVal     = NULL;
    m_pbRetVal     = NULL;
    m_psRetMessage = NULL;
    m_iLogLevel    = MSG_NONE;
    m_iIndentLevel++;
    m_bInitialized = false;
    m_ulTimeStart  = GetUniversalTickCount();
    m_psFuncName = new std::string();
}

Log::Log(const char* pcFuncName, int iLogLevel, int* piRetVal, std::string* psRetMessage)
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

Log::Log(const char* pcFuncName, int iLogLevel, bool* pbRetVal, std::string* psRetMessage)
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

Log::~Log()
{
   uint32_t ulTimeDiff = GetUniversalTimeDiff(m_ulTimeStart);

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

void Log::Debug(const char* fmt, ...)
{
    va_list argptr;
    va_start(argptr, fmt);
    Send    (MSG_DEBUG1, fmt, argptr);
    va_end  (argptr);
}

void Log::Debug(int iLogLevel, const char* fmt, ...)
{
    va_list argptr;
    va_start(argptr, fmt);
    Send    (iLogLevel, fmt, argptr);
    va_end  (argptr);
}

void Log::Error(const char* fmt, ...)
{
    va_list argptr;
    va_start(argptr, fmt);
    Send    (MSG_ERROR, fmt, argptr);
    va_end  (argptr);
}

void Log::Warn(const char* fmt, ...)
{
    va_list argptr;
    va_start(argptr, fmt);
    Send    (MSG_WARN, fmt, argptr);
    va_end  (argptr);
}

void Log::Info(const char* fmt, ...)
{
    va_list argptr;
    va_start(argptr, fmt);
    Send    (MSG_INFO, fmt, argptr);
    va_end  (argptr);
}

void Log::Detail(const char* fmt, ...)
{
    va_list argptr;
    va_start(argptr, fmt);
    Send    (MSG_DETAIL, fmt, argptr);
    va_end  (argptr);
}

void Log::VaLog(int iLevel, const char* fmt, va_list ap)
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


void Log::Send(int iLogLevel, const char* fmt, va_list ap)
{
    if (m_iReportLogLevel < iLogLevel)
        return;

    if (fmt == NULL)
        return;

    std::string strOutput;
    std::string strTmp;
    
    if (iLogLevel >= MSG_DEBUG)
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
    
    if ((iLogLevel == MSG_ERROR) && m_psRetMessage)
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

void Log::SetOutputCallback(tMessageCallBack pMessageCallback, int iLevel, const char* pcPrefix, void* pContext)
{
    m_pMessageCallback = pMessageCallback;
    m_iReportLogLevel  = iLevel;
    m_pContext         = pContext;
    m_strPrefix        = pcPrefix;
}

void Log::DefaultCallback(int iLevel, std::string sOutbuf, void* pContext)
{
    if (iLevel == MSG_ERROR)
        fprintf(stderr, "ERROR  : %s\n", sOutbuf.c_str());
    else if (iLevel == MSG_WARN)
        fprintf(stdout, "WARNING: %s\n", sOutbuf.c_str());
    else if (iLevel < MSG_DEBUG)
        fprintf(stdout, "INFO   : %s\n", sOutbuf.c_str());
    else if (iLevel > MSG_NONE)
        fprintf(stdout, "DEBUG  : %s\n", sOutbuf.c_str());
}

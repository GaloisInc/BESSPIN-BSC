#pragma once

#include <string>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <stdint.h>

typedef void (*tMessageCallBack)(int iLevel, std::string sOutbuf, void *pContext);

enum MessageLevelType {
  MSG_NONE = -1,
  MSG_ERROR,
  MSG_WARN,
  MSG_INFO,
  MSG_NORMAL,
  MSG_DETAIL,
  MSG_DEBUG,
  MSG_DEBUG1 = MSG_DEBUG,
  MSG_DEBUG2,
  MSG_DEBUG3,
  MSG_DEBUG4,
  MSG_DEBUG5,
  MSG_DEBUG6,
  MSG_DEBUG7,
  MSG_DEBUG8,
  MSG_DEBUG9,
  MSG_DEBUG10
};


class Log
{
public:
  Log();
  
  ///  Constructor for use in function call
  /// \param pcFuncName   A pointer to the function Name
  /// \param iLogLevel    The start log level for this function
  /// \param piRetVal     The pointer to the return value. If NULL, the value will be ignored
  /// \param psRetMessage The pointer to the return message. Will be logged if available
  Log(const char *pcFuncName, int iLogLevel, int *piRetVal = NULL, std::string *psRetMessage = NULL);

  ///  Alternate constructor for use in function call
  /// \param pcFuncName   A pointer to the function Name
  /// \param iLogLevel    The start log level for this function
  /// \param pbRetVal     The pointer to the return value. If NULL, the value will be ignored
  /// \param psRetMessage The pointer to the return message. Will be logged if available
  Log(const char *pcFuncName, int iLogLevel, bool *pbRetVal = NULL, std::string *psRetMessage = NULL);

  virtual ~Log();

    ///  Set the callback function.
    /// Only messages with log level >= iLevel will be reported.
    /// The pContext will be returned to the callback function.
    /// \param pMessageCallback Pointer to the callback function
    /// \param iLevel           The minimal report level
    /// \param pcPrefix         Prefix for each log output
    /// \param pContext         A context pointer, will be returned with each callback function
    static void SetOutputCallback(tMessageCallBack pMessageCallback, int iLevel = MSG_NORMAL, const char* pcPrefix = NULL, void* pContext = NULL);

    ///  Set the output message level
    /// Only messages with log level <= iLevel will be reported.
    /// \param iLevel           The minimal report level
    static void SetOutputLevel(int iLevel = MSG_NORMAL) { m_iReportLogLevel  = iLevel; }

    ///  Set the progress callback function.
    /// param pProgressCallback Pointer to the callback function
    ///    static void SetProgressCallback(tProgressCallBack pProgressCallback);

    ///  Debug message
    /// The message will be send with the debug level of the
    /// function + 1
    /// \param fmt Format string (like printf) for output
    void Debug(const char* fmt, ...);

    ///  Create a log message with a specific debug level
    /// \param iLogLevel The debug level
    /// \param fmt       Format string (like printf) for output

    void Debug(int iLogLevel, const char* fmt, ...);

    ///  Create a log message with log level ERROR
    /// The message will be appended to the return message string
    /// \param fmt Format string (like printf) for output
    void Error(const char* fmt, ...);

    ///  Create a log message with log level INFO
    /// \param fmt Format string (like printf) for output
    void Info(const char* fmt, ...);

    ///  Create a log message with log level DETAIL
    /// \param fmt Format string (like printf) for output
    void Detail(const char* fmt, ...);

    ///  Create a log message with log level WARN
    /// \param fmt Format string (like printf) for output
    void Warn(const char* fmt, ...);

   private:

    ///  Create a log message with the specific level
    /// \param iLevel The message level
    /// \param fmt    Format string (like printf) for output
    /// \param ap     Vararg list like vprintf
    void VaLog(int iLevel, const char* fmt, va_list ap);

    ///  Send the message to the callback function
    /// \param iLogLevel Only messages with iLogLevel > m_iReportLogLevel are send
    /// \param fmt       Format string (like printf) for output
    /// \param ap        The variable arguments
    void Send(int iLogLevel, const char* fmt, va_list ap);

    ///  The default callback function
    /// \param iLevel   The message level
    /// \param sOutbuf  Contains the message string
    /// \param pContext An optional context pointer
    static void DefaultCallback(int iLevel, std::string sOutbuf, void* pContext);

    /// The pointer to the integer return value. Will be logged in the destructor.
    int* m_piRetVal;

    /// The pointer to the bool return value. Will be logged in the destructor.
    bool* m_pbRetVal;

    /// If no pointer to return value is present in c'tor, this var will be used.
    int  m_iRetVal;

    /// The pointer to the return message. Will be logged in the destructor.
    std::string* m_psRetMessage;

    /// The function name for the log messages
    // This is dynamically allocated to avoid Windows VC++ warning C4251
    // about STL member data in DLLs.
    std::string* m_psFuncName;

    /// The default loglevel for this function
    int m_iLogLevel;

    /// Set if the object is fully initialized
    bool m_bInitialized;

    /// The message callback function
    static tMessageCallBack m_pMessageCallback;

    /// This pointer will be provided on each output callback
    static void* m_pContext;

    /// Store the prefix, which will be printed before every output
    static std::string m_strPrefix;

    /// Every message with level <= m_iReportLogLevel will be sent to the application
    static int m_iReportLogLevel;

    /// An internal marker for the indent level
    static int m_iIndentLevel;

    /// The timestamp of the function
    uint32_t m_ulTimeStart;
};  

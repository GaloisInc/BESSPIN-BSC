
#include <string>
#include <set>
#include <list>

typedef std::set<std::string>  tStringSet;
typedef std::list<std::string> tStringList;

#define TOKENPASTEIN(x, y) x ## y
#define TOKENPASTE(x, y) TOKENPASTEIN(x, y)
#define foreachInStringSet(SET, STRING)  tStringSet* TOKENPASTE(_X_, __LINE__) = SET; for(tStringSet::iterator STRING =  (* TOKENPASTE(_X_, __LINE__)).begin(); STRING !=  (* TOKENPASTE(_X_, __LINE__)).end(); STRING++ )
#define foreachInStringList(LIST, STRING)  tStringList* TOKENPASTE(_X_, __LINE__) = LIST; for(tStringList::iterator STRING =  (* TOKENPASTE(_X_, __LINE__)).begin(); STRING !=  (* TOKENPASTE(_X_, __LINE__)).end(); STRING++ )


char*       itoa (int i);
const char* removeQuotes (const char* orig);
char*       removeQuotes (char* orig);


#include <string>
#include <set>
#include <sstream>
#include <iostream>
#include <list>

typedef std::set<std::string>  tStringSet;
typedef std::list<std::string> tStringList;

#define TOKENPASTEIN(x, y) x ## y
#define TOKENPASTE(x, y) TOKENPASTEIN(x, y)
#define foreachInStringSet(SET, STRING)  tStringSet* TOKENPASTE(_X_, __LINE__) = SET; for(tStringSet::iterator STRING =  (* TOKENPASTE(_X_, __LINE__)).begin(); STRING !=  (* TOKENPASTE(_X_, __LINE__)).end(); STRING++ )
#define foreachInStringList(LIST, STRING)  tStringList* TOKENPASTE(_X_, __LINE__) = LIST; for(tStringList::iterator STRING =  (* TOKENPASTE(_X_, __LINE__)).begin(); STRING !=  (* TOKENPASTE(_X_, __LINE__)).end(); STRING++ )


using std::cout;
using std::endl;
using std::list;

template <class T>
std::ostream& operator<< (std::ostream& os, std::list<T> _x)
{

 os << "<list";
 typename std::list<T>::iterator it;
 for( it = _x.begin(); it != _x.end(); ++it)
   {
     T zow = (T) *it;
     os << " ";
     os << zow;
   }
  os << ">";
  return os;
}


template < class T >
std::string ToString(const T &arg)
{
	std::ostringstream out;
	out << arg;

	return(out.str());
}


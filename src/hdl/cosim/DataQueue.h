
#ifndef _DATAQUEUE_H_
#define _DATAQUEUE_H_

#include <string>

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/

class DataQueue
{

public :
  // Constructors
  explicit DataQueue();
  // Destructor
  ~DataQueue() ;

private :
  // Prevent compiler from defining the following
//  DataQueue& operator=(const DataQueue &) ; // Purposely leave unimplemented

public :

  bool        HasN(unsigned num);
  void        PushN(unsigned value, unsigned size);
  void        PopN(unsigned num);
  const char* FirstN(unsigned num);
  void        Clear();

 private:

  std::string          _data;
  std::string          _current;
  
} ; // class DataQueue;

#endif // #ifndef _DATAQUEUE_H_

// -*- c++ -*-

#ifndef __READBACKPROBE_H__
#define __READBACKPROBE_H__

#include <assert.h>
#include <string>
#include <sstream>

class ReadBackProbe
{
private:
    /// instance that this probe corresponds to
    //Instance * inst;

    /// net name of this probe
    std::string name;

    /// base net name
    std::string basename;

    /// index
    unsigned int index;

    /// width of signal in bits
    int width;

    /// unique id
    unsigned int id;

    /// next available unique id
    static unsigned int next_id;

    /// disable default constructors
  ReadBackProbe & operator= (const ReadBackProbe &);
  ReadBackProbe (const ReadBackProbe &);

public:
    ReadBackProbe(const std::string * newname, int newwidth)
    {
        name     = *newname;
	basename = *newname;
	index    = 0;
        width    = newwidth;
        id       = next_id++;
    }

    ReadBackProbe(const std::string * newname, int newwidth, unsigned int newid)
    {
        name     = *newname;
	basename = *newname;
	index    = 0;
        width    = newwidth;
        id       = newid;
	next_id  = std::max(next_id, newid + 1);
    }

  ReadBackProbe(const std::string * newname, int newwidth, unsigned int newindex, unsigned int newid)
    {
        name     = *newname;
	std::ostringstream convert;
	convert << "[" << newindex << "]";
	name.append(convert.str());

	basename = *newname;
	index    = newindex;
        width    = newwidth;
        id       = newid;
	next_id  = std::max(next_id, newid + 1);
    }

    ~ReadBackProbe()
    {
    }

    /// Query unique id
    unsigned int getId(void)
    {
        return id;
    }

    /// query net name
    std::string getName(void)
    {
        return name;
    }

    /// query net basename
    std::string getBaseName(void)
    {
        return basename;
    }

    /// query net index
    unsigned int getIndex(void)
    {
      return index;
    }

    /// query the width in bits of this probe
    unsigned int getWidth(void)
    {
        return width;
    }
};

#endif

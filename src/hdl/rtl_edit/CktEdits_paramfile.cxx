// Copyright 2010, Bluespec Inc. All rights reserved

#include "CktEdits.h"

#include "tcl.h"

#include <fstream>
#include <ctime>
#include <boost/tokenizer.hpp>


#define MAX_LINE_SIZE 1023

using namespace std;


class UpdateParamFile : public CktModVisitor {
private:
  ifstream & m_infile;
  ofstream & m_outfile;
  int m_lastSerial;
public:
  UpdateParamFile(ifstream & infile, ofstream &outfile)
    : m_infile(infile)
    , m_outfile(outfile)
    , m_lastSerial(-1)
  {}

  // Should be const...
  int process (CktEdits::ModList_t & edits) {
    int status = writeAndFindMaxSerial();

    for (CktEdits::ModListIter_t it =  edits.begin(); status == TCL_OK && it !=  edits.end();  ++it) {
      status = (*it)->accept (this);
    }
    return status;
  }

private:
  int writeAndFindMaxSerial () {
    char lineBuf[MAX_LINE_SIZE+1];

    typedef boost::tokenizer<boost::char_separator<char> >   tokenizer;
    boost::char_separator<char> sep(" \n\t,");

    time_t nowt = time(NULL) ;
    m_outfile << "// Sce-Mi parameters file amended by CktEdit: " << asctime(localtime(&nowt)) << endl;

    // Scan and write the input file looking for Serial lines
    m_lastSerial = -1;
    while (!m_infile.eof()){
      m_infile.getline(lineBuf, MAX_LINE_SIZE);

      BString line(lineBuf);
      tokenizer tokens(line, sep);
      int i;
      tokenizer::iterator titer;
      for ( titer = tokens.begin(), i = 0; titer != tokens.end(); ++ titer, ++i) {
        if (i == 0 && *titer != "Serial") break;
        if (i == 1) {
          m_lastSerial = max (m_lastSerial, atoi(titer->c_str()) );
        }
      }
      m_outfile << lineBuf << endl;
    }
    m_infile.close();
    return TCL_OK;
  }


protected:
  int novisitor (CktMod *cm) {return TCL_OK;}

  virtual int visit(class AddProbe *cm){
    ++m_lastSerial;
    m_outfile << dec;
    m_outfile << "Serial " << m_lastSerial << " Path        \"" << cm->getInstName() << "\"\n" ;
    m_outfile << "Serial " << m_lastSerial << " PrbNum      " << cm->getKey() << "\n" ;
    m_outfile << "Serial " << m_lastSerial << " Label       \"" << cm->getName() << "\"\n" ;
    m_outfile << "Serial " << m_lastSerial << " Kind        0\n" ;
    m_outfile << "Serial " << m_lastSerial << " Samples     0\n" ;
    m_outfile << "Serial " << m_lastSerial << " Offset      0\n" ;
    m_outfile << "Serial " << m_lastSerial << " Width       " << cm->getWidth() << "\n";
    m_outfile << "Serial " << m_lastSerial << " Type        \"" << cm->getBSVType() << "\"\n" ;

    // Write out sub fields for the probe 
    std::list<string>::iterator nitr;
    std::list<string> widths, nets;
    int first = 1;
    cm->getSignals(nets);
    if (nets.size() > 1) {
      m_outfile << "Serial " << m_lastSerial << " SubNets     \"";
      for (nitr = nets.begin(); nitr != nets.end(); nitr++) {
	if (first == 0)
	  m_outfile << ","; 
	else
	  first = 0;
	m_outfile << *nitr;
      }
      m_outfile << "\"\n" ;
    }
    m_outfile << endl;
    return TCL_OK;
  }

  virtual int visit(class AddCapture *cm)
  {
    ++m_lastSerial;
    m_outfile << dec;
    m_outfile << "Serial " << m_lastSerial << " Path        \"" << cm->getInstName() << "\"\n" ;
    m_outfile << "Serial " << m_lastSerial << " PrbNum      " << cm->getKey() << "\n" ;
    m_outfile << "Serial " << m_lastSerial << " Label       \"" << cm->getName() << "\"\n" ;
    m_outfile << "Serial " << m_lastSerial << " Kind        3\n" ;
    m_outfile << "Serial " << m_lastSerial << " Samples     " << cm->getDepth() << "\n" ;
    m_outfile << "Serial " << m_lastSerial << " Offset      " << cm->getRunWidth() << "\n" ;
    m_outfile << "Serial " << m_lastSerial << " Width       " << cm->getWidth() << "\n";
    m_outfile << "Serial " << m_lastSerial << " Type        \"" << cm->getBSVType() << "\"\n" ;

    // Write out sub fields for the probe 
    std::list<string>::iterator nitr;
    std::list<string> widths, nets;
    int first = 1;
    cm->getSignals(nets);
    if (nets.size() > 1) {
      m_outfile << "Serial " << m_lastSerial << " SubNets     \"";
      for (nitr = nets.begin(); nitr != nets.end(); nitr++) {
	if (first == 0)
	  m_outfile << ","; 
	else
	  first = 0;
	m_outfile << *nitr;
      }
      m_outfile << "\"\n" ;
    }
    m_outfile << endl;
    return TCL_OK;
  }
  virtual int visit(class AddTrigger *cm)
  {
    ++m_lastSerial;
    m_outfile << dec;
    m_outfile << "Serial " << m_lastSerial << " Path        \"" << cm->getInstName() << "\"\n" ;
    m_outfile << "Serial " << m_lastSerial << " PrbNum      " << cm->getKey() << "\n" ;
    m_outfile << "Serial " << m_lastSerial << " Label       \"" << cm->getName() << "\"\n" ;
    m_outfile << "Serial " << m_lastSerial << " Kind        2\n" ;
    m_outfile << "Serial " << m_lastSerial << " Samples     0\n" ;
    m_outfile << "Serial " << m_lastSerial << " Offset      2\n" ;
    m_outfile << "Serial " << m_lastSerial << " Width       0\n" ;
    m_outfile << "Serial " << m_lastSerial << " Type        \"void\"\n";
    m_outfile << endl;
    return TCL_OK;
  }

 virtual int visit(class AddCosim *cm){
    ++m_lastSerial;
    m_outfile << dec;
    m_outfile << "Serial " << m_lastSerial << " Path        \"" << cm->getInstName() << "\"\n" ;
    m_outfile << "Serial " << m_lastSerial << " PrbNum      " << cm->getKey() << "\n" ;
    m_outfile << "Serial " << m_lastSerial << " Label       \"" << cm->getName() << "\"\n" ;
    m_outfile << "Serial " << m_lastSerial << " Kind        4\n" ;
    m_outfile << "Serial " << m_lastSerial << " Samples     0\n" ;
    m_outfile << "Serial " << m_lastSerial << " Offset      0\n" ;
    m_outfile << "Serial " << m_lastSerial << " Width       " << cm->getWidth() << "\n";
    m_outfile << "Serial " << m_lastSerial << " Type        \"Bit#(" << cm->getWidth() << ")\"\n" ;
    m_outfile << endl;
    return TCL_OK;
  }
};


// TODO
// Add check for duplicate probe number
// Add check for rewriting of param files which leads to duplicate information...
//
int CktEdits::params  (Tcl_Interp *interp, const BString &infileName, const BString &outfileName)
{
  BString errMsg;
  // Open and read in file
  ifstream infile (infileName.c_str());
  if (!infile) {
    errMsg = "Cannot open file '" + infileName + "' for reading";
    toTclResult (interp, errMsg);
    return TCL_ERROR ;
  }

  ofstream outfile (outfileName.c_str());
  if (!outfile) {
    errMsg = "Cannot open file '"  + outfileName + "' for output";
    toTclResult (interp, errMsg);
    return TCL_ERROR ;
  }

  UpdateParamFile updateV (infile, outfile);
  int status = updateV.process (s_modifications);

  outfile.close();
  return status;
}

#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>

#include <algorithm>
#include <sstream>
#include <iterator>

#include <gzstream.hpp>

#include "Netlister.hpp"

#define OPENPRN     0
#define CLOSEPRN    1

#define ARRAY       2
#define CELL        3 
#define CELLREF     4
#define DESIGN      5
#define DIRECTION   6
#define INSTANCE    7
#define INSTANCEREF 8
#define INIT        9
#define JOINED      10
#define LIBRARY     11
#define LIBRARYREF  12
#define LUT1        13
#define LUT2        14
#define LUT3        15
#define LUT4        16
#define LUT5        17
#define LUT6        18
#define MEMBER      19
#define NET         20
#define PORT        21
#define PORTREF     22
#define PROPERTY    23
#define Q           24     
#define LOWER_Q     25
#define RENAME      26
#define STRING      27
#define PROGRAM     28
#define CONST0      29
#define CONST1      30

// FLOP CELL NAMES
#define FD          31
#define FDC         32
#define FDCE        33
#define FDE         34
#define FDP         35
#define FDPE        36
#define FDR         37
#define FDRE        38
#define FDS         39
#define FDSE        40

// Signal directions
#define INPUT       41
#define OUTPUT      42

// Program names
#define VIVADO      43
#define PLANAHEAD   44
#define PART        45

#define IS_CLR_INVERTED 46
#define IS_D_INVERTED   47
#define IS_PRE_INVERTED 48
#define IS_R_INVERTED   49
#define IS_S_INVERTED   50

static char  token[32768]; /* token buffer */
static int   lineNumber;  /* current input line number */

unsigned int Netlister::initializeMap()
{

  m_map->clear();

  m_map->seed("(",           OPENPRN);
  m_map->seed(")",           CLOSEPRN);

  m_map->seed("array",       ARRAY);
  m_map->seed("cell",        CELL);
  m_map->seed("cellref",     CELLREF);
  m_map->seed("design",      DESIGN);
  m_map->seed("direction",   DIRECTION);
  m_map->seed("instance",    INSTANCE);
  m_map->seed("instanceref", INSTANCEREF);
  m_map->seed("INIT",        INIT);
  m_map->seed("joined",      JOINED);
  m_map->seed("Library",     LIBRARY);
  m_map->seed("libraryref",  LIBRARYREF);
  m_map->seed("LUT1",        LUT1);
  m_map->seed("LUT2",        LUT2);
  m_map->seed("LUT3",        LUT3);
  m_map->seed("LUT4",        LUT4);
  m_map->seed("LUT5",        LUT5);
  m_map->seed("LUT6",        LUT6);
  m_map->seed("member",      MEMBER);
  m_map->seed("net",         NET);
  m_map->seed("port",        PORT);
  m_map->seed("portref",     PORTREF);
  m_map->seed("property",    PROPERTY);
  m_map->seed("Q",           Q);
  m_map->seed("q",           LOWER_Q);
  m_map->seed("rename",      RENAME);
  m_map->seed("string",      STRING);
  m_map->seed("program",     PROGRAM);
  m_map->seed("_const0_",    CONST0);
  m_map->seed("_const1_",    CONST1);

  // series 6 flops
  m_map->seed("FD",          FD);
  m_map->seed("FDC",         FDC);
  m_map->seed("FDCE",        FDCE);
  m_map->seed("FDE",         FDE);
  m_map->seed("FDP",         FDP);
  m_map->seed("FDPE",        FDPE);
  m_map->seed("FDR",         FDR);
  m_map->seed("FDRE",        FDRE);
  m_map->seed("FDS",         FDS);
  m_map->seed("FDSE",        FDSE);

  m_map->seed("INPUT",       INPUT);
  m_map->seed("OUTPUT",      OUTPUT);

  m_map->seed("Vivado",      VIVADO);
  m_map->seed("PlanAhead",   PLANAHEAD);
  m_map->seed("part",        PART);
  m_map->seed("IS_CLR_INVERTED", IS_CLR_INVERTED);
  m_map->seed("IS_D_INVERTED",   IS_D_INVERTED);
  m_map->seed("IS_PRE_INVERTED", IS_PRE_INVERTED);
  m_map->seed("IS_R_INVERTED",   IS_R_INVERTED);
  m_map->seed("IS_S_INVERTED",   IS_S_INVERTED);

  return 0;
}

bool isDInverted(unsigned int label) {
  return (label == IS_D_INVERTED);
}

bool isRSTInverted(unsigned int label) {
  return (label == IS_CLR_INVERTED || 
	  label == IS_PRE_INVERTED || 
	  label == IS_R_INVERTED || 
	  label == IS_S_INVERTED);
}

Netlister::Netlister()
{

  m_map         = new TokenMap();
  m_libCurrent  = 0;
  m_reg_count   = 0;
  m_family      = NOTSET;
  m_part        = NoPart;
  
  std::vector<Module *> * p = new   std::vector<Module *>(2);
  std::vector<Module *> v = *p;
  v[0] = NULL;
  v[1] = NULL;
  m_top = v;
  initializeMap();
}

Netlister::~Netlister()
{

}


tResult Netlister::createRegBit(EdifView view, unsigned int name_num)
{
  std::string name = m_map->getToken(name_num);
  unsigned int matches, index, index2;
  int index_signed, index2_signed;
  std::replace(name.begin(), name.end(), '[', ' ');
  std::replace(name.begin(), name.end(), ']', ' ');

  matches = sscanf(name.c_str(), " %s %d %s", (char*) token, &index_signed, (char*) token);

  if (matches == 2) {
    if (index_signed < 0) {
      fprintf(stderr, "WARNING: negative index: %s [%d]\n", token, index_signed);
      index = 0;
    } else {
      index = index_signed;
    }

    std::string tmp_0 = (char *) token;
    std::string tmp_1 = std::string(tmp_0.end() - 4, tmp_0.end());
    if (getFamily() == KINTEX7 && tmp_1.compare("_reg") == 0) {
      std::string tmp_2 = std::string(tmp_0.begin(), tmp_0.end() - 4);
      unsigned int id = m_map->getCode((char*) tmp_2.c_str());
      
      Signal* signal = addSignal(view, m_parents.front(), (char*) tmp_2.c_str(), index, index, id, NULL);
      BitRef* br     = signal->getBitRef(index);
      if (br != NULL) {
	//	fprintf(stderr, "REG BIT: %s[%d]\n", signal->getFullName(), index);
	tResult r;
	r.kind = BIT;
	r.p = br;
	return r;
      }
    }

    if (getFamily() == VIRTEX6) {
      unsigned int id = m_map->getCode((char*) tmp_0.c_str());
      Signal* signal = addSignal(view, m_parents.front(), (char*) tmp_0.c_str(), index, index, id, NULL);
      BitRef* br     = signal->getBitRef(index);
      if (br != NULL) {
	tResult r;
	r.kind = BIT;
	r.p = br;
	return r;
      }
    }
  }

  matches = sscanf(name.c_str(), " %s %d %d %s", (char*) token, &index_signed, &index2_signed, (char*) token);

  if (matches == 3) {
    if ((index_signed < 0) || (index2_signed < 0)) {
      fprintf(stderr, "WARNING: negative index: %s [%d:%d]\n", token, index_signed, index2_signed);
    }
    index = (index_signed < 0) ? 0 : index_signed;
    index2 = (index2_signed < 0) ? 0 : index2_signed;

    std::string tmp_0 = (char *) token;
    std::string tmp_1 = std::string(tmp_0.end() - 4, tmp_0.end());
    if (getFamily() == KINTEX7 && tmp_1.compare("_reg") == 0) {
      std::string tmp_2 = std::string(tmp_0.begin(), tmp_0.end() - 4);
      
      std::ostringstream convert;
      convert << tmp_2.c_str();
      convert << "<";
      convert << index;
      convert << ">";

      unsigned int id = m_map->getCode((char*) convert.str().c_str());
      
      Signal* signal = addSignal(view, m_parents.front(), (char*) convert.str().c_str(), index2, index2, id, NULL);
      BitRef* br     = signal->getBitRef(index2);
      if (br != NULL) {
	tResult r;
	r.kind = BIT;
	r.p = br;
	return r;
      }
    }

    if (getFamily() == VIRTEX6) {

      std::ostringstream convert;
      convert << tmp_0.c_str();
      convert << "<";
      convert << index;
      convert << ">";

      unsigned int id = m_map->getCode((char*) convert.str().c_str());

      Signal* signal = addSignal(view, m_parents.front(), (char*) convert.str().c_str(), index2, index2, id, NULL);
      BitRef* br     = signal->getBitRef(index2);
      if (br != NULL) {
	tResult r;
	r.kind = BIT;
	r.p = br;
	return r;
      }
    }
  }

  name = m_map->getToken(name_num);

  std::string tmp_1 = std::string(name.end() - 4, name.end());
  if (getFamily() == KINTEX7 && tmp_1.compare("_reg") == 0) {
    index = 0;
    std::string tmp_2 = std::string(name.begin(), name.end() - 4);
    unsigned int id = m_map->getCode((char*) tmp_2.c_str());
    Signal* signal = addSignal(view, m_parents.front(), (char*) tmp_2.c_str(), index, index, id, NULL);
    BitRef* br     = signal->getBitRef(index);
    if (br != NULL) {
      tResult r;
      r.kind = BIT;
      r.p = br;
      return r;
    }
  }

  if (getFamily() == VIRTEX6) {
    index = 0;
    Signal* signal = addSignal(view, m_parents.front(), (char*) name.c_str(), index, index, name_num, NULL);
    BitRef* br     = signal->getBitRef(index);
    if (br != NULL) {
      tResult r;
      r.kind = BIT;
      r.p = br;
      return r;
    }
  }

  tResult r;
  r.kind = EMPTY;
  return r;

}

bool Netlister::isPrimitive(unsigned int cell_name, unsigned int lib_name, unsigned int lut_init)
{
  return (getKind(cell_name, lib_name, lut_init) != opUNKNOWN);
}

////////////////////////////////////////////////////////////////////////////////
/// This function is heuristic based on Xilinx naming of primitives
/// and registers. It is thus fragile if Xilinx naming changes etc.
////////////////////////////////////////////////////////////////////////////////

opKind Netlister::getKind(unsigned int cell_name, unsigned int lib_name, unsigned int lut_init)
{

  if (cell_name == LUT1 && lut_init == 2) {
    return opBUF;
  }

  if (cell_name == LUT1 && lut_init == 1) {
    return opINV;
  }

  std::map<unsigned int, opKind>::iterator it;
  it = m_kind_map.find(cell_name);

  opKind value = opUNKNOWN;
  if (it == m_kind_map.end()) {
    while (true) {
      if (cell_name == FD   || 
	  cell_name == FDC  || 
	  cell_name == FDCE || 
	  cell_name == FDE  || 
	  cell_name == FDP  || 
	  cell_name == FDPE || 
	  cell_name == FDR  || 
	  cell_name == FDRE || 
	  cell_name == FDS  ||
	  cell_name == FDSE) 
	{
	  value = opREG;
	  break;
	}
      std::string name = m_map->getToken(cell_name);
      if (name.compare("GND") == 0 || name.compare("RTL_GND") == 0)
	{
	  value = opGND;
	  break;
	}
      if (name.compare("VCC") == 0 || name.compare("RTL_PWR") == 0)
	{
	  value = opPWR;
	  break;
	}
      if (name.compare("INV") == 0 || name.compare("RTL_INV") == 0 || name.compare("RTL_INV0") == 0)
	{
	  value = opINV;
	  break;
	}
//      if (name.compare("BUFG") == 0 || name.compare("RTL_BUF") == 0 || name.compare("LUT1") == 0)
      if (name.compare("BUFG") == 0 || name.compare("RTL_BUF") == 0)
	{
	  value = opBUF;
	  break;
	}
      if (name.substr(0,7).compare("RTL_REG") == 0) {
	value = opREG;
	break;
      }
      if (name.substr(0,6).compare("RTL_FD") == 0) {
	value = opREG;
	break;
      }
      if (name.substr(0,11).compare("RTL_wide_fd") == 0) {
	value = opREG_WIDE;
	break;
      }
      if (name.substr(0,7).compare("RTL_xor") == 0) {
	value = opXOR;
	break;
      }

      if (name.substr(0,13).compare("RTL_not_equal") == 0) {
	value = opNEQ;
	break;
      }
      if (name.substr(0,13).compare("RTL_equal") == 0) {
	value = opEQ;
	break;
      }
      if (name.substr(0,3).compare("RTL") == 0) {
	value = opPRIM;
	break;
      }
      if (name.substr(0,3).compare("LUT") == 0) {
	value = opPRIM;
	break;
      }
      break;
    }
    m_kind_map.insert(std::pair<unsigned int, opKind>(cell_name, value));
  } else {
    value = it->second;
  }
    
  return value;

}

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

/* parsing state machine values */
#define	S_BETWEEN       0
#define	S_TOKEN		1
#define	S_QUOTE		2

unsigned int Netlister::parse_rtl(std::string file)
{

  std::ifstream edfFile;
  edfFile.open (file.c_str());

  if (edfFile.is_open()) {
    m_codes.clear();

    fprintf(stdout, "Parsing elaborated RTL netlist: %s ...\n", file.c_str());
    parse_edf(edfFile);

    edfFile.close();

    m_current = m_codes.begin();

    parse_sexpr(RTL);
    
    printf(" NOTE: Source RTL includes %d register state bits.\n", m_reg_count);
    printf("Parsing elaborated RTL netlist: %s complete.\n\n", file.c_str());


    postProcess(RTL);
    return 0;

  } else {
    std::cerr << "Can't open file " << file << std::endl;
    exit(1);
  }
}

unsigned int Netlister::parse_synth(std::string file)
{

  std::ifstream edfFile;
  edfFile.open (file.c_str());

  if (edfFile.is_open()) {
    m_codes.clear();
    m_parents.clear();
    m_cellMap.clear();
    m_reg_count = 0;

    fprintf(stdout, "Parsing post-synthesis netlist: %s ...\n", file.c_str());
    parse_edf(edfFile);

    edfFile.close();

    m_current = m_codes.begin();

    parse_sexpr(SYNTH);

    printf(" NOTE: Post-synthesis netlist includes %d register state bits.\n", m_reg_count);
    printf("Parsing post-synthesis netlist: %s complete.\n\n", file.c_str());

    postProcess(SYNTH);
    return 0;

  } else {
    std::cerr << "Can't open file " << file << std::endl;
    exit(1);
  }
}

unsigned int Netlister::parse_edf(std::ifstream & edfFile)
{

  std::string one_line;

  if (edfFile.is_open()) {

    // initialize data structures
    m_libCurrent = 0;
    register int state, c, l, code;

    state = S_BETWEEN;
    l = 0;
    lineNumber = 0;

    while (!edfFile.eof())
      {

	c = edfFile.get();
	if (c == '\n') lineNumber++;

	switch (state){
	case S_BETWEEN:
	  if (isspace(c))
	    l = 0;
	  else if (c == '(') {
	    token[l++] = c;
	    token[l] = '\0';
	    // collect token
	    code = m_map->getCode((char *) token);
	    m_codes.push_back(code);
	    // fprintf(stderr, "TOKEN: '%s'\n", (char*) token);
	    l = 0;
	  } else if (c == ')') {
	    token[l++] = c;
	    token[l] = '\0';
	    // collect token
	    code = m_map->getCode((char *) token);
	    m_codes.push_back(code);
	    // fprintf(stderr, "TOKEN: '%s'\n", (char*) token);
	    l = 0;
	  } else if (c == '"') {
	    state = S_QUOTE;
	  }
	  else if (c == '\\') {
	    token[l++] = c;
	    state = S_TOKEN;
	    c = edfFile.get();
	    if (c != EOF) token[l++] = c;
	  } else if (c == '&') {
	    // just ignore for now (to clean up Xilinx tokens starting with &)
	  }
	  else {
	    token[l++] = c;
	    state = S_TOKEN;
	  }
	  break;
	case S_TOKEN:
	  if (isspace(c)) {
	    if (l != 0) {
	      token[l] = '\0';
	      // collect token
	      code = m_map->getCode((char *) token);
	      m_codes.push_back(code);
	      // fprintf(stderr, "TOKEN: '%s'\n", (char*) token);
	    }
	    l = 0;
	    state = S_BETWEEN;
	  }  else if (c == '(') {
	    fprintf(stderr, "ERROR: Illegal character '(' inside token.\n");
	    exit(1);
	  } else if (c == ')') {
	    if (l != 0) {
	      token[l] = '\0';
	      // collect token
	      code = m_map->getCode((char *) token);
	      m_codes.push_back(code);
	      // fprintf(stderr, "TOKEN: '%s'\n", (char*) token);
	    }
	    l = 0;
	    token[l++] = c;
	    token[l] = '\0';
	    // collect token
	    code = m_map->getCode((char *) token);
	    m_codes.push_back(code);
	    // fprintf(stderr, "TOKEN: '%s'\n", (char*) token);
	    l = 0;
	    state = S_BETWEEN;
	  } else if (c == '\\') {
	    token[l++] = c;
	    c = edfFile.get();
	    if (c != EOF) token[l++] = c;
	  } else if (c == '"') {
	    fprintf(stderr, "ERROR: Illegal character '\"' inside token.\n");
	    exit(1);
	  } else {
	    token[l++] = c;
	  }
	  break;
	case S_QUOTE:
	  if (c == '\\') {
	    token[l++] = c;
	    c = edfFile.get();
	    if (c != EOF) token[l++] = c;
	  } else if (c == '"') {
	    if (l != 0) {
	      token[l] = '\0';
	      // collect token
	      code = m_map->getCode((char *) token);
	      m_codes.push_back(code);
	      // fprintf(stderr, "TOKEN: '%s'\n", (char*) token);
	    }
	    l = 0;
	    state = S_BETWEEN;
	  } else {
	    token[l++] = c;
	  }
	  break;
	}
      }
    return 0;
  } else {
    std::cerr << "File stream is not open " << std::endl;
    exit(1);
  }
}

unsigned int Netlister::parse_body(EdifView view)
{

  tResult result;
  result.kind  = NUM;
  result.value = 0;

  while (result.kind != ERR) {
    result = parse_sexpr(view);
  }
  return 0;
}

unsigned int Netlister::skip_body()
{

  tResult result;
  result.kind  = NUM;
  result.value = 0;

  while (result.kind != ERR) {
    result = skip_sexpr();
  }
  return 0;
}

tResult Netlister::skip_sexpr()
{
  tResult result;
  result.kind  = EMPTY;

  unsigned int code = (*m_current);
  if (code == OPENPRN) {
    m_current++;
    skip_body();
    return result;
  }
  if (code == CLOSEPRN) {
    m_current++;
    result.kind = ERR;
    return result;
  }

  m_current++;
  return result;

}

tResult Netlister::parse_number()
{

  unsigned int code = *m_current;
  m_current++;
  std::string tkn = m_map->getToken(code);
  unsigned int value;
  int value_signed;
  unsigned int matches = sscanf (tkn.c_str(), "%d", &value_signed);
  if (matches == 1) {
    if (value_signed < 0) {
      fprintf(stderr, "WARNING: parse_number: negative number: %d\n", value_signed);
      value = 0;
    } else {
      value = value_signed;
    }

    tResult result;
    result.kind  = NUM;
    result.value = value;
    return result;
  } else {
    fprintf(stderr, "ERROR:Token '%s' is not a number!\n", tkn.c_str());
    exit(1);
  }
}

tResult Netlister::parse_sexpr(EdifView view)
{

  
  unsigned int code = *m_current;
  tResult result;
  tResult r;
  result.kind = EMPTY;

  if (false) {
    std::string tkn = m_map->getToken(code);
    fprintf(stderr, "T: %s ", tkn.c_str());
  }

  if (code == OPENPRN) {
    m_current++;
    unsigned int op = (*m_current);

    if (false) {
      std::string kk = m_map->getToken(op);
      fprintf(stderr, "T: %s ", kk.c_str());
    }

    if (op == ARRAY) {
      m_current++;
      tResult l = parse_sexpr(view);
      tResult n = parse_number();
      parse_body(view);
      std::string name = m_map->getToken(l.name);
      Signal* signal = addSignal(view, m_parents.front(), (char*) name.c_str(), 0, n.value - 1, l.name, NULL);
      result.kind = SIG;
      result.p = signal;
      return result;
    }

    if (op == CELL) {
      m_current++;
      tCodeIter prev = m_current;
      tResult l = parse_sexpr(view);
      m_current = prev;
      backup(2);
      record_cell(l.name, m_libCurrent);
      skip_sexpr();
      result.kind = EMPTY;
      return result;
    }

    if (op == CELLREF) {
      m_current++;
      unsigned int cell_name = *m_current;
      m_current++;
      tResult lib = parse_sexpr(view);
      unsigned int lib_name = lib.name;
      elaborate_cell(view, cell_name, lib_name);
      parse_body(view);
      result.kind = EMPTY;
      return result;
    }

    if (op == DESIGN || op == INSTANCE) {

      m_current++;
      r = parse_sexpr(view);
      if (r.kind != LBL) {
	std::cerr << "Illegal design or instance name. Got type: " << r.kind << " ." << std::endl;
	exit(1);
      }
      
      std::string inst_name = m_map->getToken(r.name);
      

      if (op == DESIGN) {
	Module * mod_root = new Module((char *) "", (char *) "", 0);
	registerModule(view, mod_root);
	m_parents.push_front(mod_root);
      }

      Module * mod = new Module((char *) inst_name.c_str(), (char *) "", r.name);
      Module * parent = m_parents.front();
      parent->addChild(mod);
      registerModule(view, mod);

      m_parents.push_front(mod);
      if (op == DESIGN) {
	m_top[view] = mod;
      }

      parse_sexpr(view);
      parse_body(view);

      m_parents.pop_front();

      if (mod->getKind() == opREG_WIDE) {
	Signal* signal_out = mod->getSignal(Q, true);
	if (signal_out == NULL) {
	  signal_out = mod->getSignal(LOWER_Q, true);
	}
	if (signal_out != NULL) {
	  m_reg_count = m_reg_count + signal_out->getWidth();
	  signal_out->setSrcState();

	  if (r.name != r.alias) {

	    std::string name = m_map->getToken(r.name);

	    Signal* ignore = m_parents.front()->getSignal(r.name, true);
	    bool exists = (ignore != NULL);

	    Signal* signal = addSignal(view, m_parents.front(), (char*) name.c_str(), signal_out->getLsb(), signal_out->getMsb(), r.name, NULL);
	    if (!exists) {
	      signal->setHidden();
	    }
	    signal->merge(signal_out, 0);
	  }

	  std::string name = m_map->getToken(r.alias);
	  //	  fprintf(stderr, "NAME: %s NUM: %d\n", name.c_str(), r.alias);
	  Signal* signal = addSignal(view, m_parents.front(), (char*) name.c_str(), signal_out->getLsb(), signal_out->getMsb(), r.alias, NULL);
	  signal->merge(signal_out, 0);
	}
      }

      if (mod->getKind() == opREG) {
	Signal* signal_out = mod->getSignal(Q, true);
	if (signal_out == NULL) {
	  signal_out = mod->getSignal(LOWER_Q, true);
	}
	
	if (signal_out != NULL) {
	  m_reg_count++;
	  signal_out->setSrcState();
	  //	  fprintf(stderr, "STATE: %s\n", signal_out->getFullName());
	  BitRef* br_out = signal_out->getBitRef(0);

	  if (r.name != r.alias || getFamily() == KINTEX7) {
	    
	    std::string name = m_map->getToken(r.name);

	    Signal* ignore = m_parents.front()->getSignal(r.name, true);
	    bool exists = (ignore != NULL);

	    Signal* signal = addSignal(view, m_parents.front(), (char*) name.c_str(), 0, 0, r.name, NULL);
	    if (!exists) {
	      signal->setHidden();
	    }
	    signal->merge(signal_out, 0);
	  }

	  tResult a = createRegBit(view, r.alias);
	  if (a.kind == BIT) {
	    BitRef* br1 = (BitRef*) a.p;
	    // if (br1 != NULL) {
	    //   Bit* b1 = br1->getBit();
	    //   b1->setKind(Bit::STATE);
	    // }
	    if (br_out != NULL && br1 != NULL) {
	      br1->merge(br_out);
	    }
	  }
	}
      }

      result.kind = EMPTY;
      return result;
    }

    if (op == DIRECTION) {
      m_current++;
      tResult direction = parse_sexpr(view);
      parse_body(view);
      return direction;
    }

    if (op == JOINED) {
      m_current++;
      bool first = true;
      BitRef* br0 = NULL;
      while (true) { 
 	tResult bit = parse_sexpr(view);
 	if (bit.kind == ERR) {
 	  break;
 	}
 	if (bit.kind == EMPTY) {
	  fprintf(stderr, "LLLLLLL\n");
 	  continue;
 	}
	if (bit.kind == BIT) {
	  BitRef* br = (BitRef*) bit.p;
	  if (br != NULL) {
	    if (first) {
	      first = false;
	      br0 = br;
	    } else {
	      br0->merge(br);
	    }
	  }
	  continue;
	}
	fprintf(stderr, "ERROR: Shouldn't get here (%d)\n", bit.kind);
	exit(1);
      }
      if (first) {
	result.kind = EMPTY;
      } else {
	result.kind = BIT;
	result.p = br0;
      }
      return result;
    }

    if (op == INSTANCEREF) {
      m_current++;
      unsigned int id = *m_current;
      m_current++;
      parse_body(view);
      Module* mod     = m_parents.front();
      Module* child   = mod->getChild(id);
      result.kind = MOD;
      result.p = child;
      return result;
    }

    if (op == LIBRARY) {
      m_current++;
      m_libCurrent = *m_current;
      parse_body(view);
      m_libCurrent = 0;
      result.kind = EMPTY;
      return result;
    }

    if (op == LIBRARYREF) {
      m_current++;
      unsigned int lib_name = *m_current;
      parse_body(view);
      result.kind  = LBL;
      result.name  = lib_name;
      result.alias = lib_name;
      return result;
    }

    if (op == MEMBER) {
      m_current++;
      unsigned int signal_id = *m_current;
      Module* mod            = m_parents.front();
      //      std::string name = m_map->getToken(signal_id);
      //      fprintf(stderr, "MEMBER: %s/%s/%s \n", mod->getPath(), mod->getName(), name.c_str());
      Signal* signal         = mod->getSignal(signal_id, true);
      m_current++;
      if (signal != NULL) {
	r                      = parse_number();
	parse_body(view);
	unsigned int index     = r.value;
	index = signal->getMsb() - index;

	BitRef* br = signal->getBitRef(index, true);
	if (br != NULL) {
	  result.kind = BIT;
	  result.p = br;
	}
	return result;
      } else {
	parse_number();
	parse_body(view);
      }
      result.kind = EMPTY;
      return result;
    }

    if (op == NET) {
      m_current++;
      tResult l   = parse_sexpr(view);
      tResult bit = parse_sexpr(view);
      parse_body(view);
      result.kind = EMPTY;
      if (bit.kind != BIT) {
	return result;
      }
      if (l.kind == LBL) {
	Module* mod = m_parents.front();
	Signal* signal = mod->getSignal(l.name, true);
	if (signal == NULL) {
	  if (l.name == CONST0 || l.name == CONST1) {
	    return result;
	  }
	  std::string name  = m_map->getToken(l.name);
	  BitRef* br  = (BitRef*) bit.p;
	  Bit* b      = br->getBit();
	  if (getFamily() == VIRTEX6 && b->getKind() == Bit::CNST) {
	    if (b->getValue() == 0 && name.find("GND") == 0) {
	      return result;
	    }
	    if (b->getValue() == 1 && name.find("PWR") == 0) {
	      return result;
	    }
	  }

	  if (l.name != l.alias) {
	    std:: string alias = m_map->getToken(l.alias);
	    std:: string copy  = alias;
	    std::replace(copy.begin(), copy.end(), '[', ' ');
	    std::replace(copy.begin(), copy.end(), ']', ' ');

	    unsigned int matches;
	    unsigned int index;
	    int index_signed;
	    matches = sscanf(copy.c_str(), " %s %d %s", (char*) token, &index_signed, (char*) token);

	    if (matches == 2) {
	      if (index_signed < 0) {
		fprintf(stderr, "WARNING: negative index: %s [%d]\n", token, index_signed);
		index = 0;
	      } else {
		index = index_signed;
	      }

	      unsigned int id = m_map->getCode((char*) token);
	      Signal* signal = addSignal(view, mod, (char*) token, index, index, id, NULL);
	      BitRef* br0    = signal->getBitRef(index, true);
	      if (br0 != NULL) {
		BitRef* br = (BitRef*) bit.p;
		br0->merge(br);
		result.p = br0;
		result.kind = BIT;
	      }
	    } else {
	      if(name.length() == alias.length()) {
		bool has_dollar  = alias.find('$') !=  std::string::npos;
		bool has_period  = alias.find('.') !=  std::string::npos;
		bool has_bracket = alias.find('[') !=  std::string::npos;
		if ((has_dollar || has_period) && !has_bracket) {
		  Signal* signal = addSignal(view, mod, (char*) alias.c_str(), 0, 0, l.alias, NULL);
		  BitRef* br0    = signal->getBitRef(0, true);
		  if (br != NULL) {
		    BitRef* br = (BitRef*) bit.p;
		    br0->merge(br);
		    result.p = br0;
		    result.kind = BIT;
		  }
		}
	      }
	    }

	    Signal* signal_0 = addSignal(view, mod, (char*) name.c_str(), 0, 0, l.name, NULL);
	    BitRef* br_0     = signal_0->getBitRef(0, true);

	    BitRef* br_1 = NULL;
	    if (result.kind == BIT) {
	      br_1  = (BitRef*) result.p;
	      signal_0->setHidden();
	    } else {
	      Signal* signal_1 = addSignal(view, mod, (char*) alias.c_str(), 0, 0, l.alias, NULL);
	      br_1     = signal_1->getBitRef(0, true);
	      bool has_bracket = alias.find('[') !=  std::string::npos;
	      if (has_bracket) {
		signal_0->setHidden();
		signal_1->setHidden();
	      }
	    }

	    if (br_0 != NULL && br_1 != NULL) {
	      BitRef* br = (BitRef*) bit.p;
	      br_0->merge(br_1);
	      br_0->merge(br);
	      result.p = br_0;
	      result.kind = BIT;
	      return result;
	    }
	  }

	  Signal* signal = addSignal(view, mod, (char*) name.c_str(), 0, 0, l.name, NULL);
	  BitRef* br0    = signal->getBitRef(0, true);

	  if (view == RTL) {
	    if (utils::isAddedXilinxNetName(getFamily(), name.c_str())) {
	      signal->setHidden();
	    }
	  }

	  if (br0 != NULL) {
	    BitRef* br = (BitRef*) bit.p;
	    br0->merge(br);
	    result.p = br0;
	    result.kind = BIT;
	    return result;
	  }
	}
      } else {
	fprintf(stderr, "ERROR: Net name is not a LBL\n.");
	exit(1);
      }
      return result;
    }

    if (op == PORT) {
      m_current++;
      tResult l = parse_sexpr(view);
      tResult d = parse_sexpr(view);
      parse_body(view);
      result = l;
      if (l.kind == LBL) {
	std::string name = m_map->getToken(l.name);
	Signal* signal = addSignal(view, m_parents.front(), (char*) name.c_str(), 0, 0, l.name, NULL);
	result.kind = SIG;
	result.p = signal;
	if (l.name != l.alias) {
	  std:: string copy = m_map->getToken(l.alias);
	  std::replace(copy.begin(), copy.end(), '[', ' ');
	  std::replace(copy.begin(), copy.end(), ']', ' ');
	  unsigned int index;
	  int index_signed;
	  unsigned int matches = sscanf(copy.c_str(), " %s %d ", (char*) token, &index_signed);
	  if (matches == 2) {
	    if (index_signed < 0) {
	      fprintf(stderr, "WARNING: negative index: %s [%d]\n", token, index_signed);
	      index = 0;
	    } else {
	      index = index_signed;
	    }

	    std::string alias = (char*) token;
	    Signal* signal_alias = addSignal(view, m_parents.front(), (char*) token, index, index,  m_map->getCode(alias), NULL);
	    signal->merge(signal_alias, index);
	    signal->setHidden();
	  }
	}
      }
      Signal* s = (Signal*) result.p;
      if (d.name == INPUT) {
	s->setKind(dIn);
      }
      if (d.name == OUTPUT) {
	s->setKind(dOut);
      }
      return result;
    }

    if (op == PORTREF) {
      m_current++;
      tCodeIter prev = m_current;
      skip_sexpr();
      tResult m = parse_sexpr(view);
      m_current = prev;
      Module* mod = m_parents.front();
      if (m.kind == MOD) {
	mod = (Module*) m.p;
      }
      m_parents.push_front(mod);
      // fprintf(stderr, "PUSH  Parent: %s/%s\n", m_parents.front()->getPath(), m_parents.front()->getName());
      tResult l = parse_sexpr(view);
      m_parents.pop_front();
      // fprintf(stderr, "POP Parent: %s/%s\n", m_parents.front()->getPath(), m_parents.front()->getName());
      skip_body();

      if (l.kind == LBL) {
	Signal* signal = mod->getSignal(l.name, true);
	if (signal != NULL) {
	  BitRef* br = signal->getBitRef(0, true);
	  if (br != NULL) {
	    result.kind = BIT;
	    result.p = br;
	    //	    if (b->getId() >= 1000000) {
	    //	      fprintf(stderr, "REFERENCING (%d) %s\n", b->getId(), signal->getFullName());
	    //	    }
	    return result;
	  }
	}
	result.kind = EMPTY;
	return result;
      } else {
	  return l;
      }
    }

    if (op == PROGRAM) {
      m_current++;
      unsigned int prog_name = *m_current;
      if (prog_name == VIVADO) {
	setFamily(KINTEX7);
      } else if (prog_name == PLANAHEAD) {
	setFamily(VIRTEX6);
      } else {
	std::string name = m_map->getToken(prog_name);
	fprintf(stderr, "ERROR: Unknown program '%s'\n", name.c_str());
      }
      parse_body(view);
      result.kind = EMPTY;
      return result;
    }

    if (op == PROPERTY) {
      m_current++;
      unsigned int label = *m_current;
      if (label == INIT || isDInverted(label) || isRSTInverted(label)) {
	r = parse_sexpr(view);
	r = parse_sexpr(view);
	if (view == SYNTH) {
	  Module * parent = m_parents.front();
	  unsigned int matches, ignore;
	  uint64_t value;
	  std::string tmp = m_map->getToken(r.name);
	  // assumptions about how parameter values are printed
	  // hex in the form 32'hFD123 or 1-bit binary like 1'b0
	  std::replace(tmp.begin(), tmp.end(), '\'', ' ');
	  std::replace(tmp.begin(), tmp.end(), 'h', ' ');
	  std::replace(tmp.begin(), tmp.end(), 'b', ' ');
	  matches = sscanf(tmp.c_str(), " %d %" PRIu64 " ",  &ignore, &value);
	  if (matches == 2) {
	    if (label == INIT) {
	      parent->setInit(value);
	      //	      fprintf(stderr, "INIT PARAM: %s\n", parent->getDefName());
	      //	      fprintf(stderr, "VALUE: %lu\n", value);
	    }
	    if (isDInverted(label)) {
	      if (value == 1) {
		parent->setDInverted(true);
	      }
	      //	      fprintf(stderr, "D_INVERTED PARAM: %s\n", parent->getDefName());
	      //	      fprintf(stderr, "VALUE: %lu\n", value);
	    }
	    if (isRSTInverted(label)) {
	      if (value == 1) {
		parent->setRSTInverted(true);
	      }
	    }
	  }
	}
      }
      if (label == PART) {
	r = parse_sexpr(view);
	r = parse_sexpr(view);
	if (view == RTL) {
	  setPart(utils::getPart(m_map->getToken(r.name).c_str()));
	}
      }
      parse_body(view);
      result.kind = EMPTY;
      return result;
    }

    if (op == RENAME) {
      m_current++;
      r = parse_sexpr(view);
      unsigned int name_0 = r.name;
      r = parse_sexpr(view);
      unsigned int name_1 = r.name;
      parse_body(view);
      r.kind = LBL;
      r.name  = name_0;
      r.alias = name_1;
      return r;
    }

    if (op == STRING) {
      m_current++;
      r = parse_sexpr(view);
      parse_body(view);
      return r;
    }

    //    fprintf(stderr, "SKIPPING %s\n", m_map->getToken(op).c_str());

    m_current++;
    parse_body(view);
    return result;
  } else if (code == CLOSEPRN) {
    m_current++;
    result.kind = ERR;
    return result;
  } else {
    m_current++;
    // encountered an atom;
    result.kind  = LBL;
    result.name  = code;
    result.alias = code;
    return result;
  }
}

unsigned int Netlister::backup(unsigned int n)
{
  for (unsigned int i = 0; i < n; i++)
    {
      m_current--;
    }
  return 0;
}

unsigned int Netlister::record_cell (unsigned int cell_name, unsigned int  lib_name) 
{

  // assume no name conflicts across libraries for now.
  m_cellMap.insert(std::pair<unsigned int, tCodeIter>(cell_name, m_current));

  return 0;
}

unsigned int Netlister::elaborate_cell (EdifView view, unsigned int cell_name, unsigned int  lib_name)
{

  Module * mod = m_parents.front();
  unsigned int lut_init = 0;

  if (view == RTL) {
    lut_init = 2;
  }

  std::string name = m_map->getToken(cell_name);
  mod->setDefName(name.c_str());
  if (isPrimitive(cell_name, lib_name, lut_init)) {
    mod->setHidden();
  }
  mod->setKind(getKind(cell_name, lib_name, lut_init));

  std::string* path = new std::string(mod->getPath());
  path->append("/");
  path->append(mod->getName());

  if (view == RTL) {
    m_rtl_def_map.insert(std::pair<std::string, std::string>(name, *path));
  } else {
    m_synth_def_map.insert(std::pair<std::string, std::string>(name, *path));
  }

  tCodeIter prev = m_current;

  tCellIter it = m_cellMap.find(cell_name);

  if (it == m_cellMap.end()) {
    fprintf(stderr, "ERROR: Cell not found '%s'\n", name.c_str());
    exit(1);
  }

  m_current = it->second;
  m_current++;
  m_current++;

  parse_body(view);


  m_current = prev;
  return 0;
}

Signal* Netlister::addSignal(unsigned int num, EdifView view, Module * module, char* name, unsigned int lsb, unsigned int msb, unsigned int id, char* value)
{
  // fprintf(stderr, "ADDING SIGNAL: (%d) %s[%d:%d]\n", num, name, msb, lsb);
  return addSignal(view, module, name, lsb, msb, id, value);
}

Signal* Netlister::addSignal(EdifView view, Module * module, char* name, unsigned int lsb, unsigned int msb, unsigned int id, char* value)
{

  Signal* signal = module->getSignal(id, true);

  if (signal == NULL) {
    signal = new Signal(module, name, id, lsb, msb);
    module->addSignal(signal);
    //    fprintf(stderr, "ADDING: %s (%d)\n", signal->getFullName(), id);
    if (module->getKind() == opGND) {
      BitRef* br = signal->getBitRef(0);
      if (br != NULL) {
	Bit* b = br->getBit();
	b->setKind(Bit::CNST);
	b->setValue(0);
	b->setZ();
      }
    }
    if (module->getKind() == opPWR) {
      BitRef* br = signal->getBitRef(0);
      if (br != NULL) {
	Bit* b = br->getBit();
	b->setKind(Bit::CNST);
	b->setValue(1);
	b->setZ();
      }
    }
    //    fprintf(stderr, "INSERTING: %s (%d)\n", signal->getFullName(), signal->getId());
  } else {
    //    fprintf(stderr, "EXPANDING: %s (%d) %d %d \n", signal->getFullName(), signal->getId(), lsb, msb);
    signal->expand(lsb, msb);
  }

  for (unsigned int i = lsb; i <= msb; i++)
    {
      BitRef* br = signal->getBitRef(i);
      if (br != NULL) {
	Bit* b = br->getBit();
	b->setInferred(false);
      }
    }
  registerSignal(view, signal);
  return signal;
}

Module* Netlister::getTop(EdifView view)
{
  return m_top[view];
}

Signal * Netlister::findSignal(const EdifView view, const std::string path)
{

  if (m_signal_map.count(path) == 0) {
    return NULL;
  } else {
    std::vector<Signal*>* p = m_signal_map.find(path)->second;
    Signal * signal = p->at(view);
    return signal;
  }
}

unsigned int Netlister::registerSignal(const EdifView view, Signal* signal)
{

  std::string path = signal->getFullName();
  if (m_signal_map.count(path) == 0) {
    std::vector<Signal*> * p = new std::vector<Signal*>(2);
    p->insert(p->begin(), NULL);
    p->insert(p->begin()+1, NULL);
    p->insert(p->begin()+view, signal);
    //    fprintf(stderr, "REGISTER: A %d |%s| %d %d\n",view,  path.c_str(), (p->at(0) == NULL), (p->at(1) == NULL));
    m_signal_map.insert(std::pair<std::string, std::vector<Signal*>* >(path, p));
    if (path.length() == 1) {
      m_signal_map.insert(std::pair<std::string, std::vector<Signal*>* >("", p));
    }
  } else {
    std::vector<Signal*>* p = m_signal_map.find(path)->second;
    p->insert(p->begin()+view, signal);
    m_signal_map.insert(std::pair<std::string, std::vector<Signal*>* >(path, p));
    if (path.length() == 1) {
      m_signal_map.insert(std::pair<std::string, std::vector<Signal*>* >("", p));
    }
  }
  
  return 0;

}

Module * Netlister::findModule(const EdifView view, const std::string path)
{

  if (m_module_map.count(path) == 0) {
    return NULL;
  } else {
    std::vector<Module*>* p = m_module_map.find(path)->second;
    Module * module = p->at(view);
    return module;
  }
}

unsigned int Netlister::registerModule(const EdifView view, Module* module)
{

  std::string path = module->getPath();
  path.append("/");
  path.append(module->getName());
  if (m_module_map.count(path) == 0) {
    std::vector<Module*> * p = new std::vector<Module*>(2);
    p->insert(p->begin(), NULL);
    p->insert(p->begin()+1, NULL);
    p->insert(p->begin()+view, module);
    //    fprintf(stderr, "REGISTER: A %d |%s| %d %d\n",view,  path.c_str(), (p->at(0) == NULL), (p->at(1) == NULL));
    m_module_map.insert(std::pair<std::string, std::vector<Module*>* >(path, p));
    if (path.length() == 1) {
      m_module_map.insert(std::pair<std::string, std::vector<Module*>* >("", p));
    }
  } else {
    std::vector<Module*>* p = m_module_map.find(path)->second;
    p->insert(p->begin()+view, module);
    m_module_map.insert(std::pair<std::string, std::vector<Module*>* >(path, p));
    if (path.length() == 1) {
      m_module_map.insert(std::pair<std::string, std::vector<Module*>* >("", p));
    }
  }
  
  return 0;

}

unsigned int Netlister::finalPostProcess()
{

  for (std::map<std::string,  std::vector<Signal*>* >::iterator it =  m_signal_map.begin();
       it != m_signal_map.end(); it++)
    {
      std::vector<Signal*>* p = it->second;
      Signal* signal_rtl   = p->at(RTL);
      Signal* signal_synth = p->at(SYNTH);
      if (signal_rtl != NULL && signal_synth != NULL) {
	//	  Net* net_rtl   = signal_rtl->getNet();
	//	  Net* net_synth = signal_synth->getNet();
	  unsigned int lsb = signal_rtl->getLsb();
	  unsigned int msb = signal_rtl->getMsb();

	  if (signal_synth->getLsb() == lsb && signal_synth->getMsb() == msb) {
	    //	    bool first = true;
	    for (unsigned int i = lsb; i <= msb; i++)
	      {
		BitRef* br_rtl   = signal_rtl->getBitRef(i);
		BitRef* br_synth = signal_synth->getBitRef(i);
		if (br_rtl != NULL && br_synth != NULL) {
		  Bit* b_rtl   = br_rtl->getBit();
		  Bit* b_synth = br_synth->getBit();

		  if (b_rtl->getKind() == Bit::UNAVAILABLE && b_synth->getKind() != Bit::UNAVAILABLE) {
		    b_rtl->setAddr(b_synth->getAddr());
		    b_rtl->setOffset(b_synth->getOffset());
		    b_rtl->setSlr(b_synth->getSlr());
		    b_rtl->setKind(b_synth->getKind());
		    b_rtl->setValue(b_synth->getValue());
		    b_rtl->setZ();
		  }
		}
	      }
	  }
      }
      if (signal_rtl != NULL && signal_synth == NULL && signal_rtl->isSrcState()) {
       	unsigned int lsb = signal_rtl->getLsb();
       	unsigned int msb = signal_rtl->getMsb();
       	for (unsigned int i = lsb; i <= msb; i++)
       	  {
       	    BitRef* br_rtl = signal_rtl->getBitRef(i);
       	    if (br_rtl != NULL) {
       	      Bit* b_rtl   = br_rtl->getBit();
       	      if (b_rtl->getKind() == Bit::UNAVAILABLE) {
       		b_rtl->setKind(Bit::UNREAD);
       		b_rtl->setValue(0);
       		b_rtl->setZ();
		// fprintf(stderr, "%s[%d] UNREAD\n", signal_rtl->getFullName(), i);
       	      }
       	    }
       	  }
      }
    }

  // add "inferred" signals that vivado has dropped during rtl elaboration
  for (std::set<Signal *>::iterator it = Signal::m_all_signals.begin();
       it != Signal::m_all_signals.end(); it++) {
	
    Signal* signal = *it;

    if (signal->getKind() == dIn || signal->getKind() == dOut) {
      if (signal->getAvail() == signal->getWidth() && signal->getWidth() != 1) {
	unsigned int lsb = signal->getLsb();
	unsigned int msb = signal->getMsb();
	Module* module = signal->getModule();
	Module* parent = module->getParent();

	tSignalSet & signals = parent->getSignals(false);
	for (tSignalSet::iterator it = signals.begin();
	     it != signals.end(); it++) {

	  Signal* signal_conn = *it;
	  if (signal_conn != NULL && 
	      signal_conn->getLsb() == lsb &&
	      signal_conn->getMsb() == msb) {
	    unsigned int inferred  = signal_conn->getInferred();
	    unsigned int available = signal_conn->getAvail();
	    if (inferred != 0 && available != 0 && (inferred + available) == signal_conn->getWidth()) {
	      bool valid    = true;
	      bool has_state = false;
	      for (unsigned int i = lsb; i <= msb; i++)
		{
		  BitRef* br_0 = signal->getBitRef(i);
		  BitRef* br_1 = signal_conn->getBitRef(i);
		  if (br_0 != NULL && br_1 != NULL) {
		    Bit* b_0 = br_0->getBit();
		    Bit* b_1 = br_1->getBit();
		    if (b_0 != NULL && b_1 != NULL) {
		      if (b_0->getId() == b_1->getId() || b_1->getInferred()) {
			if (b_1->getKind() == Bit:: STATE) {
			  has_state = true;
			}
			continue;
		      }
		      valid = false;
		      break;
		    }
		  }
		}
	      if (valid && has_state) {
		fprintf(stdout, "Adding inferred bits: (%d/%d) %s %s\n", signal_conn->getAvail(), signal_conn->getWidth(), signal->getFullName(), signal_conn->getFullName());
		for (unsigned int i = lsb; i <= msb; i++)
		  {
		    BitRef* br_0 = signal->getBitRef(i);
		    BitRef* br_1 = signal_conn->getBitRef(i);
		    if (br_0 != NULL && br_1 != NULL) {
		      Bit* b_0 = br_0->getBit();
		      Bit* b_1 = br_1->getBit();
		      if (b_0 != NULL && b_1 != NULL) {
			b_0->merge(b_1);
		      }
		    }
		  }
	      }
	    }
	  }
	}
      }
    }
  }

  unsigned int count_total = 0;
  unsigned int count_covered = 0;
  for (std::map<std::string,  std::vector<Signal*>* >::iterator it =  m_signal_map.begin();
       it != m_signal_map.end(); it++)
    {
      std::vector<Signal*>* p = it->second;
      Signal* signal_rtl   = p->at(RTL);
      //      Signal* signal_synth = p->at(SYNTH);
      if (signal_rtl != NULL && signal_rtl->isSrcState()) {
	count_total   = count_total + signal_rtl->getWidth();
	count_covered = count_covered + signal_rtl->getAvail();
	unsigned int diff = signal_rtl->getWidth() - signal_rtl->getAvail();
	if (diff != 0) {
	  fprintf(stderr, "MISSED NET: %s (%d/%d)\n", signal_rtl->getFullName(), signal_rtl->getAvail(), signal_rtl->getWidth());
	} else {
	  //	  fprintf(stderr, "HAVE NET: %s (%d/%d)\n", signal_rtl->getFullName(), signal_rtl->getAvail(), signal_rtl->getWidth());
	}
      }
    }

  fprintf(stdout, "\nRegister state bit visibility: %d/%d\n", count_covered, count_total);

  return 0;
}


unsigned int Netlister::postProcess(EdifView view)
{

  for (std::map<std::string,  std::vector<Module*>* >::iterator it =  m_module_map.begin();
       it != m_module_map.end(); it++)
    {

      std::vector<Module*>* p = it->second;
      Module* mod = p->at(view);

      if (mod == NULL) {
	continue;
      }

      mod->updateVisibleChildren();
      mod->updateVisibleSignals();

      if (mod->getKind() == opBUF) {
	Signal* signal_out = NULL;
	Signal* signal_in  = NULL;
	tSignalSet & signals = mod->getSignals();
	for (tSignalSet::iterator it = signals.begin();
	     it != signals.end(); it++)
	  {
	    Signal* signal = *it;
  	    if (signal->getKind() == dOut) {
	      signal_out = signal;
		
	    }
	    if (signal->getKind() == dIn) {
	      signal_in = signal;
	    }
	  }
 
	if (signal_out != NULL && signal_in != NULL) {
	  unsigned int r = signal_in->merge(signal_out, 0);
	  if (r) {
	    fprintf(stderr, "BUF: %s -> %s\n", signal_in->getFullName(), signal_out->getFullName());
	  }
	}
      }

      if (view == RTL && mod->getKind() == opXOR) {
	//	  fprintf(stderr, "XOR CELL: %s\n", mod->getDefName());
	Signal* signal_out = NULL;
	Signal* signal_in  = NULL;
	bool all_zeros = false;
	tSignalSet & signals = mod->getSignals();
	for (tSignalSet::iterator it = signals.begin();
	     it != signals.end(); it++)
	  {
	    Signal* signal = *it;
	    if (signal->getKind() == dOut) {
	      signal_out = signal;
		
	    }
	    if (signal->getKind() == dIn) {
	      if (signal->allZeros()) {
		all_zeros = true;
	      } else {
		signal_in = signal;
	      }
	    }
	  }

	if (all_zeros && signal_out != NULL && signal_in != NULL) {
	  signal_in->merge(signal_out, 0);
	  //	    fprintf(stderr, "ASSIGN: %s -> %s\n", signal_in->getFullName(), signal_out->getFullName());
	}
      }
    }

  // Collect equivalent signals
  std::map<std::string, std::set<Signal*>*> equivalence_map;
  for (std::set<Signal *>::iterator it = Signal::m_all_signals.begin();
       it != Signal::m_all_signals.end(); it++)
    {
      Signal* signal = *it;
      std::string signature = signal->getSignature();
      std::map<std::string, std::set<Signal*>*>::iterator jt = equivalence_map.find(signature);
      if (jt == equivalence_map.end()) {
  	std::set<Signal*>* s = new std::set<Signal*>();
  	s->insert(signal);
  	equivalence_map.insert(std::pair<std::string, std::set<Signal*>*>(signature, s));
      } else {
  	std::set<Signal*>* s = jt->second;
  	s->insert(signal);
      }
    }

  
  // Add nets
  tNetSet* all_nets = new tNetSet();
  for (std::map<std::string, std::set<Signal*>*>::iterator it = equivalence_map.begin();
       it != equivalence_map.end(); it++)
    {

      Net* net = new Net();
      std::set<Signal*>* s = it->second;
      for (std::set<Signal*>::iterator it = s->begin();
  	   it != s->end(); it++) {
	Signal* signal = *it;
	net->addSignal(signal);
      }
      all_nets->insert(net);
    }

  unsigned int id = 0;
  for (std::set<Bit *>::iterator it = Bit::m_all_bits.begin();
       it != Bit::m_all_bits.end(); it++)
    {
      Bit* b = *it;
      b->setId(id);
      id++;
    }
  if (id > 0) {
    Bit::setMaxBitId(id - 1);
  }

  return 0;
}

unsigned int Netlister::setFamily(Family family)
{

  if (m_family == NOTSET && family != NOTSET) {
    m_family = family;
    if (family == VIRTEX6) {
      fprintf(stdout, " NOTE: Design is series 6.\n");
    }
    if (family == KINTEX7) {
      fprintf(stdout, " NOTE: Design is series 7.\n");
    }
  }
  if (family != m_family && family != NOTSET) {
    fprintf(stderr, " ERROR: Cannot change design family!\n");
    exit(1);
  }
  return 0;
}

Family Netlister::getFamily()
{
  return m_family;

}

unsigned int Netlister::setPart(Part part)
{

  if (m_part == NoPart && part != NoPart) {
    m_part = part;
  }
  if (part != m_part && part != NoPart) {
    fprintf(stderr, " ERROR: Cannot change design part!\n");
    exit(1);
  }
  return 0;
}

Part Netlister::getPart()
{
  return m_part;

}

unsigned int setValue(Bit* b, char* v) {
  switch (v[0]) {
  case 'z' :
    b->setZ();
    break;
  case '0' :
    b->setValue(0);
    break;
  case '1' :
    b->setValue(1);
    break;
  default:
    fprintf(stderr, "Unknown bit value: '%s'!\n", v);
    exit(1);
  }
  return 0;
}

unsigned int setKind(Bit* b, char* k) {
  switch (k[0]) {
  case '?' :
    b->setKind(Bit::UNAVAILABLE);
    break;
  case 'C' :
    b->setKind(Bit::CNST);
    break;
  case 'U' :
    b->setKind(Bit::UNREAD);
    break;
  default:
    fprintf(stderr, "Unknown bit kind: '%s'!\n", k);
    exit(1);
  }
  return 0;
}

unsigned int setKind(Signal* signal, char* k) {
  switch (k[0]) {
  case 'I' :
    signal->setKind(dIn);
    break;
  case 'O' :
    signal->setKind(dOut);
    break;
  case 'W' :
    signal->setKind(dWire);
    break;
  default:
    fprintf(stderr, "Unknown signal kind: '%s'!\n", k);
    exit(1);
  }
  return 0;
}

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

unsigned int Netlister::parse_xrf(std::string file)
{

  gzs::igzstream xrfFile;
  //  std::ifstream  xrfFile;
  xrfFile.open (file.c_str());
  std::string one_line;
  EdifView view = RTL;
  std::vector<Bit*> bit_vector;
  std::vector<Net*> net_vector;

  if (xrfFile.is_open()) {

    fprintf(stdout, "Parsing xrf file: %s ...\n", file.c_str());

    int matches;
    unsigned int id, net_id, addr, offset, slr, lsb, msb, bit_id, ignore, count, version;
    int lsb_signed, msb_signed;
    char kind[128], value[128];
    char vis[128], name[2048], defname[2048];
    version = 0;
    while (!xrfFile.eof())
	{

	  std::getline(xrfFile, one_line);
	  one_line.erase(0, one_line.find_first_not_of(" \n\r\t"));

	  if (one_line.length() == 0) {
	    continue;
	  }

	  matches = sscanf (one_line.c_str(), "(BIT %d %s %s %d %d %d)", &id, (char *) kind, (char *) value, &addr, &offset, &slr);
	  if (matches == 5 || matches == 6) {
	    Bit* b = new Bit(id);
	    b->setKind(Bit::STATE);
	    b->setAddr(addr);
	    b->setOffset(offset);
	    if (matches == 6) {
	      b->setSlr(slr);
	    }
	    setValue(b, (char*) value);
	    bit_vector[id] = b;
	    continue;
	  }
	  if (matches == 3) {
	    Bit* b = new Bit(id);
	    setValue(b, (char*) value);
	    setKind(b, (char*) kind);
	    bit_vector[id] = b;
	    continue;
	  }
	  matches = sscanf (one_line.c_str(), "(SIG %s %s %s %d %d %d %d %d %d", (char *) vis, (char*) kind, (char *) name, &id, &net_id, &lsb_signed, &msb_signed, &bit_id, &ignore);
	  if (matches == 9) {
	    if ((lsb_signed < 0) || (msb_signed < 0)) {
	      fprintf(stderr, "WARNING: negative index: %s [%d:%d]\n", name, msb_signed, lsb_signed);
	    }
	    lsb = (lsb_signed < 0) ? 0 : lsb_signed;
	    msb = (msb_signed < 0) ? 0 : msb_signed;

	    Module * parent = m_parents.front();
	    Signal*  signal = new Signal(parent, name, id, lsb, msb, true);
	    setKind(signal, (char*) kind);
	    parent->addSignal(signal);
	    registerSignal(view, signal);
	   
	    std::stringstream ss(one_line);
	    std::istream_iterator<std::string> begin(ss);
	    std::istream_iterator<std::string> end;
	    std::vector<std::string> vstrings(begin, end);

	    // trim off everything but the list of bits
	    vstrings.erase(vstrings.begin(), vstrings.begin()+8);
	    
	    unsigned int i = lsb;
	    for (std::vector<std::string>::iterator it =  vstrings.begin();
		 it != vstrings.end(); it++)
	      {
		std::string s = *it;
		std::replace(s.begin(), s.end(), ')', ' ');
		int n = 0;
		std::istringstream(s) >> n;

		Bit* b = bit_vector[n];
		BitRef* br = new BitRef(signal, lsb);
		br->setBit(b);
		signal->setBitRef(i, br);
		i++;
	      }
	    if (vis[0] == 'H') {
	      signal->setHidden();
	    }
	    Net* net = net_vector[net_id];
	    net->addSignal(signal);

	    unsigned int n = std::count(one_line.begin(), one_line.end(), ')');
	    n = n - 1;
	    while (n != 0) {
	      m_parents.pop_front();
	      n--;
	    }
	    continue;
	  }
	  matches = sscanf (one_line.c_str(), "(SIG %s %s %s %d %d %d %d %d", (char *) vis, (char*) kind, (char *) name, &id, &net_id, &lsb_signed, &msb_signed, &bit_id);
	  if (matches == 8) {
	    if ((lsb_signed < 0) || (msb_signed < 0)) {
	      fprintf(stderr, "WARNING: negative index: %s [%d:%d]\n", name, msb_signed, lsb_signed);
	    }
	    lsb = (lsb_signed < 0) ? 0 : lsb_signed;
	    msb = (msb_signed < 0) ? 0 : msb_signed;

	    if (lsb != msb) {
	      fprintf(stderr, "Illegal signal: %s\n", one_line.c_str());
	      exit(1);
	    }
	    Module * parent = m_parents.front();
	    Signal*  signal = new Signal(parent, name, id, lsb, msb, true);
	    setKind(signal, (char*) kind);
	    parent->addSignal(signal);
	    registerSignal(view, signal);

	    Bit* b = bit_vector[bit_id];
	    BitRef* br = new BitRef(signal, lsb);
	    br->setBit(b);
	    signal->setBitRef(lsb, br);
	    if (vis[0] == 'H') {
	      signal->setHidden();
	    }
	    Net* net = net_vector[net_id];
	    net->addSignal(signal);

	    unsigned int n = std::count(one_line.begin(), one_line.end(), ')');
	    n = n - 1;
	    while (n != 0) {
	      m_parents.pop_front();
	      n--;
	    }
	    continue;
	  }
	  matches = sscanf (one_line.c_str(), "(MOD %s %s %s %d", (char *) vis, (char *) defname, (char *) name, &id);
	  if (matches == 4) {
	    Module * mod = new Module((char *) name, defname, id);
	    Module * parent = m_parents.front();
	    if (m_parents.size() == 1) {
	      m_top[view] = mod;
	    }
	    parent->addChild(mod);
	    registerModule(view, mod);
	    m_parents.push_front(mod);
	    if (vis[0] == 'H') {
	      mod->setHidden();
	    }
	    continue;
	  }
	  matches = sscanf (one_line.c_str(), "(VIEW %s)", (char *) kind);
	  if (matches == 1) {
	    switch (kind[0]) {
	    case 'R' :
	      view = RTL;
	      break;
	    case 'S' :
	      view = SYNTH;
	      break;
	    default:
	      fprintf(stderr, "Unknown view type: '%s'!\n", (char*) kind);
	      exit(1);
	    }
	    Module * mod_root = new Module((char *) "", (char *) "", 0);
	    registerModule(view, mod_root);
	    m_parents.clear();
	    m_parents.push_front(mod_root);
	    continue;
	  }
	  matches = sscanf (one_line.c_str(), "(VERSION %d)", &version);
	  if (matches == 1) {
	    continue;
	  }
	  matches = sscanf (one_line.c_str(), "(FAMILY %s)", (char *) kind);
	  if (matches == 1) {
	    switch (kind[0]) {
	    case 'V' :
	      setFamily(VIRTEX6);
	      break;
	    case 'K' :
	      setFamily(KINTEX7);
	      break;
	    default:
	      fprintf(stderr, "Unknown family: '%s'!\n", (char*) kind);
	      exit(1);
	    }
	    continue;
	  }
	  matches = sscanf (one_line.c_str(), "(BITS %d)", &count);
	  if (matches == 1) {
	    bit_vector.resize(count);
	    continue;
	  }
	  matches = sscanf (one_line.c_str(), "(NETS %d)", &count);
	  if (matches == 1) {
	    net_vector.resize(count);
	    for (unsigned int i = 0; i < count; i++) {
	      net_vector[i] = new Net();
	    }
	    continue;
	  }
	  fprintf(stderr, "Unexpected syntax encountered.\n %s\n", one_line.c_str());
	  exit(1);
	}

    xrfFile.close();

    if (version != utils::getVersion()) {
      fprintf(stderr, "WARNING: The .xrf file format version (%d) and sofware format version (%d) do not match.\n", version,  utils::getVersion());
    }

    Module* root_rtl = findModule(RTL, "");
    root_rtl->updateVisibleChildren(true);
    root_rtl->updateVisibleSignals(true);

    Module* root_synth = findModule(SYNTH, "");
    root_synth->updateVisibleChildren(true);
    root_synth->updateVisibleSignals(true);
      
    fprintf(stdout, "Parsing xrf file: %s complete.\n\n", file.c_str());
    return 0;

  } else {
    std::cerr << "Can't open file " << file << std::endl;
    exit(1);
  }
}

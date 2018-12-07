
#include <string.h>
#include "CktMod.h"
#include "EditCompilePartition.h"

unsigned int CktMod::s_nextkey = 0;
unsigned int CktMod::s_nextUniqueId = 0;
unsigned int ScanData::s_nextid = 0;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

// Port names are in a separate scope use a new look up
const BString & ScanData::genPortName (const BString & portName, bool broadcast, unsigned int width)
{
  BString actualName(portName);
  // TODO XXX check the portName is unique in the port list

  m_addedPorts.push_back (PushedPort(portName, actualName, broadcast, width));
  return m_addedPorts.back().m_uniqPortName;
}


const unsigned ScanData::getLength() 
{ 

  if (m_path) {
    return m_path->Length(true) + (m_path->Length(false) * m_width);
  }
  return 0;
}

bool ScanData::isFirstScan()
{
  bool first = !m_scan_inst_found;
  m_scan_inst_found = true;
  return first;
}

const BString & ScanData::mkProbeId (const BString & root)
{
  BString* id = new BString(root);
  char suffix[32];
  snprintf(suffix, 32, "_%d", m_probe);
  (*id) += suffix;
  return (*id);
}

const BString & ScanData::mkChainId (const BString & root)
{
  BString* id = new BString(root);
  char suffix[32];
  snprintf(suffix, 32, "_%d", m_chain);
  (*id) += suffix;
  return (*id);
}

const BString & ScanData::mkId (const BString & root)
{
  BString* id = new BString(root);
  char suffix[32];
  snprintf(suffix, 32, "_%d_%d", m_probe, m_chain);
  (*id) += suffix;
  return (*id);
}

void CktMod::getSignalTypeName (SignalType sig_type, BString &ret) {
  int first = 1;
  if (sig_type == CktModAny) {
    ret = "Any";
    return;
  }
  if (sig_type & CktModFlop) {
    ret += "Flop";
    first = 0;
  }
  if (sig_type & CktModReg) {
    if (first == 0)
      ret += " ";
    ret += "Reg";
    first = 0;
  }
  if (sig_type & CktModInput) {
    if (first == 0)
      ret += " ";
    ret += "Input";
      first = 0;
  }
  if (sig_type & CktModOutput) {
    if (first == 0)
      ret += " ";
    ret += "Output";
    first = 0;
  }
  return;
}

Partition::Partition (const BString &boardspec, const BString &modspec)
  : CktMod("/")
  , m_boardspec(boardspec)
  , m_modspec(modspec)
{
  m_partition_spec = new PartitionSpec();
}

GenSceMi::GenSceMi (const instHandle & ih, int phase, const BString &modname,
		    const BString &newmodname, const BString &dir,
		    netCollection &clk_rst_sigs)
  : CktMod(ih)
  , m_phase(phase)
  , m_module(modname)
  , m_new_mod_name(newmodname)
  , m_output_dir(dir)
  , m_clk_rst_signals(clk_rst_sigs)
{
}

size_t match_name(BString &portName, BString &specName)
{
  size_t pos;

  if (portName == specName)
    return 0;

  pos = specName.find_first_of('*');
  if (pos == std::string::npos)
    return pos;

  if (!strncmp(portName.c_str(), specName.c_str(), pos))
    return pos;

  return std::string::npos;
}


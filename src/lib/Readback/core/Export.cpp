#include "Export.hpp"
#include <iostream>
#include <fstream>
#include <gzstream.hpp>

unsigned int Export::exportDesign(Family family, Module* mod_rtl, Module* mod_synth, std::string file, bool include_hidden)
{
  gzs::ogzstream exportFile;
  //  std::ofstream exportFile;
  exportFile.open (file.c_str());

  if (exportFile.is_open()) {

    bool show_hidden = Module::m_show_hidden;
    Module::m_show_hidden = include_hidden;

    fprintf(stdout, "Exporting design to: %s ...\n", file.c_str());
    if (include_hidden) {
      fprintf(stdout, "NOTE: Hidden signals and modules ARE being exported.\n");
    } else {
      fprintf(stdout, "NOTE: Hidden signals and modules are NOT being exported.\n");
    }

    exportFile << "(VERSION " << utils::getVersion() << ")" << std::endl;

    bool family_found = false;
    if (family == VIRTEX6) {
      exportFile << "(FAMILY VIRTEX6)" << std::endl;
      family_found = true;
    } 
    if (family == KINTEX7) {
      exportFile << "(FAMILY KINTEX7)" << std::endl;
      family_found = true;
    }
    if (family == NOTSET) {
      fprintf(stderr, "ERROR: No design has been loaded. Cannot export.\n");
      exit(1);
    }
    if (!family_found) {
      fprintf(stderr, "ERROR: Design has unknown family. Cannot export.\n");
      exit(1);
    }

    exportFile << "(BITS " <<  (Bit::getMaxBitId() + 1) << ")" << std::endl;
    exportFile << "(NETS " <<  Net::m_all_nets.size() << ")" << std::endl;

    for (std::set<Bit *>::iterator it = Bit::m_all_bits.begin();
	 it != Bit::m_all_bits.end(); it++)
      {
	Bit* b = *it;
	exportFile << b << std::endl;
      }

    exportFile << std::endl;

    exportFile << "(VIEW RTL)" << std::endl;

    exportFile << std::endl;

    exportFile << mod_rtl;

    exportFile << std::endl;

    exportFile << "(VIEW SYNTH)" << std::endl;

    exportFile << std::endl;

    exportFile << mod_synth;

    exportFile.close();
    fprintf(stdout, "Exporting design to: %s complete.\n\n", file.c_str());
    Module::m_show_hidden = show_hidden;
    return 0;

  } else {
    std::cerr << "Can't open file " << file << std::endl;
    exit(1);
  }
}


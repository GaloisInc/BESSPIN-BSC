
#include "V.h"
#include "ModuleDesc.h"
#include <boost/regex.hpp>

void       addStateChain(Verific::VeriModule* module, std::string* prefix, boost::regex filter, ScanPath* path, unsigned* includes_all, int width);
void       addSampleReg(Verific::VeriModule* module, Verific::VeriIdDef* signal, Verific::VeriIdDef** scan_internal,  Verific::VeriIdDef* scan_mode, Verific::VeriIdDef* scan_length, const char* name, int width);

Verific::VeriIdDef* getScanAny(Verific::VeriModule* module);
void connectScanOuput(Verific::VeriModule* module, Verific::VeriIdDef* scan_internal,  Verific::VeriIdDef* scan_out);

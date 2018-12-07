
#include "GenModule.h"
#include <string.h>
#include <set>
#include <list>
#include <vector>
#include <algorithm>


tPath GenModule::toPath(std::string full_name)
{
  tPath path;

  if (full_name.length() == 0) return path;

  std::string::size_type pos = full_name.find("/", 0 );

  if( pos == std::string::npos ) {
    path.push_front(full_name);
    return path;
  } 

  if (pos == 0) {
    full_name.erase(0,1);
    return GenModule::toPath(full_name);
  }

  std::string name = full_name.substr(0,pos);
  full_name.erase(0, pos);
  path = GenModule::toPath(full_name);
  path.push_front(name);

  return path;
}

std::string GenModule::fromPath(tPath & path)
{

  std::string name = "";
  
  if (path.size() == 0) return name;

  foreachInStringList(&path, part) {
    name += "/";
    name += *part;
  }

  return name;

}

VHandle* GenModule::getSignal(VHandle module, std::string name)
{

  vpiHandle iter;
  vpiHandle up_ihref = module.get();

  if ((iter = vpi_iterate(vpiReg, up_ihref)) != NULL) {
    for (;;)
      {
	vpiHandle ihref = vpi_scan(iter);
	if (ihref == NULL) break;
		// printf("C3 %s\n", vpi_get_str(vpiName, ihref));
	if (!strcmp(vpi_get_str(vpiName, ihref), name.c_str())) {
	  // printf("CFOUND %s\n", name.c_str());
	  return new VHandle(ihref);
	}
		// printf("C4\n");
      }
  }
  if ((iter = vpi_iterate(vpiNet, up_ihref)) != NULL) {
    for (;;)
      {
	vpiHandle ihref = vpi_scan(iter);
	if (ihref == NULL) break;
	 // printf("C3 %s\n", vpi_get_str(vpiName, ihref));
	if (!strcmp(vpi_get_str(vpiName, ihref), name.c_str())) {
	  // printf("CFOUND %s\n", name.c_str());
	  return new VHandle(ihref);
	}
	 // printf("C4\n");
      }
  }
  if ((iter = vpi_iterate(vpiNetArray, up_ihref)) != NULL) {
    for (;;)
      {
	vpiHandle ihref = vpi_scan(iter);
	if (ihref == NULL) break;
	// printf("C3 %s\n", vpi_get_str(vpiName, ihref));
	if (!strcmp(vpi_get_str(vpiName, ihref), name.c_str())) {
	  // printf("CFOUND %s\n", name.c_str());
	  return new VHandle(ihref);
	}
	// printf("C4\n");
      }
  }
  if ((iter = vpi_iterate(vpiRegArray, up_ihref)) != NULL) {
    for (;;)
      {
	vpiHandle ihref = vpi_scan(iter);
	if (ihref == NULL) break;
	// printf("C3 %s\n", vpi_get_str(vpiName, ihref));
	if (!strcmp(vpi_get_str(vpiName, ihref), name.c_str())) {
	  // printf("CFOUND %s\n", name.c_str());
	  return new VHandle(ihref);
	}
	// printf("C4\n");
      }
  }
  if ((iter = vpi_iterate(vpiMemory, up_ihref)) != NULL) {
    for (;;)
      {
	vpiHandle ihref = vpi_scan(iter);
	if (ihref == NULL) break;
	// printf("C3 %s\n", vpi_get_str(vpiName, ihref));
	if (!strcmp(vpi_get_str(vpiName, ihref), name.c_str())) {
	  // printf("CFOUND %s\n", name.c_str());
	  vpiHandle xiter;
	  if ((xiter = vpi_iterate(vpiMemoryWord, ihref)) != NULL) {
	    for (;;)
	      {
		vpiHandle xihref = vpi_scan(xiter);
		if (xihref == NULL) break;
		// printf("D3 %s\n", vpi_get_str(vpiName, xihref));
		if (!strcmp(vpi_get_str(vpiName, xihref), name.c_str())) {
		  // printf("DFOUND %s\n", name.c_str());
		}
	      
	      }
	  }
	  return new VHandle(ihref);
	}
	// printf("C4\n");
      }
  }
  if ((iter = vpi_iterate(vpiMemoryWord, up_ihref)) != NULL) {
    for (;;)
      {
	vpiHandle ihref = vpi_scan(iter);
	if (ihref == NULL) break;
	// printf("D3 %s\n", vpi_get_str(vpiName, ihref));
	if (!strcmp(vpi_get_str(vpiName, ihref), name.c_str())) {
	  // printf("CFOUND %s\n", name.c_str());
	  return new VHandle(ihref);
	}
	// printf("C4\n");
      }
  }


  // printf("DIDNT FIND %s\n", name.c_str());
  return NULL;
}

VHandle* GenModule::getSignal(VHandle module, std::string name, unsigned index)
{
  vpiHandle iter;
  vpiHandle up_ihref = module.get();

  if ((iter = vpi_iterate(vpiMemory, up_ihref)) != NULL) {
    for (;;)
      {
	vpiHandle ihref = vpi_scan(iter);
	if (ihref == NULL) break;
//	printf("I3 %s\n", vpi_get_str(vpiName, ihref));
	if (!strcmp(vpi_get_str(vpiName, ihref), name.c_str())) {
//	  printf("IFOUND %s %d\n", name.c_str(), index);
	  vpiHandle xx = vpi_handle_by_index(ihref, index);
	  return new VHandle(xx);
	}
//	printf("I4\n");
      }
  }

  printf("DIDNT FIND %s\n", name.c_str());
  return NULL;

}

//  /* first all instance regs, wires, and variables */ 
//  iter = vpi_iterate(vpiNet, ihref);
//  if (iter != NULL) setup_1iter_chgcbs(iter);
//  iter = vpi_iterate(vpiReg, ihref);
//  if (iter != NULL) setup_1iter_chgcbs(iter);
//  iter = vpi_iterate(vpiVariables, ihref);
//  if (iter != NULL) setup_1iter_chgcbs(iter);

VHandle* GenModule::getInst(VHandle module, tPath path)
{

  if (path.size() == 0) return new VHandle(module);

  vpiHandle iter;

  vpiHandle up_ihref = module.get();

  if ((iter = vpi_iterate(vpiModule, up_ihref)) == NULL) return NULL;
  for (;;)
    {
      vpiHandle ihref = vpi_scan(iter);
      if (ihref == NULL) break;
      if (!strcmp(vpi_get_str(vpiName, ihref), path.front().c_str())) {
	path.pop_front();
	VHandle h = ihref;
	return GenModule::getInst(h, path);
      }
    }
  return NULL;
}

VHandle* GenModule::getInst(VHandle module, std::string full_name)
{

  tPath path = toPath(full_name);

  return GenModule::getInst(module, path);

}



VHandle* GenModule::getInst(std::string full_name)
{

  tPath path = toPath(full_name);

  return GenModule::getInst(path);

}

VHandle* GenModule::getInst(tPath path)
{

//  printf("Path is: %s\n", ToString(path).c_str());

  BCModule & mod = m_cosim.getModule(); // Get top cosim module;

  VHandle & handle = mod.getModule() ;
  return GenModule::getInst(handle, path);

}

void GenModule::initializeAllStateValues(VHandle module)
{

  GenModule::initializeStateValues(module);

  vpiHandle iter;
  vpiHandle up_ihref = module.get();

  if ((iter = vpi_iterate(vpiModule, up_ihref)) == NULL) return;
  for (;;)
    {
      vpiHandle ihref = vpi_scan(iter);
      if (ihref == NULL) break;
      VHandle h = ihref;
      GenModule::initializeAllStateValues(h);
    }
}

void GenModule::initializeStateValues(VHandle module)
{

  vpiHandle iter;
  vpiHandle up_ihref = module.get();

  static char val_str[1024];

  if ((iter = vpi_iterate(vpiReg, up_ihref)) != NULL) {
    for (;;)
      {
	vpiHandle ihref = vpi_scan(iter);
	if (ihref == NULL) break;

	static s_vpi_value val;
	strcpy(val_str, "X");
	val.format = vpiBinStrVal;
	val.value.str = val_str;
	vpi_put_value (ihref, &val, NULL, vpiNoDelay);

	// printf("REG %s\n", vpi_get_str(vpiName, ihref));
      }
  }
  if ((iter = vpi_iterate(vpiRegArray, up_ihref)) != NULL) {
    for (;;)
      {
	vpiHandle ihref = vpi_scan(iter);
	if (ihref == NULL) break;

	static s_vpi_value val;
	strcpy(val_str, "X");
	val.format = vpiBinStrVal;
	val.value.str = val_str;
	vpi_put_value (ihref, &val, NULL, vpiNoDelay);

	// printf("REGARRAY %s\n", vpi_get_str(vpiName, ihref));
      }
  }
  if ((iter = vpi_iterate(vpiMemory, up_ihref)) != NULL) {
    for (;;)
      {
	vpiHandle ihref = vpi_scan(iter);
	if (ihref == NULL) break;
	vpiHandle xiter;
	if ((xiter = vpi_iterate(vpiMemoryWord, ihref)) != NULL) {
	  for (;;)
	    {
	      vpiHandle xihref = vpi_scan(xiter);
	      if (xihref == NULL) break;

	      static s_vpi_value val;
	      strcpy(val_str, "X");
	      val.format = vpiBinStrVal;
	      val.value.str = val_str;
	      vpi_put_value (ihref, &val, NULL, vpiNoDelay);

	      // printf("MEMWORD0 %s\n", vpi_get_str(vpiName, xihref));
	    }
	}
      }
  }
  if ((iter = vpi_iterate(vpiMemoryWord, up_ihref)) != NULL) {
    for (;;)
      {
	vpiHandle ihref = vpi_scan(iter);
	if (ihref == NULL) break;

	static s_vpi_value val;
	strcpy(val_str, "X");
	val.format = vpiBinStrVal;
	val.value.str = val_str;
	vpi_put_value (ihref, &val, NULL, vpiNoDelay);

	// printf("MEMWORD1 %s\n", vpi_get_str(vpiName, ihref));
      }
  }
}

void GenModule::displayAllValues(VHandle module, std::string prefix)
{

  GenModule::displayValues(module, prefix);

  vpiHandle iter;
  vpiHandle up_ihref = module.get();

  if ((iter = vpi_iterate(vpiModule, up_ihref)) == NULL) return;
  for (;;)
    {
      vpiHandle ihref = vpi_scan(iter);
      if (ihref == NULL) break;
      char* name = vpi_get_str(vpiName, ihref);
      VHandle h = ihref;
      std::string full = prefix;
      full += "/";
      full += name;
      GenModule::displayAllValues(h, full);
    }
}


bool mycompare(std::pair<std::string, vpiHandle> a, std::pair<std::string, vpiHandle> b)
{
  bool value = a.first.compare(b.first) < 0;
//  printf("CMP: %s %s %d\n", a.first.c_str(), b.first.c_str(), value);
  return(value);
}


void GenModule::displayValues(VHandle module, std::string prefix)
{

  vpiHandle iter;
  vpiHandle up_ihref = module.get();

//  static char val_str[1024];
  s_vpi_value tmpval;

  std::list<std::pair<std::string, vpiHandle> > handles;
  std::list<std::pair<std::string, vpiHandle> >::iterator it;

  if ((iter = vpi_iterate(vpiReg, up_ihref)) != NULL) {
    for (;;)
      {
	vpiHandle ihref = vpi_scan(iter);
	if (ihref == NULL) break;
	handles.push_back(std::pair<std::string, vpiHandle> (vpi_get_str(vpiName, ihref), ihref));
      }
  }

  handles.sort(mycompare);

  for (it=handles.begin(); it!=handles.end(); ++it)
    {
      pair<std::string, vpiHandle> p = *it;
      vpiHandle ihref = p.second;
      tmpval.format = vpiBinStrVal; 
      vpi_get_value(ihref, &tmpval);
      printf("REG %s/%s = %s\n", prefix.c_str(), vpi_get_str(vpiName, ihref),  tmpval.value.str);

    }

 //  if ((iter = vpi_iterate(vpiReg, up_ihref)) != NULL) {
//     for (;;)
//       {
// 	vpiHandle ihref = vpi_scan(iter);
// 	if (ihref == NULL) break;

// 	  tmpval.format = vpiBinStrVal; 
// 	  vpi_get_value(ihref, &tmpval);

// // 	static s_vpi_value val;
// // 	strcpy(val_str, "X");
// // 	val.format = vpiBinStrVal;
// // 	val.value.str = val_str;
// // 	vpi_put_value (ihref, &val, NULL, vpiNoDelay);

// 	printf("REG %s/%s = %s\n", prefix.c_str(), vpi_get_str(vpiName, ihref),  tmpval.value.str);
//       }
//   }
//   if ((iter = vpi_iterate(vpiRegArray, up_ihref)) != NULL) {
//     for (;;)
//       {
// 	vpiHandle ihref = vpi_scan(iter);
// 	if (ihref == NULL) break;

// 	static s_vpi_value val;
// 	strcpy(val_str, "X");
// 	val.format = vpiBinStrVal;
// 	val.value.str = val_str;
// 	vpi_put_value (ihref, &val, NULL, vpiNoDelay);

// 	// printf("REGARRAY %s\n", vpi_get_str(vpiName, ihref));
//       }
//   }
//   if ((iter = vpi_iterate(vpiMemory, up_ihref)) != NULL) {
//     for (;;)
//       {
// 	vpiHandle ihref = vpi_scan(iter);
// 	if (ihref == NULL) break;
// 	vpiHandle xiter;
// 	if ((xiter = vpi_iterate(vpiMemoryWord, ihref)) != NULL) {
// 	  for (;;)
// 	    {
// 	      vpiHandle xihref = vpi_scan(xiter);
// 	      if (xihref == NULL) break;

// 	      static s_vpi_value val;
// 	      strcpy(val_str, "X");
// 	      val.format = vpiBinStrVal;
// 	      val.value.str = val_str;
// 	      vpi_put_value (ihref, &val, NULL, vpiNoDelay);

// 	      // printf("MEMWORD0 %s\n", vpi_get_str(vpiName, xihref));
// 	    }
// 	}
//       }
//   }
//   if ((iter = vpi_iterate(vpiMemoryWord, up_ihref)) != NULL) {
//     for (;;)
//       {
// 	vpiHandle ihref = vpi_scan(iter);
// 	if (ihref == NULL) break;

// 	static s_vpi_value val;
// 	strcpy(val_str, "X");
// 	val.format = vpiBinStrVal;
// 	val.value.str = val_str;
// 	vpi_put_value (ihref, &val, NULL, vpiNoDelay);

// 	// printf("MEMWORD1 %s\n", vpi_get_str(vpiName, ihref));
//       }
//   }
}

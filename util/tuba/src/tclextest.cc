#include "Tclparser.h"
#include <stdio.h>
#include <stdlib.h>
#include "Tstring.h"


main(int argc, char **argv)
{
  FILE *fp;
  Tstring token;
  int lineno;
  long file_size;
  size_t rc;
  
  if(argc != 2) {
    fprintf(stderr, "syntax: %s tclfile\n", argv[0]);
    exit(1);
  }
  
  fp = fopen(argv[1], "r");
  if(fp == NULL) {
    fprintf(stderr, "can't open file '%s'\n", argv[1]);
    exit(1);
  }
  
  // get the size of the file
  fseek(fp, 0L, SEEK_END);
  file_size = ftell(fp);
  rewind(fp);
  
  // allocate a buffer
  char* buffer = new char[file_size+1];
  
  // read in the file
  if((rc = fread(buffer, 1, file_size, fp)) != file_size) {
    fprintf(stderr, "could only read %d bytes of %d\n", rc, file_size);
    fclose(fp);
    exit(1);
  }
  else {
    buffer[rc] = '\0';
  }

  fclose(fp);
  
  // create the parser object
  Tclparser tp(buffer, 1);
  
  while((token = tp.gettok()) != geteof()) {
    if(token == geteos()) {
      continue;
    }
    
    
    printf("Line %d: token = '%s'\n", tp.getlineno(), token.data());
    if(token == "proc") {
      printf("proc name is '%s'\n", tp.gettok().data());
      printf("args are '%s'\n", tp.gettok().data());
      Tclparser tptp(tp.gettok(), tp.getlineno());
      printf("block characters are '%c' and '%c'\n", tptp.getBlockStart(),
        tptp.getBlockEnd());
      while((token = tptp.gettok()) != geteof()) {
        printf("line %d: token = '%s'\n", tptp.getlineno(), token.data());
      }
    }
  }
  
  delete buffer;
}

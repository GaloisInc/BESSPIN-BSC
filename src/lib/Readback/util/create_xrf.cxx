# include <stdio.h>
# include "Design.hpp"

int main(int argc, char *argv[])
{
  if (argc != 3) {
    printf("usage: %s <dir> <topmod>\n", argv[0]);
    return 1;
  }
   
  char *dir = argv[1];
  char *topmod = argv[2];

  size_t pathlen = strlen(dir) + 1 + strlen(topmod) + 5 + 1;

  // .ll
  char *llpath = (char *) malloc(pathlen);
  if (llpath == NULL) {
    printf("%s: malloc failure\n", argv[0]);
    return(1);
  }
  sprintf(llpath, "%s/%s.ll", dir, topmod);
  
  // .rtl
  char *rtlpath = (char *) malloc(pathlen);
  if (rtlpath == NULL) {
    printf("%s: malloc failure\n", argv[0]);
    return(1);
  }
  sprintf(rtlpath, "%s/%s.rtl", dir, topmod);

  // .edf
  char *synpath = (char *) malloc(pathlen);
  if (synpath == NULL) {
    printf("%s: malloc failure\n", argv[0]);
    return(1);
  }
  sprintf(synpath, "%s/%s.edf", dir, topmod);

  // .slog
  char *slogpath = (char *) malloc(pathlen);
  if (slogpath == NULL) {
    printf("%s: malloc failure\n", argv[0]);
    return(1);
  }
  sprintf(slogpath, "%s/%s.slog", dir, topmod);

  // .xrf
  char *xrfpath = (char *) malloc(pathlen);
  if (xrfpath == NULL) {
    printf("%s: malloc failure\n", argv[0]);
    return(1);
  }
  sprintf(xrfpath, "%s/%s.xrf", dir, topmod);

  bool skip_vcd = true;
  bool include_hidden = false;

  Design *design = new Design();

  int ret = 0;

  // load the input files
  //
  if (ret == 0)  ret = design->parse_rtl(rtlpath);
  if (ret == 0)  ret = design->parse_synth(synpath);
  if (ret == 0)  ret = design->parse_ll(llpath);
  if (ret == 0)  ret = design->parse_log(slogpath, skip_vcd);
  if (ret == 0)  ret = design->syncConfig();

  // export the xrf file
  //
  if (ret == 0)  ret = design->exportDesign(xrfpath, include_hidden);
  if (ret == 0)  printf("Generated file %s\n", xrfpath);

  return(ret);
}
    

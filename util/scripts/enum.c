
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv)
	{

	  char* name = argv[0];

	  if (argc != 2) {

	    printf(
		   "Usage: %s label_string\n"
		   "       %s is called with exactly one argument (i.e. %s \"A B C\").\n", name, name, name);
	    return(0);
	  }

	  char* labels = argv[1];
	  int len = strlen(labels);
	  int OS;
	  int gap;
	  int loop;

	  OS = 0;
	  gap = 1;

	  for (loop = 0; loop < len; loop++){
	    if ( labels[loop] == ' '){
	      gap = 1;
	    }
	    else {
	      if (gap == 1) {
		gap = 0;
		OS++;
	      }
	    }
	  }

	  char* output[OS];

	  OS = 0;
	  gap = 1;

	  for (loop = 0; loop < len; loop++){
	    if ( labels[loop] == ' '){
	      if(!gap) {
		labels[loop] = 0;
	      }
	      gap = 1;
	    }
	    else {
	      if (gap == 1) {
	      output[OS] = 0;
	      output[OS] = &labels[loop];
	      gap = 0;
	      OS++;
	      }
	    }
	  }

	  unsigned int count = 0;
	  while(1)
	    {
	      char buf[1025];
	
	      buf[0] = 0;
	      fscanf(stdin, "%s", buf);
	      if(buf[0]) {
		
		count = 0;
		int hx;
		sscanf(buf, "%x", &hx);

		if (hx > (OS - 1)) {
		  printf("UNDEF(%x)\n", hx);
		} else if (hx < 0) {
		  printf("UNDEF(%x)\n", hx);
		} else {
		  printf("%s\n", output[hx]);
		}
		fflush(stdout);
	      } else {
		if (count == 100) {
                  /* exit if orphaned process */
		  if (getppid() == 1) return(0);
		  sleep(0.01);
		} else {
		  count++;
		}
	      }
	    }

	  return(0);
	}

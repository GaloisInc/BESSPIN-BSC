RM=rm
EXEC=estimator

CFLAGS=-Wall -g
# CFLAGS=-Wall -O2

# Link math lib for round() and other functions
LINKFLAGS= -lm

.PHONY: all run clean realclean

OBJECTS= utils.o  parser.o  graph.o  analysis.o  estimator.o

all: $(EXEC)

# ----------------

estimator: $(OBJECTS)
	$(CC) $(CFLAGS) $(LINKFLAGS) $(OBJECTS) -o estimator

estimator.o: utils.h  parser.h  estimator.c
	$(CC) $(CFLAGS) -c estimator.c

parser.o: utils.h  parser.h  parser.c
	$(CC) $(CFLAGS) -c parser.c

graph.o: utils.h  parser.h  graph.h  graph.c
	$(CC) $(CFLAGS) -c graph.c

analysis.o: utils.h  parser.h  graph.h  analysis.h  analysis.c
	$(CC) $(CFLAGS) -c analysis.c

utils.o: utils.h utils.c
	$(CC) $(CFLAGS) -c utils.c

# ----------------

clean:
	$(RM) -f $(EXEC)  *.o  *~  core*

realclean:
	make clean
	$(RM) -f  a.out

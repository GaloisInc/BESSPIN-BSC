
TARGETS=$(patsubst %.md,%,$(wildcard *.md))

default: all

all: $(TARGETS)

%: %.md
	pandoc -s -w man $< -o $@

clean:
	$(RM) $(TARGETS)

.PHONY: default all clean

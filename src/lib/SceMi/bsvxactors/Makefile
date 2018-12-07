
all:
	@echo "No default target"

ALLHEADERS = $(wildcard *.h)

# Static check of the header files before releasing them.
.PHONY: test-headers
test-headers:
	@echo "Checking header files for self consistency at: `date`"
	@$(foreach hf,$(ALLHEADERS), \
		$(RM) -f test_hdr.cxx && \
		echo "#include \"$(hf)\"" >> test_hdr.cxx  && \
		$(CXX) -I. -I../BlueNoC -c -o /dev/null test_hdr.cxx &&\
	) \
	echo "Done at: `date`"
	@-$(RM) -f test_hdr.cxx



.PHONY: clean
clean:
	@rm -f *.o *~

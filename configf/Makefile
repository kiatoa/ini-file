CSCOPTS= 

SRCFILES = common.scm configf.scm run_configf.scm struct-indexer.scm

OFILES   = $(SRCFILES:%.scm=%.o)

#csc -deploy $(CSCOPTS) $(OFILES) megatest.scm -o deploytarg

help: 
	@echo "Makefile targets"
	@echo "================"
	@echo "make test - run tests"
	@echo "make clean - clean up .o's and binaries"

test: $(OFILES)
	csc $(CSCOPTS) $(OFILES) -o run_configf
	./run_configf

%.o : %.scm
	csc $(CSCOPTS) -c $<

clean:
	rm -f *.o test-configf

GHC = ghc
HFLAGS = -package parsec -fglasgow-exts

OBJS = lisp.hs
PROGRAM = lisp

.PHONY: all clean

all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(GHC) $(HFLAGS) -o $(PROGRAM) $(OBJS)

clean:
	rm -rf *.o *.hi $(PROGRAM)

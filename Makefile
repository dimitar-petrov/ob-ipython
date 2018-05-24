TANGLE = tangle

ORG=$(wildcard *.org)

IPYTHON_SRC = $(patsubst %.org, %.ipython, $(ORG))
PYTHON_SRC  = $(patsubst %.org, %.py     , $(ORG))
PYTHON_OBJ  = $(patsubst %.org, %.pyc    , $(ORG))

all : test.py ob-ipython-client.elc

all-py-src: $(PYTHON_SRC)

%.ipython : %.org
	$(TANGLE) $<

%.sh : %.org
	$(TANGLE) $<

%.py : %.ipython
	sed  -e 's/^%/# %/' "$<" > "$@"

%.pyc : %.py
	python -m py_compile $<


%.elc : %.el
	emacsclient -e '(byte-compile-file "$^")'




.PHONY: all-py-src




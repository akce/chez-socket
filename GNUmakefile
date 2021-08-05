# chez-socket GNUmakefile.
# Written by Jerry 2019-2021.
# SPDX-License-Identifier: Unlicense

# Path to chez scheme executable.
SCHEME = /usr/bin/chez-scheme

# Install destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
LIBDIR = ~/lib/csv$(shell $(SCHEME) --version 2>&1)

# Scheme compile flags.
SFLAGS = -q

# Path to install executable.
INSTALL = /usr/bin/install

CFLAGS = -c -fpic $(CONFIG_H)

LIBFLAGS = -shared

## Should be no need to edit anything below here.

# This makefile assumes a library layout as follows:
# TOP
# PROJDIR/
#   FFI
#   SUBSRC ..
#   SUBOBJ ..
#   SUBWPO ..
#
# Where TOP is the high level library definition that imports all sub libs within PROJDIR.
# FFI (if needed) is a C compilable lib.
# The rest are scheme libraries.
# Scheme compilation is handled by building TOP and letting Chez scheme automatically manage dependants.

PROJDIR = socket

FFIOBJ = $(PROJDIR)/socket.o
FFILIB = $(PROJDIR)/libsocket.so

# Source files, shared objects, and whole program optimisations for the library subdirectory.
SUBSRC = $(addprefix $(PROJDIR)/,c.chezscheme.sls ftypes-util.chezscheme.sls impl.chezscheme.sls)
SUBOBJ = $(SUBSRC:.sls=.so)
SUBWPO = $(SUBSRC:.sls=.wpo)

# Top level (ie, root) library source, .. etc.
TOPSRC = $(addprefix $(PROJDIR)/,basic.sls extended.chezscheme.sls)
TOPOBJ = $(TOPSRC:.sls=.so)
TOPWPO = $(TOPSRC:.sls=.wpo)

# Installed versions of all the above.
IFFILIB = $(addprefix $(LIBDIR)/,$(FFILIB))
ISUBSRC = $(addprefix $(LIBDIR)/,$(SUBSRC))
ISUBOBJ = $(addprefix $(LIBDIR)/,$(SUBOBJ))
ISUBWPO = $(addprefix $(LIBDIR)/,$(SUBWPO))
ITOPSRC = $(addprefix $(LIBDIR)/,$(TOPSRC))
ITOPOBJ = $(addprefix $(LIBDIR)/,$(TOPOBJ))
ITOPWPO = $(addprefix $(LIBDIR)/,$(TOPWPO))

# Tell GNU make about the files generated as a "side-effect" of building TOPWPO,
# otherwise make will raise an error that it doesn't know how to build these.
.SECONDARY: $(SUBWPO) $(SUBOBJ) $(TOPOBJ)

# Default to just a local build.
all: build

$(FFILIB): $(FFIOBJ)
	$(CC) $(LIBFLAGS) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) $< -o $@

# Build target is structured so that the main wpo file is dependant on all scheme source files and triggers
# a Chez compile such that Chez rebuilds all dependancies on demand.
$(TOPWPO): $(TOPSRC) $(SUBSRC)
	echo '(reset-handler abort) (compile-imported-libraries #t) (generate-wpo-files #t) (library-directories ".") (compile-library "$(PROJDIR)/basic.sls")' | $(SCHEME) $(SFLAGS)
	echo '(reset-handler abort) (compile-imported-libraries #t) (generate-wpo-files #t) (library-directories ".") (compile-library "$(PROJDIR)/extended.chezscheme.sls")' | $(SCHEME) $(SFLAGS)

$(LIBDIR)/%: %
	$(INSTALL) -p -D "$<" "$@"

build: $(FFILIB) $(TOPWPO)

# install-ffi is always required, installations then need to decide what combination of src/so they want.
# Default install target is for everything.
install: install-ffi install-so install-src

install-ffi: $(IFFILIB)

install-so: $(ITOPWPO) $(ISUBWPO) $(ITOPOBJ) $(ISUBOBJ)

install-src: $(ITOPSRC) $(ISUBSRC)

clean:
	$(RM) $(FFIOBJ) $(FFILIB) $(TOPOBJ) $(TOPWPO) $(SUBOBJ) $(SUBWPO)

clean-install:
	$(RM) $(IFFIOBJ) $(IFFILIB) $(ITOPOBJ) $(ITOPWPO) $(ISUBOBJ) $(ISUBWPO) $(ITOPSRC) $(ISUBSRC)

clean-all: clean clean-install

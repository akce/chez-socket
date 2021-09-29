# chez-socket GNUmakefile.
# Written by Jerry 2019-2021.
# SPDX-License-Identifier: Unlicense

# Path to chez scheme executable.
SCHEME = /usr/bin/chez-scheme

# Install destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
LIBDIR = ~/lib/csv$(shell $(SCHEME) --version 2>&1)

# Install directory for SRFI-106 (basic socket) interface. This must be the top level directory *not* including
# the 'srfi' prefix.
SRFILIBDIR = $(LIBDIR)

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
SUBSRC = $(addprefix $(PROJDIR)/,bytevector.sls c.chezscheme.sls ftypes-util.chezscheme.sls impl.chezscheme.sls)
SUBOBJ = $(SUBSRC:.sls=.so)
SUBWPO = $(SUBSRC:.sls=.wpo)

# Top level (ie, root) library source, .. etc.
TOPSRC = socket.chezscheme.sls
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

# SRFI 106.

SRFIDIR = srfi
SRFI_TOPSRC = $(SRFIDIR)/\:106.chezscheme.sls
SRFI_TOPOBJ = $(SRFI_TOPSRC:.sls=.so)
SRFI_TOPWPO = $(SRFI_TOPSRC:.sls=.wpo)

SRFI_SUBSRC = $(SRFIDIR)/\:106/socket.sls
SRFI_SUBOBJ = $(SRFI_SUBSRC:.sls=.so)
SRFI_SUBWPO = $(SRFI_SUBSRC:.sls=.wpo)

ISRFI_TOPSRC = $(addprefix $(SRFILIBDIR)/,$(SRFI_TOPSRC))
ISRFI_TOPOBJ = $(addprefix $(SRFILIBDIR)/,$(SRFI_TOPOBJ))
ISRFI_TOPWPO = $(addprefix $(SRFILIBDIR)/,$(SRFI_TOPWPO))
ISRFI_SUBSRC = $(addprefix $(SRFILIBDIR)/,$(SRFI_SUBSRC))
ISRFI_SUBOBJ = $(addprefix $(SRFILIBDIR)/,$(SRFI_SUBOBJ))
ISRFI_SUBWPO = $(addprefix $(SRFILIBDIR)/,$(SRFI_SUBWPO))

# Tell GNU make about the files generated as a "side-effect" of building TOPWPO,
# otherwise make will raise an error that it doesn't know how to build these.
.SECONDARY: $(SUBWPO) $(SUBOBJ) $(TOPOBJ) $(SRFI_SUBWPO) $(SRFI_SUBOBJ) $(SRFI_TOPOBJ)

# Default to just a local build.
all: build

$(FFILIB): $(FFIOBJ)
	$(CC) $(LIBFLAGS) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) $< -o $@

# Build target is structured so that the main wpo file is dependant on all scheme source files and triggers
# a Chez compile such that Chez rebuilds all dependancies on demand.
$(TOPWPO): $(TOPSRC) $(SUBSRC)
	echo '(reset-handler abort) (compile-imported-libraries #t) (generate-wpo-files #t) (library-directories ".") (compile-library "'$(TOPSRC)'")' | $(SCHEME) $(SFLAGS)

$(SRFI_TOPWPO): $(TOPWPO) $(SRFI_TOPSRC) $(SRFI_SUBSRC)
	echo '(reset-handler abort) (compile-imported-libraries #t) (generate-wpo-files #t) (library-directories ".") (compile-library "'$(SRFI_TOPSRC)'")' | $(SCHEME) $(SFLAGS)

$(LIBDIR)/%: %
	$(INSTALL) -p -D "$<" "$@"

build: $(FFILIB) $(TOPWPO) $(SRFI_TOPWPO)

# install-ffi is always required, installations then need to decide what combination of src/so they want.
# Default install target is for everything.
install: install-ffi install-so install-src

install-ffi: $(IFFILIB)

install-so: $(ITOPWPO) $(ISUBWPO) $(ITOPOBJ) $(ISUBOBJ)

install-src: $(ITOPSRC) $(ISUBSRC)

install-srfi: install install-srfi-src install-srfi-so

install-srfi-src: $(ISRFI_TOPSRC) $(ISRFI_SUBSRC)

install-srfi-so: $(ISRFI_TOPWPO) $(ISRFI_SUBWPO) $(ISRFI_TOPOBJ) $(ISRFI_SUBOBJ)

clean:
	$(RM) $(FFIOBJ) $(FFILIB) $(TOPOBJ) $(TOPWPO) $(SUBOBJ) $(SUBWPO) $(SRFI_TOPWPO) $(SRFI_SUBWPO) $(SRFI_SUBOBJ) $(SRFI_TOPOBJ)

clean-install:
	$(RM) $(IFFIOBJ) $(IFFILIB) $(ITOPOBJ) $(ITOPWPO) $(ISUBOBJ) $(ISUBWPO) $(ITOPSRC) $(ISUBSRC) $(ISRFI_TOPWPO) $(ISRFI_SUBWPO) $(ISRFI_SUBOBJ) $(ISRFI_TOPOBJ) $(ISRFI_SUBSRC) $(ISRFI_TOPSRC)

clean-all: clean clean-install

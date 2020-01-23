# chez-socket Makefile.
# Written by Akce 2019, 2020.
# SPDX-License-Identifier: Unlicense

# Install destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
LIBDIR = ~/lib

# Path to chez scheme executable.
SCHEME = /usr/bin/scheme

# Scheme compile flags.
SFLAGS = -q

# Path to install executable.
INSTALL = /usr/bin/install

# Path to rm executable.
RM = /bin/rm

CFLAGS = -c -fpic

LIBFLAGS = -shared

## Should be no need to edit anything below here.

COBJ = socket/socket.o

# Scheme sources need to be in order of dependencies first, library last.
# That should avoid compile/load errors.
SSRC = socket/ftypes-util.chezscheme.sls	\
       socket/c.chezscheme.sls			\
       socket/impl.chezscheme.sls		\
       socket/basic.sls				\
       socket/extended.chezscheme.sls

SOBJ = socket/ftypes-util.chezscheme.so	\
       socket/c.chezscheme.so		\
       socket/impl.chezscheme.so	\
       socket/basic.so			\
       socket/extended.chezscheme.so

LIBS = socket/libsocket.so

all: $(LIBS) $(SOBJ)

%.o: %.c
	$(CC) $(CFLAGS) $< -o $@

socket/libsocket.so: $(COBJ)
	$(CC) $(LIBFLAGS) $< -o $@

%.so: %.sls
	echo '(reset-handler abort) (compile-library "'$<'")' | $(SCHEME) $(SFLAGS)

# install-lib is always required, installations then need to decide what combination of src/so they want.
# Default install target is for everything.
install: install-lib install-so install-src

install-lib: all
	$(INSTALL) -D -p -t $(LIBDIR)/socket $(LIBS)

install-so: all
	$(INSTALL) -D -p -t $(LIBDIR)/socket $(SOBJ)

install-src: all
	$(INSTALL) -D -p -t $(LIBDIR)/socket $(SSRC)

clean:
	$(RM) -f $(COBJ) $(LIBS) $(SOBJ)

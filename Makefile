# chez-socket Makefile.
# Written by Akce 2019.
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

CFLAGS = -c -fpic $(CONFIG_H)

LIBFLAGS = -shared

## Should be no need to edit anything below here.

COBJS = socket/socket.o

# SOBJS need to be in order of dependencies first, library last so that they can be built in order.
SOBJS = socket/ftypes-util.chezscheme.so socket/c.chezscheme.so socket/impl.chezscheme.so socket/basic.so

all: socket/libsocket.so $(SOBJS)

%.o: %.c
	$(CC) $(CFLAGS) $< -o $@

socket/libsocket.so: $(COBJS)
	$(CC) $(LIBFLAGS) $< -o $@

%.so: %.sls
	echo '(reset-handler abort) (compile-library "'$<'")' | $(SCHEME) $(SFLAGS)

install: all
	$(INSTALL) -D -t $(LIBDIR) $(BINS)

clean:
	rm -f $(COBJS) $(SOBJS) $(BINS)

# Chez socket library

chez-socket: A sockets interface for [Chez Scheme].

This library provides two socket interfaces, a stable and standard [srfi-106] "basic sockets" interface, and an extended highly experimental non-standard one.

This library should work on any POSIX system, although it has currently only been tried on Linux.

chez-socket should be considered Alpha software. It is missing a lot of features and needs more error handling.

## Compiling and installing

The recommended install method is to use [GNU make](https://www.gnu.org/software/make/).

A C compiler is required, along with libc headers.

There's two types of install supported. One for the extended interface and the other which installs both [srfi-106] and the extended one. That is due to [srfi-106] being built on top of the extended interface.

To compile and install the extended interface library source and shared-object files to LIBDIR:

    $ make install

Override LIBDIR to change install location (default is ~/lib/csv\<CHEZ-SCHEME-VERSION>). eg,

    $ make LIBDIR=/some/other/libdir install

Note that LIBDIR must be in `(library-directories)`. One way to add is by setting **CHEZSCHEMELIBDIRS**.

The install directory for the [srfi-106] interface is defined by SRFILIBDIR which defaults to LIBDIR.

To compile and install the [srfi-106] interface (which also builds and installs the extended interface):

    $ make srfi-install

Note that the SRFILIBDIR must be the toplevel library directory (as found in `(library-directories)`) and **not** include the leading `srfi` portion.

The default install target will install source files, shared object files, and [whole program optimisation](https://cisco.github.io/ChezScheme/csug9.5/system.html#./system:s117) files to LIBDIR. Other targets exist that install source only (install-src) and objects only (install-so).

Those using this library as part of [compiled programs](https://cisco.github.io/ChezScheme/csug9.5/system.html#./system:s76) will need to also distribute chez-socket's C compiled library `socket/libsocket.so`.

## How To Use

### SRFI-106 (Basic sockets)

`(import (srfi :160))` or `(import (srfi :160 socket))`  to use the [srfi-106], basic style sockets interface.

See the [examples](examples/) directory for a sample echo client/server implementation. The [srfi-106] documentation is comprehensive and a good information source.

### Extensions

(import (socket)) to use [srfi-106] plus local extensions. These are highly experimental and have lots of room for improvement.

These include:

```
[proc] socket-fd: returns the file descriptor for the socket.
```
```
[proc] socket->port: shortcut for creating a transcoded text port from a binary socket
```
The created port is input/output.
```
[proc] open-socket-input-port: creates a binary socket port for input only.
```
```
[proc] open-socket-output-port: creates a binary socket port for output only.
```
```
[proc] open-socket-input/output-port: creates a binary socket port for both input and output operations.
```
Some socket options whose values are integers or boolean may also be retrieved and set. For boolean options, use 0 for FALSE, and 1 for TRUE.
See the *extended* source file for a list of the options that are defined.
```
[proc] socket-get-int: Get integer socket option.
```
```
[proc] socket-set-int!: Set integer socket option.
```
eg,
```
> (define client-socket (make-client-socket "localhost" "5000"
                                            (address-family inet)
                                            (socket-domain stream)
                                            (address-info v4mapped addrconfig)
                                            (ip-protocol ip)))
> (getnameinfo client-socket (name-info none))
("localhost.localdomain" . "commplex-main")
> (getnameinfo client-socket (name-info nofqdn numericserv))
("localhost" . "5000")

```
```
[proc] getnameinfo: Get host and service information from a socket.
```
```
[proc] mcast-add-membership: Join IP multicast group.
```

## Links

[Chez Scheme]: https://cisco.github.io/ChezScheme/ "Chez Scheme"
[srfi-106]: https://srfi.schemers.org/srfi-106/srfi-106.html "srfi-106"

## Hacking

libsocket.so provides access to the OS socket layer. socket.c is used to build it.

(socket c) uses Chez Ftypes FFI, along with (socket ftypes-util) helper functions, to export libsocket.so to scheme.

From there, various higher level abstractions for using sockets can be defined. eg, (socket basic) for an [srfi-106] style interface.

## License

chez-socket is an [unlicensed](LICENSE) work released into the Public Domain.

examples/server.ss and examples/client.ss are taken from the examples provided in [srfi-106] with the following copyright:

	Copyright (C) Takashi Kato (2012). All Rights Reserved.

	Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


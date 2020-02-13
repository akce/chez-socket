/*
 * C/OS layer for Chez scheme basic sockets.
 *
 * Try and keep this as minimal as possible, with most of the work done at the scheme level.
 *
 * This module should only contain variable definitions and struct accessors so
 * that scheme doesn't need to track offsets etc.
 *
 * Written by Akce 2019-2020.
 * SPDX-License-Identifier: Unlicense
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netdb.h>
#include <arpa/inet.h>	// inet_pton

#include <stdio.h>	// printf
#include <stdlib.h>	// calloc, malloc
#include <unistd.h>	// close

// TODO set -1 #if !defined(n).
#define C_CONST_INT(n) const int c_ ## n = n

/* Constants: required by Basic Sockets (SRFI-106). */
C_CONST_INT(AF_INET);
C_CONST_INT(AF_INET6);
C_CONST_INT(AF_UNSPEC);

C_CONST_INT(SOCK_STREAM);
C_CONST_INT(SOCK_DGRAM);

C_CONST_INT(AI_CANONNAME);
C_CONST_INT(AI_NUMERICHOST);
C_CONST_INT(AI_V4MAPPED);
C_CONST_INT(AI_ALL);
C_CONST_INT(AI_ADDRCONFIG);

C_CONST_INT(IPPROTO_IP);
C_CONST_INT(IPPROTO_TCP);
C_CONST_INT(IPPROTO_UDP);

C_CONST_INT(MSG_PEEK);
C_CONST_INT(MSG_OOB);
C_CONST_INT(MSG_WAITALL);

C_CONST_INT(SHUT_RD);
C_CONST_INT(SHUT_WR);
C_CONST_INT(SHUT_RDWR);

/* Constants: extensions to Basic Sockets (SRFI-106). */

C_CONST_INT(SOMAXCONN);
C_CONST_INT(AI_NUMERICSERV);
C_CONST_INT(AI_PASSIVE);

/* Socket level (SOL_) */
C_CONST_INT(SOL_SOCKET);

/* SOL_SOCKET socket level Options. See socket(7) */
// TODO There's still plenty undefined, only adding the more interesting ones for now.
C_CONST_INT(SO_ACCEPTCONN);	/* bool read-only */
C_CONST_INT(SO_BROADCAST);	/* bool datagram only. */
C_CONST_INT(SO_DONTROUTE);	/* bool */
C_CONST_INT(SO_ERROR);		/* read-only: value cleared after read. */
C_CONST_INT(SO_KEEPALIVE);	/* bool */
C_CONST_INT(SO_LINGER);		/* linger struct. */
C_CONST_INT(SO_OOBINLINE);	/* bool */
C_CONST_INT(SO_PROTOCOL);	/* read-only */
C_CONST_INT(SO_REUSEADDR);	/* bool */
C_CONST_INT(SO_TYPE);		/* read-only */

const int c_S_SIZEOF_SOCKADDR = sizeof(struct sockaddr_storage);

/* See getaddrinfo(2) for a full C client/server example. */


/* See getaddrinfo(2) for a full C client/server example. */

/* addrinfo accessors. */
int addrinfo_flags(const struct addrinfo* ai) { return ai->ai_flags; }
int addrinfo_family(const struct addrinfo* ai) { return ai->ai_family; }
int addrinfo_socktype(const struct addrinfo* ai) { return ai->ai_socktype; }
int addrinfo_protocol(const struct addrinfo* ai) { return ai->ai_protocol; }
int addrinfo_addrlen(const struct addrinfo* ai) { return ai->ai_addrlen; }
struct sockaddr* addrinfo_addr(const struct addrinfo* ai) { return ai->ai_addr; }
const struct addrinfo* addrinfo_next(const struct addrinfo* ai) { return ai->ai_next; }

/* create an addrinfo struct suitable for use as hints with getaddrinfo.
 * getaddrinfo(3) specifies that only flags, family, socktype, and protocol are used as hints.
 */
struct addrinfo*
make_addrinfo_hints(int flags, int family, int socktype, int protocol)
	{
	/* calloc(3) will zero the alloc'd memory. */
	struct addrinfo* hints = calloc(sizeof(*hints), 1);
	if (hints)
		{
		hints->ai_flags = flags;
		hints->ai_family = family;
		hints->ai_socktype = socktype;
		hints->ai_protocol = protocol;
		}
	return hints;
	}



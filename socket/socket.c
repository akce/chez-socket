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
C_CONST_INT(SO_DOMAIN);		/* int read-only: eg, AF_INET6. */
C_CONST_INT(SO_DONTROUTE);	/* bool */
C_CONST_INT(SO_ERROR);		/* read-only: value cleared after read. */
C_CONST_INT(SO_KEEPALIVE);	/* bool */
C_CONST_INT(SO_LINGER);		/* linger struct. */
C_CONST_INT(SO_OOBINLINE);	/* bool */
C_CONST_INT(SO_PROTOCOL);	/* int read-only: eg, IPPROTO_TCP */
C_CONST_INT(SO_REUSEADDR);	/* bool */
C_CONST_INT(SO_TYPE);		/* int read-only: eg, SOCK_STREAM */

const int c_S_SIZEOF_SOCKADDR = sizeof(struct sockaddr_storage);

/* IPPROTO_IP socket level options. See ip(7) */

/* IP multicast. */
C_CONST_INT(IP_MULTICAST_LOOP);		/* bool */
C_CONST_INT(IP_MULTICAST_TTL);		/* int range: 1-255. */
C_CONST_INT(IP_MULTICAST_IF);		/* struct in_addr */
C_CONST_INT(IP_ADD_MEMBERSHIP);		/* struct ip_mreqn or older struct ip_mreq */
C_CONST_INT(IP_DROP_MEMBERSHIP);	/* struct ip_mreqn or older struct ip_mreq */

/* getnameinfo(3) flags */
C_CONST_INT(NI_NAMEREQD);
C_CONST_INT(NI_DGRAM);
C_CONST_INT(NI_NOFQDN);
C_CONST_INT(NI_NUMERICHOST);
C_CONST_INT(NI_NUMERICSERV);
/* glibc 2.3.4+ extensions */
#if 0
/* Remove for now: these aren't included in my glibc version. */
C_CONST_INT(NI_IDN);
C_CONST_INT(NI_IDN_ALLOW_UNASSIGNED);
C_CONST_INT(NI_IDN_USE_STD3_ASCII_RULES);
#endif
/* These are sensible buffer defaults and may be glibc extensions; they should be checked via C feature flags... */
/* There's a case for hardcoding these on the scheme side so they're always available. */
C_CONST_INT(NI_MAXHOST);
C_CONST_INT(NI_MAXSERV);

/* See getaddrinfo(2) for a full C client/server example. */

/* addrinfo accessors. */
int addrinfo_flags(const struct addrinfo* ai) { return ai->ai_flags; }
int addrinfo_family(const struct addrinfo* ai) { return ai->ai_family; }
int addrinfo_socktype(const struct addrinfo* ai) { return ai->ai_socktype; }
int addrinfo_protocol(const struct addrinfo* ai) { return ai->ai_protocol; }
socklen_t addrinfo_addrlen(const struct addrinfo* ai) { return ai->ai_addrlen; }
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

/* mcast_add_membership: Add socket to IPv4 multicast group.
 * returns:
 *   = 0 on success
 *   = -1 setsockopt error
 *   = -2 address/node string conversion failure.
 */
int
mcast4_add_membership(int fd, const char* node, int interface)
	{
	int rc = -2;
	// TODO Check on some BSDs, struct ip_mreqn might be Linux specific.
	struct ip_mreqn req =
		{
		.imr_ifindex = interface,
		};
	if (inet_pton(AF_INET, node, &req.imr_multiaddr) == 1)
		{
		/* Node address converted successfully. */
		rc = setsockopt(fd, IPPROTO_IP, IP_ADD_MEMBERSHIP, &req, sizeof(req));
		}
	return rc;
	}

/* mcast6_add_membership: Add socket to IPv6 multicast group.
 * returns:
 *   = 0 on success
 *   = -1 setsockopt error
 *   = -2 address/node string conversion failure.
 */
int
mcast6_add_membership(int fd, const char* node, int interface)
	{
	int rc = -2;
	struct ipv6_mreq req =
		{
		.ipv6mr_interface = interface,
		};
	if (inet_pton(AF_INET6, node, &req.ipv6mr_multiaddr) == 1)
		{
		/* Node address converted successfully. */
		rc = setsockopt(fd, IPPROTO_IPV6, IPV6_ADD_MEMBERSHIP, &req, sizeof(req));
		}
	return rc;
	}

/*
 * C/OS layer for Chez scheme basic sockets.
 *
 * Try to implement enough at this layer so that the scheme only needs
 * to deal with opaque pointers.
 *
 * Written by Akce 2019.
 * SPDX-License-Identifier: Unlicense
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netdb.h>

#include <stdio.h>	// printf
#include <stdlib.h>	// malloc
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

/* See ip(7) */
//C_CONST_LONG(INADDR_ANY);
/* IPPROTO_IP socket level options. See ip(7) */
C_CONST_INT(IP_ADD_MEMBERSHIP);	/* struct ip_mreqn or older struct ip_mreq */

/* See ipv6(7) */


/* See getaddrinfo(2) for a full C client/server example. */

typedef struct
	{
	int socketfd;
	struct addrinfo* addrs;
	struct addrinfo* addr;
	} t_connection;

typedef int (*cb_t)(int sockfd, const struct sockaddr*, socklen_t);

t_connection*
make_connection(const char* node, const char* service, int ai_family, int ai_socktype, int ai_flags, int ai_protocol, cb_t func)
	{
	struct addrinfo hints =
		{
		.ai_family = ai_family,
		.ai_socktype = ai_socktype,
		.ai_flags = ai_flags,
		.ai_protocol = ai_protocol
		};
	struct addrinfo* addrs = 0;
	t_connection* conn = 0;
	int ret = 0;

	ret = getaddrinfo(node, service, &hints, &addrs);
	if (ret == 0)
		{
		/* Success. */
		struct addrinfo* ptr = 0;
		int fd = 0;
		for (ptr = addrs; ptr; ptr = ptr->ai_next)
			{
			fd = socket(ptr->ai_family, ptr->ai_socktype, ptr->ai_protocol);
			if (fd == -1)
				continue;
			ret = func(fd, ptr->ai_addr, ptr->ai_addrlen);
			if (ret == 0)
				{
				/* Connected. */
				conn = malloc(sizeof(*conn));
				conn->socketfd = fd;
				conn->addrs = addrs;
				conn->addr = ptr;
				break;
				}
			else
				{
				/* Failed to connect. Close socket and try next address (if any).. */
				close(fd);
				}
			}
		if (conn == 0)
			{
			/* Unable to connect to any address. Cleanup before exit. */
			freeaddrinfo(addrs);
			/* TODO propagate error to scheme code.. */
			}
		}
	else
		{
		/* TODO propagate error to scheme code.. */
		printf("%d: %s\n", ret, gai_strerror(ret));
		}

	return conn;
	}

t_connection*
make_client_connection(const char* node, const char* service, int ai_family, int ai_socktype, int ai_flags, int ai_protocol)
	{
	return make_connection(node, service, ai_family, ai_socktype, ai_flags, ai_protocol, connect);
	}

t_connection*
make_server_connection(const char* service, int ai_family, int ai_socktype, int ai_protocol)
	{
	/* Flags use the defaults from Linux/GLIBC & SRFI-106. */ 
	const int flags = AI_V4MAPPED | AI_ADDRCONFIG;
	t_connection* conn = make_connection(0, service, ai_family, ai_socktype, flags, ai_protocol, bind);

	if (conn && (ai_socktype == SOCK_STREAM))
		{
		/* TCP sockets (streams) also need to be listened to. */
		listen(conn->socketfd, SOMAXCONN);
		}

	return conn;
	}

/* See accept(2) */
t_connection*
connection_accept(t_connection* conn)
	{
	t_connection* peer = 0;
	if (conn && (conn->socketfd != -1))
		{
		/* TODO store peer address info.
		 * See ip(7) for sockaddr_in, and ipv6(7) for sockaddr_in6.
		 */
		int peerfd = accept(conn->socketfd, 0, 0);
		if (peerfd == -1)
			{
			/* Error. */
			/* TODO propagate error to scheme code.. */
			}
		else
			{
			peer = malloc(sizeof(*peer));
			peer->socketfd = peerfd;
			peer->addrs = 0;
			peer->addr = 0;
			}
		}
	return peer;
	}

/* See recv(2) */
int
connection_recv(const t_connection* conn, void* buf, ssize_t buflen, int flags)
	{
	int ret = -1;
	if (conn && (conn->socketfd != -1))
		{
		ret = recv(conn->socketfd, buf, buflen, flags);
		/* TODO handle EINTR here? */
		}
	return ret;
	}

/* See send(2) */
int
connection_send(const t_connection* conn, const void* buf, ssize_t buflen, int flags)
	{
	int ret = -1;
	if (conn && (conn->socketfd != -1))
		{
		ret = send(conn->socketfd, buf, buflen, flags);
		/* TODO handle EINTR here? */
		}
	return ret;
	}

/* See close(2) */
void
connection_close(t_connection* conn)
	{
	if (conn && (conn->socketfd != -1))
		{
		if (close(conn->socketfd) == 0)
			{
			conn->socketfd = -1;
			freeaddrinfo(conn->addrs);
			}
		/* TODO propagate error to scheme code.. */
		}
	}

/* shutdown(2) */
void
connection_shutdown(t_connection* conn, int how)
	{
	if (conn && (conn->socketfd != -1))
		{
		/* TODO assumes 'how' is a valid value. */
		if (shutdown(conn->socketfd, how) != 0)
			{
			/* TODO propagate error to scheme code.. */
			}
		}
	}


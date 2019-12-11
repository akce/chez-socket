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

const int c_AF_INET = AF_INET;
const int c_AF_INET6 = AF_INET6;
const int c_AF_UNSPEC = AF_UNSPEC;

const int c_SOCK_STREAM = SOCK_STREAM;
const int c_SOCK_DGRAM = SOCK_DGRAM;

const int c_AI_CANONNAME = AI_CANONNAME;
const int c_AI_NUMERICHOST = AI_NUMERICHOST;
const int c_AI_V4MAPPED = AI_V4MAPPED;
const int c_AI_ALL = AI_ALL;
const int c_AI_ADDRCONFIG = AI_ADDRCONFIG;

const int c_IPPROTO_IP = IPPROTO_IP;
const int c_IPPROTO_TCP = IPPROTO_TCP;
const int c_IPPROTO_UDP = IPPROTO_UDP;

const int c_MSG_PEEK = MSG_PEEK;
const int c_MSG_OOB = MSG_OOB;
const int c_MSG_WAITALL = MSG_WAITALL;

const int c_SHUT_RD = SHUT_RD;
const int c_SHUT_WR = SHUT_WR;
const int c_SHUT_RDWR = SHUT_RDWR;

/* GLIBC: 'man getaddrinfo' for a full C client/server example. */

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

/* GLIBC: 'man 2 close' */
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

/* GLIBC: 'man 2 shutdown' */
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


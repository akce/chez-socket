/*
 * C/OS layer for Chez scheme basic sockets.
 * Written by Akce 2019.
 * SPDX-License-Identifier: Unlicense
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netdb.h>

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


/*
*
* SSHole man-in-the-middle SSL debugging tool
* $Id$
*
* Copyright (C) 2004 by Konstantin Klyagin <konst@konst.org.ua>
*
* This program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2 of the License, or (at
* your option) any later version.
*
* This program is distributed in the hope that it will be useful, but
* WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
* USA
*
*/

#include <stdio.h>

#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>

#define OPENSSL_NO_KRB5 1
#include <openssl/ssl.h>
#include <openssl/err.h>

SSL_CTX *ctx;

char *remotehost = 0, *certfname = "server.pem", *progname;
int remoteport = 0, listenport = 0;

struct sockaddr_in clientsa;

void bailout(const char *msg) {
    printf("Error: %s\n", msg);
    exit(1);
}

void usage() {
    printf("Usage: %s -l <listen> -h <remote host> -p <remote port> [ -c <cert fname> ]\n\n", progname);
    printf("General options:\n");
    printf("  --listen-port, -l\tPort number to listen on for incoming connections\n");
    printf("  --remote-host, -h\tRemote host we sshole the traffic for\n");
    printf("  --remote-port, -p\tPort on the remote host\n");
    printf("  --certificate, -c\tServer certificate; defaults to server.pem\n\n");
    exit(1);
}

void commandline(int argc, char **argv) {
    int i;
    progname = argv[0];

    printf("SSHole: Secure Sockets Man-in-the-Middle Tool\n");
    printf("by Konstantin Klyagin <konst@konst.org.ua>\n\n");

    for(i = 1; i < argc; i++) {
	if(!strcmp(argv[i], "-l") || !strcmp(argv[i], "--listen-port")) {
	    listenport = atoi(argv[++i]);
	} else if(!strcmp(argv[i], "-h") || !strcmp(argv[i], "--remote-host")) {
	    remotehost = argv[++i];
	} else if(!strcmp(argv[i], "-p") || !strcmp(argv[i], "--remote-port")) {
	    remoteport = atoi(argv[++i]);
	} else if(!strcmp(argv[i], "-c") || !strcmp(argv[i], "--certificate")) {
	    certfname = argv[++i];
	} else {
	    printf("Unknown parameter: %s\n", argv[i]);
	    usage();
	}
    }

    if(!listenport || !remoteport || !remotehost) {
	usage();
    }
}

void dump(const unsigned char *buf, int size, int cli) {
    int n, pos, rsize, k;
    char hd[128];

    if(cli) {
	sprintf(hd, "%s:%d -> %s:%d", remotehost, remoteport,
	    inet_ntoa(clientsa.sin_addr), ntohs(clientsa.sin_port));
    } else {
	sprintf(hd, "%s:%d -> %s:%d", inet_ntoa(clientsa.sin_addr),
	    ntohs(clientsa.sin_port), remotehost, remoteport);
    }

    printf("\nData block :: %s:\n", hd);

    rsize = ((size+15)/16)*16;

    for(n = pos = 0; n < rsize; n++) {
	if(n < size) printf("%02X ", buf[n]);
	    else printf("   ");

	if(n % 4 == 3) printf(" ");

	if(n % 16 == 15) {
	    printf("  ");

	    for(k = n-15; k < n; k++) {
		if(k < size) printf("%c", isprint(buf[k]) ? (buf[k] < 0x80 ? buf[k] : '.') : '.');
		    else printf(" ");
	    }

	    printf("\n");
	    pos = 0;
	}
    }

    fflush(stdout);
}

void getsession(SSL *serverssl) {
    int clientfd, serverfd, err, len, fl, hfd;
    struct hostent *he;
    struct sockaddr_in sa;
    fd_set fds;
    char buf[512];

    SSL *clientssl;

    he = gethostbyname(remotehost);
    if(!he) bailout("Unable to resolve the host");

    clientfd = socket(AF_INET, SOCK_STREAM, 0);
    if(clientfd < 0) bailout("Unable to create a socket");

    memset(&sa, 0, sizeof(sa));
    memcpy(&sa.sin_addr.s_addr, *he->h_addr_list, he->h_length);
    sa.sin_family = AF_INET;
    sa.sin_port = htons(remoteport);

    err = connect(clientfd, (struct sockaddr *) &sa, sizeof(sa));
    if(err < 0) bailout("Connection failed");

    clientssl = SSL_new(ctx);
    SSL_set_fd(clientssl, clientfd);

    err = SSL_connect(clientssl);
    if(err != 1) bailout("SSL connection failed");

    serverfd = SSL_get_fd(serverssl);

    fcntl(serverfd, F_SETFL, fcntl(serverfd, F_GETFL) & ~O_NONBLOCK);
    fcntl(clientfd, F_SETFL, fcntl(clientfd, F_GETFL) & ~O_NONBLOCK);

    while(1) {
	FD_ZERO(&fds);
	FD_SET(clientfd, &fds);
	FD_SET(serverfd, &fds);

	err = select(clientfd+1, &fds, 0, 0, 0);
	if(err < 0) break;

	if(FD_ISSET(clientfd, &fds)) {
	    len = SSL_read(clientssl, buf, sizeof(buf));
	    if(len > 0) SSL_write(serverssl, buf, len), dump(buf, len, 1);
		else break;
	}

	if(FD_ISSET(serverfd, &fds)) {
	    len = SSL_read(serverssl, buf, sizeof(buf));
	    if(len > 0) SSL_write(clientssl, buf, len), dump(buf, len, 0);
		else break;
	}
    }

    SSL_free(clientssl);
    shutdown(clientfd, 2);
    close(clientfd);
}

int main(int argc, char **argv) {
    // Listen socket fd, client socket, DNS and sockaddr variables.
    int fd, cfd, err, n;
    struct hostent *he;
    struct sockaddr_in sa;
    socklen_t len;

    commandline(argc, argv);

    SSL_library_init();
    SSL_load_error_strings();

#ifdef HAVE_SSLEAY
    SSLeay_add_all_algorithms();
#else
    OpenSSL_add_all_algorithms();
#endif

    ctx = SSL_CTX_new(SSLv23_method());

    SSL_CTX_use_certificate_file(ctx, certfname, SSL_FILETYPE_PEM);
    SSL_CTX_use_PrivateKey_file(ctx, certfname, SSL_FILETYPE_PEM);

    // This routine lets us check if everything went fine.
    if(!SSL_CTX_check_private_key(ctx))
	bailout("Private key verification failed: check your certificate file");

    // Create a listen socket.
    fd = socket(AF_INET, SOCK_STREAM, 0);
    if(fd < 0) bailout("Unable to create a socket");

    // Fill in the sockaddr structure.
    memset(&sa, 0, sizeof(sa));
    sa.sin_family = AF_INET;
    sa.sin_addr.s_addr = INADDR_ANY;
    sa.sin_port = htons(listenport);

    // Bind the socket to the specified port.
    err = bind(fd, (struct sockaddr *) &sa, sizeof(sa));
    if(err < 0) bailout("Binding to the port failed");

    n = 0;
    printf("Started. Waiting for connections..\n");

    while(1) {
	// Listen on socket. The program stays here until an
	// incoming connection is received.
	err = listen(fd, 5);
	if(err < 0) bailout("Listening for incoming connections failed");

	// Accept connection, obtain a client socket fd.
	len = sizeof(clientsa);
	cfd = accept(fd, (struct sockaddr *) &clientsa, &len);
	if(cfd < 0) bailout("The accept() call failed");

	if(!fork()) {
	    SSL *ssl;
	    printf("\nConnection from %s\n", inet_ntoa(clientsa.sin_addr));

	    ssl = SSL_new(ctx);
	    SSL_set_fd(ssl, cfd);

	    // Negociate SSL for the client connection.
	    err = SSL_accept(ssl);
	    if(err != 1) bailout("SSL_accept() failed");

	    getsession(ssl);

	    // Free the SSL structure and close a socket.
	    SSL_free(ssl);
	    shutdown(cfd, 2);
	    close(cfd);

	    printf("\nConnection closed\n");
	    fflush(stdout);
	    exit(0);
	}
    }
}

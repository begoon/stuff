/*
GazTek HTTP Daemon (ghttpd)
Copyright (C) 1999  Gareth Owen <gaz@athene.co.uk>

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

#include <stdio.h> 
#include <stdlib.h> 
#include <errno.h> 
#include <string.h> 
#include <sys/types.h> 
#include <winsock.h>
#include <time.h>
#include <dirent.h>
              
#include "ghttpd.h"

unsigned long no_vhosts = 0;
t_vhost *vhosts = 0;

unsigned int SERVERPORT = 80;
char SERVERTYPE[255] = "Standalone";
char SERVERROOT[255];
char PARSETYPES[255] = ".shtml .splice";
char ALLOWSQL[255] = "yes";
t_vhost defaulthost;
char * SERVERIP;

long WINAPI handlereq(long sq)
{
        SOCKET s;

        s = (SOCKET)sq;
        serveconnection(s);
        closesocket(s);
}

int main()
{
        WSADATA ws;
        HANDLE h;
        SOCKET sockfd, new_fd;  /* listen on sock_fd, new connection on new_fd */
	struct sockaddr_in my_addr;    /* my address information */
	struct sockaddr_in their_addr; /* connector's address information */
	int sin_size, i;
	struct timeval tv;

	/*
	 * Setup the default values
	 */

        WSAStartup(0x101,&ws);
        GetCurrentDirectory(254,SERVERROOT);

        strcpy(defaulthost.DOCUMENTROOT, "D:\\Webroot");
	strcpy(defaulthost.DEFAULTPAGE, "index.html");
	strcpy(defaulthost.CGIBINDIR, "/cgi-bin");
        strcpy(defaulthost.CGIBINROOT, "D:\\Webroot/cgi-bin");

	/*
	 * Count the virtual hosts and allocate the memory
	 */

	no_vhosts = count_vhosts();
	vhosts = (t_vhost *)malloc((no_vhosts+1) * sizeof(t_vhost));

	/*
	 * Read in all the virtual hosts and other configuration
	 */

	readinconfig();

	/*
	 * Setup the sockets and wait and process connections
	 */

	if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
		perror("socket");
		exit(1);
	}

	/* Let the kernel reuse the socket address. This lets us run
           twice in a row, without waiting for the (ip, port) tuple
           to time out. */
        i = 1;
        setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, (void*)&i, i);
	                                      

	my_addr.sin_family = AF_INET;         /* host byte order */
	my_addr.sin_port = htons(SERVERPORT);     /* short, network byte order */
	my_addr.sin_addr.s_addr = INADDR_ANY; /* auto-fill with my IP */
	
	SERVERIP = (char *)my_addr.sin_addr.s_addr;
	
	if (bind(sockfd, (struct sockaddr *)&my_addr, sizeof(struct sockaddr)) == -1) 
	{
		perror("bind");
		exit(1);
	}

	if (listen(sockfd, BACKLOG) == -1) {
		perror("listen");
		exit(1);
	}

	printf("\nLHTTPd Running...\n");

	while(1) {  /* main accept() loop */
		sin_size = sizeof(struct sockaddr_in);

		if ((new_fd = accept(sockfd, (struct sockaddr *)&their_addr, &sin_size)) == -1)
			continue;

                CreateThread(NULL,0,(LPTHREAD_START_ROUTINE) handlereq,(LPVOID)new_fd,0,(LPDWORD) &h);
                if(!h)
                        closesocket(new_fd);
                else
                        CloseHandle(h);
                
	}

	return 0;
}



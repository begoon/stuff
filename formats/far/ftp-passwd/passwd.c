// "HostName"="host_name"
// "User"="user_name"
// "Password"=hex:d1,c1,32,33,34,35,36,37,38,00,00,00,00,00,00,00,00,00,00,00,00,\

#include <stdio.h>
#include <assert.h>

const char* decode(const char* passwd) {
  char pwd[1024];
  int sz = 0, i;
  const char* p;
  for (p = passwd + 4; ; p += 3, sz += 1) {
    int n;
    assert(sscanf(p, "%02x,", &n) == 1);
    pwd[sz] = n;
    if (n == 0) break;
  }
  pwd[0] ^= pwd[1]; 
  pwd[0] |= 0x50;
  for (i = 2; i < sz; i++) pwd[i] ^= pwd[0];
  static char decoded[1024];
  decoded[0] = 0;
  for (i = 2; i < sz; i++) {
    sprintf(decoded + strlen(decoded), "%c", pwd[i]);
  }
  return decoded;
}

int main() {
  char line[1024];
  int lineno = 1;
  while (!feof(stdin)) {
    fgets(line, sizeof(line), stdin);
    if (strlen(line) == 0) break;
    if (!memcmp(line, "\"HostName\"", 10)) {
      char host[1024];
      strcpy(host, line + 12);
      host[strlen(host) - 2] = 0;
      printf("(%03d) host = %s\n", lineno, host);
    }
    if (!memcmp(line, "\"User\"", 6)) {
      char user[1024];
      strcpy(user, line + 8);
      user[strlen(user) - 2] = 0;
      printf("(%03d) user = %s\n", lineno, user);
    }
    if (!memcmp(line, "\"Password\"", 10)) {
      char passwd[1024];
      strcpy(passwd, line + 11);
      passwd[strlen(passwd) - 2] = 0;
      printf("(%03d) passwd = %s\n\n", lineno, decode(passwd));
    }
    lineno += 1;
  };
  return 0;
}


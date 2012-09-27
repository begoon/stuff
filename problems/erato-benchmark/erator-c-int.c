#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h>

int main(int argc, char* argv[]) {
  int n = argc > 1 ? atoi(argv[1]) : 100;
  int* S;
  int count;
  int sz = n * sizeof(*S);
  int i, j;

  printf("%d\n", n);

  long sqrt_n = sqrt(n) + 1;

  S = malloc(sz);
  memset(S, 0, sz);

  for (i = 2; i < sqrt_n; ++i)
    if (S[i] == 0)
      for (j = i*i; j < n; j+=i)
        S[j] = 1;

  count = 0;
  for (i = 2; i < n; ++i)
    if (S[i] == 0)
      count++;

  printf("%d\n", count);

  free(S);
  return 0;
}

int nb_dividers(int N) {
  int a = 1;
  for (int i = 2; i*i <= N; ++i) {
    if (N % i) continue;
    int c = 0;
    while ((N % i) == 0) {
      c += 1;
      N /= i;
    }
    a *= c + 1;
  }
  if (N > 1)
    a *= 2;
  return a;
}


int power(int t, int k) {
  int r = 1;
  while (k) {
    if (k & 1) r *= t;
    t *= t;
    k >>= 1;
  }
  return r;
}

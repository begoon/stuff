// The last value is C(30, 61)
long long C(int m, int n) {
  long long p = 1;
  for (int i = 1; i <= m; i++) {
    p *= n - i + 1;
    p /= i;
  }
  return p;
}

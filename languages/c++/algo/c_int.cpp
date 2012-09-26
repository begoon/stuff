// The last value is C(29, 15)
int C(int m, int n) {
  int p = 1;
  for (int i = 1; i <= m; i++) {
    p *= n - i + 1;
    p /= i;
  }
  return p;
}

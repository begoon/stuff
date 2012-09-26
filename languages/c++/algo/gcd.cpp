int gcd(int a, int b) {
  if (b == 0) return a;
  return gcd(b, a % b);
}

int gcd(int a, int b)  {
  while (b != 0) {
    int t = a % b;
    a = b;
    b = t;
  }
  return a;
}

int gcd(int a, int b, int* x, int* y) {
    if (b > a) return gcd(b, a, y, x);
    if (b == 0) {
      *x = 1;
      *y = 0;
      return a;
    }
    int x1, y1;
    int r = gcd(b, a % b, &x1, &y1);
    *x = y1;
    *y = x1 - a / b * y1;
    return r;
}

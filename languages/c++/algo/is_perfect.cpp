bool is_perfect(long long n) {
  if (n == 1) return false;
  long long s = 1;
  long long q = sqrt((double)n);
  for (long long i = 2; i <= q; ++i) {
    if ((n % i) == 0) {
      s += i;
      if (n/i != i) s += n/i;
    }
  }
  return s == n;
}

long long perfect[] = { 6, 28, 496, 8128, 33550336, 8589869056, 137438691328, 2305843008139952128 };


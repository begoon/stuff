bool extact_sqrt(int n) {
  int i = floor(sqrt((double)n) + 0.5);
  return i * i == n;
}


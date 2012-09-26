template <typename T>
int nb_bits(T n) {
  int r = 0;
  while (n) {
    n &= (n - 1);
    ++r;
  }
  return r;
}


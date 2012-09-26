template <typename T>
T min(T a, T b, T c) {
  return min(a, min(b, c));
}

template <typename T>
T max(T a, T b, T c) {
  return max(a, max(b, c));
}


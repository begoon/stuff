bool is_palindrom(const string& a) {
  int sz = a.length();
  for (int i = 0; i < sz / 2; ++i)
    if (a[i] != a[sz - i - 1])
      return false;
  return true;
}


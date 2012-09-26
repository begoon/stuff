void z_function(const string& s, vector<int>& z) {
  int sz = (int)s.length();
  z.resize(sz);

  int l = 0, r = 0;
  for (int i = 1; i < sz; ++i)
    if (z[i - l] + i <= r)
      z[i] = z[i - l];
    else {
      l = i;
      if (i > r) r = i;
      for (z[i] = r - i; r < sz; ++r, ++z[i])
        if (s[r] != s[z[i]])
          break;
      --r;
    }
}

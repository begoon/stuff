int max_rectangle(vector<string>& f, int* height = 0, int* width = 0) {
  int Y = f.size(), X = f[0].length();
  const char empty = '.';
  int r = 0;
  vector<int> c(Y + 1, 0);
  for (int x = X - 1; x >= 0; --x) {
    for (int y = 0; y < Y; ++y)
      c[y] = f[y][x] == empty ? c[y] + 1 : 0;
    stack<pair<int, int> > s;
    int w = 0;
    for (int y = 0; y <= Y; ++y) {
      if (c[y] > w) {
        s.push(make_pair(y, w));
        w = c[y];
      }
      if (c[y] < w) {
        int y0, w0;
        do {
          assert(!s.empty());
          y0 = s.top().first;
          w0 = s.top().second;
          s.pop();
          if (w * (y - y0) > r) {
            r = w * (y - y0);
            if (width) *width = w;
            if (height) *height = y - y0;
          }
          w = w0;
        } while (c[y] < w);
        w = c[y];
        if (w != 0)
          s.push(make_pair(y0, w0));
      }
    }
  }
  return r;
}


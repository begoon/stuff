int triangle_area_2(int x1, int y1, int x2, int y2, int x3, int y3) {
  return (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1);
}

int line_length(int x1, int y1, int x2, int y2) {
  return (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2);
}

// --------------------------------------------------------------------------------------

struct point {
  point() : x(0), y(0) {}
  point(int ax, int ay) : x(ax), y(ay) {}
  int x, y;
};

int square(point a, point b, point c) {
  return a.x * (b.y - c.y) + b.x * (c.y - a.y) + c.x * (a.y - b.y);
}
 
bool intersect_1(int a, int b, int c, int d) {
  return max(a, b) >= min(c, d) && max(c, d) >= min(a, b);
}
 
bool intersect(point a, point b, point c, point d) {
  int s11 = square(a, b, c);
  int s12 = square(a, b, d);
  int s21 = square(c, d, a);
  int s22 = square(c, d, b);
  if (s11 == 0 && s12 == 0 && s21 == 0 && s22 == 0)
    return intersect_1(a.x, b.x, c.x, d.x) && intersect_1(a.y, b.y, c.y, d.y);
  else
    return s11 * s12 <= 0 && s21 * s22 <= 0;
}


int triangle_square_2(int x1, int y1, int x2, int y2, int x3, int y3) {
  return x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2);
}
 
double triangle_square(int x1, int y1, int x2, int y2, int x3, int y3) {
  return abs(triangle_square_2(x1, y1, x2, y2, x3, y3)) / 2.0;
}
 
bool clockwise(int x1, int y1, int x2, int y2, int x3, int y3) {
  return triangle_square_2(x1, y1, x2, y2, x3, y3) < 0;
}
 
bool counter_clockwise(int x1, int y1, int x2, int y2, int x3, int y3) {
  return triangle_square_2(x1, y1, x2, y2, x3, y3) > 0;
}

// --------------------------------------------------------------------------------------

struct point {
  point() : x(0), y(0) {}
  point(int ax, int ay) : x(ax), y(ay) {}
  bool operator==(const point& p) { return x == p.x && y == p.y; }
  int x, y;
};

struct rect {
  rect() {}
  rect(const point& afrom, const point& ato) : from(afrom), to(ato) {
    if (from.x > to.x) swap(from.x, to.x);
    if (from.y > to.y) swap(from.y, to.y);
  }
  point from, to;
};

bool intersect_rect(const point& a1, const point& a2, const point& b1, const point& b2) {
  return min(a2.x, b2.x) > max(a1.x, b1.x) && min(a2.y, b2.y) > max(a1.y, b1.y);
}

bool intersect_rect(const rect& a, const rect& b) {
  return min(a.to.x, b.to.x) > max(a.from.x, b.from.x) && 
         min(a.to.y, b.to.y) > max(a.from.y, b.from.y);
}


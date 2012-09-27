#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <stack>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <iterator>
#include <numeric>
#include <limits>
#include <algorithm>
#include <string>
#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cassert>
#ifdef _DEBUG
#include <windows.h>
#endif
using namespace std;

#ifdef max
#undef max
#endif

#ifdef min
#undef min
#endif

const double EPS = 1e-9;

template <typename T>
T max(T a, T b, T c) {
  return max(a, max(b, c));
}

enum piece_t {
  kQueen = 0, kRook = 1, kBishop = 2, kKing = 3, kKnight = 4, kLast
};

struct item_t {
  item_t(int ay, int ax, int atype) : y(ay), x(ax), type(atype) {}
  int x, y, type;
};

typedef vector<item_t> items_t;

struct state_t {
  items_t board;
  int height;
  int width;
  int count;
  vector<int> pieces;
  vector<pair<int, int> > last_place[kLast];

  vector<int> used_cols, used_rows, used_diag_l, used_diag_r;
  vector<vector<int> > used_cells;

  bool is_attacking(int y, int x, int type);
  void place_piece(int y, int x, int type, int dir);
  void search(int nb, ostream& os);
  void print(ostream& os);
};

void state_t::print(ostream& os) {
  const char names[] = "QRBKN";
  vector<string> b(height, string(width, '.'));
  for (vector<item_t>::iterator i = board.begin(); i != board.end(); ++i) {
    b[i->y][i->x] = names[i->type];
  }
  copy(b.begin(), b.end(), ostream_iterator<string>(os, "\n"));
  os << "\n";
}

struct delta_t {
  int dy, dx;
};

delta_t king_rules[] = {
  {-1,  0}, {+1,  0}, { 0, -1}, { 0, +1},
  {-1, -1}, {+1, +1}, {+1, -1}, {-1, +1},
};

delta_t knight_rules[] = {
  {-2, -1}, {-2, +1}, {+2, -1}, {+2, +1},
  {-1, -2}, {-1, +2}, {+1, -2}, {+1, +2},
};

bool state_t::is_attacking(int y, int x, int type) {
  for (vector<item_t>::iterator i = board.begin(); i != board.end(); ++i) {
    int cy = i->y, cx = i->x;

    if (type == kQueen || type == kRook)
      if (cy == y || cx == x) 
        return true;
    
    if (type == kQueen || type == kBishop)
      if (cy + cx == y + x || cy - cx == y - x) 
        return true;

    if (type == kKing || type == kKnight) {
      const delta_t* delta = type == kKing ? king_rules : knight_rules;
      for (int i = 0; i < 8; ++i)
        if (cy == y + delta[i].dy && cx == x + delta[i].dx) 
          return true;
    }
  }
  return false;
}

void state_t::place_piece(int y, int x, int type, int dir) {
  if (type == kRook || type == kQueen) {
    used_rows[y] += dir;
    used_cols[x] += dir;
  }

  if (type == kQueen || type == kBishop) {
    used_diag_l[y + x] += dir;
    used_diag_r[y - x + width] += dir;
  }

  if (type == kKing || type == kKnight) {
    const delta_t* delta = type == kKing ? king_rules : knight_rules;
    for (int i = 0; i < 8; ++i)
      used_cells[y + delta[i].dy + 2][x + delta[i].dx + 2] += dir;
  }

  used_cells[y + 2][x + 2] += dir;
}

void state_t::search(int nb, ostream& os) {
  if (nb == pieces.size()) {
    count += 1;
    if (height <= 4 && width <= 4)
      print(os);
    return;
  }
  int type = pieces[nb];
  int fy = last_place[type].back().first;
  int fx = last_place[type].back().second;

  for (int y = fy; y < height; ++y) {
    if (used_rows[y] == 0)
      for (int x = fx; x < width; ++x) {
        if (used_cells[y + 2][x + 2]) continue;
        if (used_cols[x]) continue;
        if (used_diag_l[y + x] || used_diag_r[y - x + width]) continue;

        if (is_attacking(y, x, type)) continue;

        place_piece(y, x, type, 1);

        board.push_back(item_t(y, x, type));
        last_place[type].push_back(make_pair(y, x));
       
        search(nb + 1, os);

        last_place[type].pop_back();
        board.pop_back();

        place_piece(y, x, type, -1);
      }
    fx = 0;
  }
}

// ------------------------------------------------------

void solve(istream& is, ostream& os) {
  state_t state;
  state.count = 0;
  
  is >> state.height >> state.width;

  int n;
  is >> n; for (int i = 0; i < n; ++i) state.pieces.push_back(kKing);
  is >> n; for (int i = 0; i < n; ++i) state.pieces.push_back(kQueen);
  is >> n; for (int i = 0; i < n; ++i) state.pieces.push_back(kBishop);
  is >> n; for (int i = 0; i < n; ++i) state.pieces.push_back(kRook);
  is >> n; for (int i = 0; i < n; ++i) state.pieces.push_back(kKnight);

  state.used_rows.resize(state.height, 0);
  state.used_cols.resize(state.width, 0);
  state.used_diag_l.resize(state.height + state.width, 0);
  state.used_diag_r.resize(state.height + state.width, 0);
  state.used_cells = vector<vector<int> >(state.height + 4, 
                                          vector<int>(state.width + 4, 0));

  for (int i = 0; i < kLast; ++i)
    state.last_place[i].push_back(make_pair(0, 0));

  state.search(0, os);
  os << state.count << endl;
}

#undef int

const char* liner = "--------";
int problem;
int problem_filter;

#ifdef int
#undef int
#endif

void run(int line, const char* input, const char* output) {
  problem += 1;
  if (problem_filter != -1 && problem_filter != problem) return;

  stringstream is(input);
  stringstream os;
  solve(is, os);
  if (os.str() != output) {
    cerr << "Case #" << problem << ": FAILED (line " << line << ")" << endl;
#ifdef _DEBUG
    stringstream error;
    error << __FILE__ << "(" << line << "): error: test case " << problem << " FAILED" << endl;
    OutputDebugStringA(error.str().c_str());  
#endif
    cerr << liner << "EXPECTED" << liner << endl << output;
    cerr << liner << "-ACTUAL-" << liner << endl << os.str() 
         << liner << liner << liner << endl;
  } else 
    cerr << "Case #" << problem << " OK (line " << line << ")" << endl;
}

int main(int argc, char* argv[]) {
#ifdef TESTING_

problem = -1;
problem_filter = -1;

run(__LINE__,
"2 2\n"
"0 0 0 2 0\n"
,
"R.\n"
".R\n"
"\n"
".R\n"
"R.\n"
"\n"
"2\n"
);

run(__LINE__,
"3 3\n"
"2 0 0 1 0\n"
,
"K.K\n"
"...\n"
".R.\n"
"\n"
"K..\n"
"..R\n"
"K..\n"
"\n"
"..K\n"
"R..\n"
"..K\n"
"\n"
".R.\n"
"...\n"
"K.K\n"
"\n"
"4\n"
);

run(__LINE__,
"4 4\n"
"0 0 0 2 4\n"
,
"R...\n"
".N.N\n"
"..R.\n"
".N.N\n"
"\n"
".R..\n"
"N.N.\n"
"...R\n"
"N.N.\n"
"\n"
"..R.\n"
".N.N\n"
"R...\n"
".N.N\n"
"\n"
"...R\n"
"N.N.\n"
".R..\n"
"N.N.\n"
"\n"
".N.N\n"
"R...\n"
".N.N\n"
"..R.\n"
"\n"
"N.N.\n"
".R..\n"
"N.N.\n"
"...R\n"
"\n"
".N.N\n"
"..R.\n"
".N.N\n"
"R...\n"
"\n"
"N.N.\n"
"...R\n"
"N.N.\n"
".R..\n"
"\n"
"8\n"
);

run(__LINE__,
"7 7\n"
"2 2 2 0 1\n"
,
"3063828\n"
);

#ifdef _DEBUG
getchar();
#endif

#else

ifstream is("INPUT.TXT");
ofstream os("OUTPUT.TXT");
// istream& is = cin;
// ostream& os = cout;

solve(is, os);

#endif

return 0;
}


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
  vector<pair<int, int> > last[kLast];
};

// ------------------------------------------------------

void print_state(const state_t& state, ostream& os) {
  const char names[] = "QRBKN";
  vector<string> b(state.height, string(state.width, '.'));
  for (vector<item_t>::const_iterator i = state.board.begin(); i != state.board.end(); ++i) {
    b[i->y][i->x] = names[i->type];
  }
  copy(b.begin(), b.end(), ostream_iterator<string>(os, "\n"));
  os << "\n";
}

struct {
  int dy, dx;
} king_rules[] = {
  {-1,  0}, {+1,  0}, { 0, -1}, { 0, +1},
  {-1, -1}, {+1, +1}, {+1, -1}, {-1, +1},
};

struct {
  int dy, dx;
} knight_rules[] = {
  {-2, -1}, {-2, +1}, {+2, -1}, {+2, +1},
  {-1, -2}, {-1, +2}, {+1, -2}, {+1, +2},
};

bool is_under_attack_or_attacking(int y, int x, int type, state_t& state) {
  for (vector<item_t>::const_iterator i = state.board.begin(); i != state.board.end(); ++i) {
    int cy = i->y, cx = i->x, ctype = i->type;

    if (cy == y && cx == x) return true;

    if (ctype == kQueen || type == kQueen)
      if (cy + cx == y + x || cy - cx == y - x || cy == y || cx == x) 
        return true;
    
    if (ctype == kBishop || type == kBishop)
      if (cy + cx == y + x || cy - cx == y - x) 
        return true;
    
    if (ctype == kRook || type == kRook) {
      if (cy == y || cx == x) 
        return true;
    }
    
    if (ctype == kKing)
      for (int i = 0; i < 8; ++i)
        if (cy + king_rules[i].dy == y && cx + king_rules[i].dx == x) 
          return true;
    
    if (ctype == kKnight)
      for (int i = 0; i < 8; ++i)
        if (cy + knight_rules[i].dy == y && cx + knight_rules[i].dx == x) 
          return true;

    if (type == kKing)
      for (int i = 0; i < 8; ++i)
        if (cy + king_rules[i].dy == y && cx + king_rules[i].dx == x) 
          return true;
    
    if (type == kKnight)
      for (int i = 0; i < 8; ++i)
        if (cy == y + knight_rules[i].dy && cx == x + knight_rules[i].dx) 
          return true;
  }

  return false;
}

/*
bool is_under_attack(int y, int x, state_t& state) {
  for (vector<item_t>::const_iterator i = state.board.begin(); i != state.board.end(); ++i) {
    int cy = i->y, cx = i->x, type = i->type;

    if (cy == y && cx == x) return true;

    if (type == kQueen) {
      // Queens
      if (cy + cx == y + x) return true;
      if (cy - cx == y - x) return true;
      if (cy == y || cx == x) return true;
    } else if (type == kBishop) {
      // Bishops
      if (cy + cx == y + x) return true;
      if (cy - cx == y - x) return true;
    } else if (type == kRook) {
      // Rooks
      if (cy == y || cx == x) return true;
    } else if (type == kKing) {
      // Kings
      for (int i = 0; i < 8; ++i) {
        if (cy + king_rules[i].dy == y && cx + king_rules[i].dx == x) return true;
      }
    } else if (type == kKnight) {
      // Knights
      for (int i = 0; i < 8; ++i) {
        if (cy + knight_rules[i].dy == y && cx + knight_rules[i].dx == x) return true;
      }
    }
  }

  return false;
}

bool is_attacking(int y, int x, int type, state_t& state) {
  for (vector<item_t>::const_iterator i = state.board.begin(); i != state.board.end(); ++i) {
    int cy = i->y, cx = i->x;

    if (cy == y && cx == x) return true;

    if (type == kQueen) {
      // Queens
      if (cy + cx == y + x) return true;
      if (cy - cx == y - x) return true;
      if (cy == y || cx == x) return true;
    } else if (type == kBishop) {
      // Bishops
      if (cy + cx == y + x) return true;
      if (cy - cx == y - x) return true;
    } else if (type == kRook) {
      // Rooks
      if (cy == y || cx == x) return true;
    } else if (type == kKing) {
      // Kings
      for (int i = 0; i < 8; ++i) {
        if (cy + king_rules[i].dy == y && cx + king_rules[i].dx == x) return true;
      }
    } else if (type == kKnight) {
      // Knights
      for (int i = 0; i < 8; ++i) {
        if (cy == y + knight_rules[i].dy && cx == x + knight_rules[i].dx) return true;
      }
    }
  }

  return false;
}
*/

void search(int nb, state_t& state, ostream& os) {
  if (nb == state.pieces.size()) {
    state.count += 1;
    if (state.height <= 4 && state.width <= 4)
      print_state(state, os);
    return;
  }
  int type = state.pieces[nb];
  int fy = state.last[type].back().first;
  int fx = state.last[type].back().second;

  for (int y = fy; y < state.height; ++y) {
    for (int x = fx; x < state.width; ++x) {
      if (is_under_attack_or_attacking(y, x, type, state)) continue;
      state.board.push_back(item_t(y, x, type));
      state.last[type].push_back(make_pair(y, x));
      search(nb + 1, state, os);
      state.board.pop_back();
      state.last[type].pop_back();
    }
    fx = 0;
  }
}

// ------------------------------------------------------

void solve(istream& is, ostream& os) {
  state_t state;
  state.count = 0;
  string pieces;
  is >> state.height >> state.width >> pieces;
  for (string::const_iterator i = pieces.begin(); i != pieces.end(); ++i) {
    if (*i == 'Q') state.pieces.push_back(kQueen);
    else if (*i == 'R') state.pieces.push_back(kRook);
    else if (*i == 'B') state.pieces.push_back(kBishop);
    else if (*i == 'K') state.pieces.push_back(kKing);
    else if (*i == 'N') state.pieces.push_back(kKnight);
  }
  
  for (int i = 0; i < kLast; ++i)
    state.last[i].push_back(make_pair(0, 0));

  search(0, state, os);
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
"RR\n"
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
"RKK\n"
,
".R.\n"
"...\n"
"K.K\n"
"\n"
"..K\n"
"R..\n"
"..K\n"
"\n"
"K..\n"
"..R\n"
"K..\n"
"\n"
"K.K\n"
"...\n"
".R.\n"
"\n"
"4\n"
);

run(__LINE__,
"4 4\n"
"RRNNNN\n"
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
"QQBBKKN\n"
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


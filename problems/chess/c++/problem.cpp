#include <iostream>
#include <vector>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <iterator>
#include <algorithm>
#include <string>
#ifdef _DEBUG
#include <windows.h>
#endif
using namespace std;

#define TESTING_

enum piece_t {
  kQueen = 0, kRook = 1, kBishop = 2, kKing = 3, kKnight = 4, kLast
};

struct item_t {
  item_t(int ay, int ax, int atype) : y(ay), x(ax), type(atype) {}
  int y, x, type;
};

typedef vector<item_t> items_t;

struct state_t {
  items_t board;
  int height;
  int width;
  int count;
  vector<int> pieces;
  vector<pair<int, int> > last_place[kLast];

  vector<int> attacked_cols, attacked_rows, attacked_diag_l, attacked_diag_r;
  vector<int> have_target_cols, have_target_rows, have_target_diag_l, have_target_diag_r;

  vector<vector<int> > attacked_cells;
  vector<vector<int> > no_kings_cells;
  vector<vector<int> > no_knights_cells;

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

void state_t::place_piece(int y, int x, int type, int dir) {
  have_target_rows[y] += dir; 
  have_target_cols[x] += dir; 
  have_target_diag_l[y + x] += dir; 
  have_target_diag_r[y - x + width] += dir; 

  for (int i = 0; i < 8; ++i) {
    no_kings_cells[y + king_rules[i].dy + 2][x + king_rules[i].dx + 2] += dir;
    no_knights_cells[y + knight_rules[i].dy + 2][x + knight_rules[i].dx + 2] += dir;
  }

  if (type == kRook || type == kQueen) {
    attacked_rows[y] += dir;
    attacked_cols[x] += dir;
  }

  if (type == kQueen || type == kBishop) {
    attacked_diag_l[y + x] += dir;
    attacked_diag_r[y - x + width] += dir;
  }

  if (type == kKing || type == kKnight) {
    const delta_t* delta = type == kKing ? king_rules : knight_rules;
    for (int i = 0; i < 8; ++i)
      attacked_cells[y + delta[i].dy + 2][x + delta[i].dx + 2] += dir;
  }

  attacked_cells[y + 2][x + 2] += dir;
}

void state_t::search(int nb, ostream& os) {
  if (nb == (int)pieces.size()) {
    count += 1;
    if (height <= 4 && width <= 4)
      print(os);
    return;
  }
  int type = pieces[nb];
  int fy = last_place[type].back().first;
  int fx = last_place[type].back().second;

  for (int y = fy; y < height; ++y) {
    if (attacked_rows[y] == 0 && !((type == kQueen || type == kRook) && have_target_rows[y]))
      for (int x = fx; x < width; ++x) {
        // Skip if the cell is attacked (by a king/knight or just occupied).
        if (attacked_cells[y + 2][x + 2]) continue;
        // Skip if the column is attacked (by a queen or a rook).
        if (attacked_cols[x]) continue;
        // Skip if the diagonal is attacked (by a queen or a bishop).
        if (attacked_diag_l[y + x] || attacked_diag_r[y - x + width]) continue;
        // Skip if the column has a target on it and we are placing a queen or a rook.
        if ((type == kQueen || type == kRook) && have_target_cols[x]) continue;
        // Skip if the diagonal has target on it and we are placing a queen or a bishop.
        if ((type == kQueen || type == kBishop) && (have_target_diag_l[y + x] || have_target_diag_r[y - x + width])) continue;
        // Skip if we are placing a king and its attacked cells have a target on them.
        if (type == kKing && no_kings_cells[y + 2][x + 2]) continue;
        // Skip if we are placing a knight and its attacked cells have a target on them.
        if (type == kKnight && no_knights_cells[y + 2][x + 2]) continue;

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

  state.attacked_rows.resize(state.height, 0);
  state.attacked_cols.resize(state.width, 0);
  state.attacked_diag_l.resize(state.height + state.width, 0);
  state.attacked_diag_r.resize(state.height + state.width, 0);

  state.have_target_rows.resize(state.height, 0);
  state.have_target_cols.resize(state.width, 0);
  state.have_target_diag_l.resize(state.height + state.width, 0);
  state.have_target_diag_r.resize(state.height + state.width, 0);

  state.attacked_cells = 
    vector<vector<int> >(state.height + 4, 
                         vector<int>(state.width + 4, 0));

  state.no_kings_cells = 
    vector<vector<int> >(state.height + 4, 
                         vector<int>(state.width + 4, 0));

  state.no_knights_cells = 
    vector<vector<int> >(state.height + 4, 
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

#include <ctime>

void run(int line, const char* input, const char* output) {
  problem += 1;
  if (problem_filter != -1 && problem_filter != problem) return;

  stringstream is(input);
  stringstream os;
  clock_t started = clock();
  solve(is, os);
  clock_t elapsed = clock() - started;
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
  } else {
    cerr << "Case #" << problem << " OK (line " << line << ")";
    if (elapsed > CLOCKS_PER_SEC / 200)
      cerr << " time " << elapsed * (1.0/CLOCKS_PER_SEC) << "s";
    cerr << endl;
  }
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

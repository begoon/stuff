package main

import (
  "fmt"
)

// This object represents a position on the board.
type xy struct {
  x, y int
}

func NewXY (x, y int) *xy {
  v := new(xy)
  v.x = x
  v.y = y
  return v
}

// This object represents a position on the board occupied by a piece.
type item_t struct {
  x    int
  y    int
  kind int
}

func NewItem (x, y, kind int) *item_t {
  v := new(item_t)
  v.x = x
  v.y = y
  v.kind = kind
  return v
}

// This structure represents the state during a recursive search.
type state_t struct {
  height, width int       // Board dimentions.
  count         int       // Number of positions found.
  board         []item_t  // List of currently allocated pieces.
  board_index   int       // Index in the list of allocated pieces.
  pieces        []int     // List of given pieces to allocate.
  last_xy       [][]xy    // List of last used position per every type of piece.
  last_index    []int     // Index in the list of last positions.
  used_cols     []int     // Occupied columns.
  used_rows     []int     // Occupied rows.
  used_diag_l   []int     // Occupied diagonals.
  used_diag_r   []int     
  used_cells    []int     // Map of cells "under attack".
  
  king_rules    [8]xy
  knight_rules  [8]xy
}

const (
  kKing    = 0
  kQueen   = 1
  kBishop  = 2
  kRook    = 3
  kKnight  = 4
  kNbTypes = 5
  
  kNames = "KQBRN"
)

// Initializaion rountine.
func NewState(height, width int, pieces []int) *state_t {
  v := new(state_t)
  v.height = height
  v.width = width
  v.count = 0
  v.board = make([]item_t, len(pieces))
  v.board_index = 0
  
  v.pieces = pieces
  v.last_xy = make([][]xy, kNbTypes)
  v.last_index = make([]int, cap(v.last_xy))
  for i := range v.last_xy {
    v.last_xy[i] = make([]xy, len(pieces) + 1)
  }
  
  v.used_cols = make([]int, width)
  v.used_rows = make([]int, height)
  v.used_diag_l = make([]int, height+width)
  v.used_diag_r = make([]int, height+width)
  v.used_cells = make([]int, (height+4)*(width+4))
  
  v.king_rules = [8]xy {
    xy{-1,  0}, xy{+1,  0}, xy{ 0, -1}, xy{ 0, +1},
    xy{-1, -1}, xy{+1, +1}, xy{+1, -1}, xy{-1, +1},
  }
  
  v.knight_rules = [8]xy {
    xy{-2, -1}, xy{-2, +1}, xy{+2, -1}, xy{+2, +1},
    xy{-1, -2}, xy{-1, +2}, xy{+1, -2}, xy{+1, +2},
  }
  
  return v
}

// This function prints out a found position.
func (t *state_t) print() {
  p := make([]uint8, t.width * t.height)
  for i := range p {
    p[i] = '.'
  }

  for i := 0; i < t.board_index; i += 1 {
    v := t.board[i]
    p[v.y * t.width + v.x] = kNames[v.kind]
  }
  
  for i, v := range p {
    fmt.Printf("%c", v)
    if i > 0 && (i + 1) % t.width == 0 {
      fmt.Printf("\n")
    }
  }
  
  fmt.Printf("\n")
}

// This function checks whether the piece is attacking other units from this position.
func (t *state_t) is_attacking(y, x, kind int) bool {
  for i := 0; i < t.board_index; i += 1 {
    cy := t.board[i].y
    cx := t.board[i].x
    
    if (kind == kQueen || kind == kRook) {
      if (cy == y || cx == x) {
        return true;
      }
    }
    
    if (kind == kQueen || kind == kBishop) {
      if (cy + cx == y + x || cy - cx == y - x) {
        return true;
      }
    }

    if (kind == kKing || kind == kKnight) {
      delta := &t.king_rules
      if kind == kKnight {
        delta = &t.knight_rules
      }
      for _, g := range *delta {
        if (cy == y + g.y && cx == x + g.x) {
          return true;
        }
      }
    }
  }
  return false
}

// This function marks the board when we are placing next piece.
func (t *state_t) place_piece(y, x, kind, dir int) {
  // Mark rows and columns.
  if (kind == kRook || kind == kQueen) {
    t.used_rows[y] += dir;
    t.used_cols[x] += dir;
  }

  // Mark diagonals.
  if (kind == kQueen || kind == kBishop) {
    t.used_diag_l[y + x] += dir;
    t.used_diag_r[y - x + t.width] += dir;
  }

  // Mark King's and Knight's attacks.
  if (kind == kKing || kind == kKnight) {
    delta := &t.king_rules
    if kind == kKnight {
      delta = &t.knight_rules
    }
    for _, g := range *delta {
      t.used_cells[(y + g.y + 2) * (t.width + 4) + (x + g.x + 2)] += dir;
    }
  }

  // Mark the cell itself.
  t.used_cells[(y + 2) * (t.width + 4) + (x + 2)] += dir;
  
  // Put a piece to the list of already placed units.
  if (dir == 1) {
    t.board[t.board_index].y = y
    t.board[t.board_index].x = x
    t.board[t.board_index].kind = kind
  }
  
  t.board_index += dir;
        
  // Save this position as the last used for this type of pieces.
  t.last_index[kind] += dir;
  if (dir == 1) {
    t.last_xy[kind][t.last_index[kind]].y = y
    t.last_xy[kind][t.last_index[kind]].x = x
  }
}

// Recursive search function.
func (t *state_t) search(nb int) {
  if (nb == len(t.pieces)) {
    // Found!
    t.count += 1;
    if (t.height <= 4 && t.width <= 4) {
      t.print();
    }
    return;
  }
  
  kind := t.pieces[nb]
  
  // Load the last used position for this type of piecess to avoid generating
  // duplicated positions.
  last_index := t.last_index[kind];
  fy := t.last_xy[kind][last_index].y
  fx := t.last_xy[kind][last_index].x

  for y := fy; y < t.height; y += 1 {
    // Skip if row is occupied.
    if t.used_rows[y] == 0 {
      for x := fx; x < t.width; x += 1 {
        // Skip if cell is occupied.
        if t.used_cells[(y + 2) * (t.width + 4) + (x + 2)] > 0 { 
          continue; 
        }
        
        // Skip if column is occupied.
        if t.used_cols[x] > 0 { 
          continue; 
        }
        
        // Skip if diagonals are occupied.
        if t.used_diag_l[y + x] > 0 || t.used_diag_r[y - x + t.width] > 0 { 
          continue; 
        }

        // Skip if the current piece attacks already placed units.
        if t.is_attacking(y, x, kind) { 
          continue; 
        }
        
        // Mark the board.
        t.place_piece(y, x, kind, 1);
        
        t.search(nb + 1);
        
        // Un-mark the board.
        t.place_piece(y, x, kind, -1);
      }
    }
    fx = 0;
  }
}

// The program reads data from the standard input.
func main() {
  height := 0	
  width := 0
  items := make([]int, 0, 100)

  // Repack input data into internal format.  
  fmt.Scanf("%d %d", &height, &width);
  for _, kind := range []int{kKing, kQueen, kBishop, kRook, kKnight} {
    var v int
    fmt.Scanf("%d", &v)
    items = items[0:len(items) + v]
    for j := 0; j < v; j += 1 {
      items[len(items) - v + j] = kind
    }
  }
  
  // Solve.
  state := NewState(height, width, items)
  state.search(0)
  fmt.Printf("%d\n\n", state.count);
}


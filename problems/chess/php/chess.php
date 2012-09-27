#!/opt/swt/bin/php
<?php

function readline() {
  fgets(STDIN);
}

function print_state(&$state) {
  $board = array_fill(0, $state["height"], str_repeat(".", $state["width"]));
  foreach($state["board"] as $item) {
    $board[$item[0]][$item[1]] = $item[2];
  }
  echo join("\n", $board), "\n\n";
}

$king_rules = array(
  array(-1,  0), array(+1,  0), array( 0, -1), array( 0, +1),
  array(-1, -1), array(+1, +1), array(+1, -1), array(-1, +1),
);

$knight_rules = array(
  array(-2, -1), array(-2, +1), array(+2, -1), array(+2, +1),
  array(-1, -2), array(-1, +2), array(+1, -2), array(+1, +2),
);

function is_under_attack($y, $x, &$state) {
  global $king_rules, $knight_rules;

  foreach($state["board"] as $item) {
    $cy = $item[0];
    $cx = $item[1];
    $type = $item[2];

    if ($cy == $x && $cx == $x) return true;

    if ($type == "Q") {
      // Queens
      if ($cy + $cx == $y + $x) return true;
      if ($cy - $cx == $y - $x) return true;
      if ($cy == $y || $cx == $x) return true;
    } else if ($type == "B") {
      // Bishops
      if ($cy + $cx == $y + $x) return true;
      if ($cy - $cx == $y - $x) return true;
    } else if ($type == "R") {
      // Rooks
      if ($cy == $y || $cx == $x) return true;
    } else if ($type == "K") {
      // Kings
      foreach($king_rules as $p) {
        if ($cy + $p[0] == $y && $cx + $p[1] == $x) return true;
      }
    } else if ($type == "N") {
      // Knights
      foreach($knight_rules as $p) {
        if ($cy + $p[0] == $y && $cx + $p[1] == $x) return true;
      }
    }
  }

  return false;
}

function is_attacking($y, $x, $type, &$state) {
  global $king_rules, $knight_rules;

  foreach($state["board"] as $item) {
    $cy = $item[0];
    $cx = $item[1];

    if ($cy == $x && $cx == $x) return true;

    if ($type == "Q") {
      // Queens
      if ($cy + $cx == $y + $x) return true;
      if ($cy - $cx == $y - $x) return true;
      if ($cy == $y || $cx == $x) return true;
    } else if ($type == "B") {
      // Bishops
      if ($cy + $cx == $y + $x) return true;
      if ($cy - $cx == $y - $x) return true;
    } else if ($type == "R") {
      // Rooks
      if ($item[0] == $y || $item[1] == $x) return true;
    } else if ($type == "K") {
      // Kings
      foreach($king_rules as $p) {
        if ($cy == $y + $p[0] && $cx == $x + $p[1]) return true;
      }
    } else if ($type == "N") {
      // Knights
      foreach($knight_rules as $p) {
        if ($cy == $y + $p[0] && $cx == $x + $p[1]) return true;
      }
    }
  }

  return false;
}

$state = array(
  "board" => array(),
  "height" => 2,
  "width" => 2,
  "count" => 0,
  "items" => "RK",
);

// print_state($state);

// var_dump(is_attacking(2, 0, "K", $state));

function search($nb, &$state) {
  if ($nb == strlen($state["items"])) {
    print_state($state);
    echo count($state["board"]), "\n";
    $state["count"] += 1;
  } else {
    for ($y = 0; $y < $state["height"]; ++$y) {
      for ($x = 0; $x < $state["width"]; ++$x) {
        if (is_under_attack($y, $x, $state)) continue;
        $type = $state["items"][$nb];
        if (is_attacking($y, $x, $type, $state)) continue;
        array_push($state["board"], array($y, $x, $type));
        search($nb + 1, $state);
        array_pop($state["board"]);
      }
    }
  }
}

search(0, $state);
?>


#!/opt/swt/bin/php
<?php

function readline() {
  fgets(STDIN);
}

function print_state(&$state) {
  $board = array_fill(0, $state["height"], str_repeat(".", $state["width"]));
  $types = array("K", "Q", "B", "R", "N");
  foreach($types as $type) {
    foreach($state[$type] as $item) {
      $y = $item[0];
      $x = $item[1];
      $board[$y][$x] = $type;
    }
  }
  echo join("\n", $board), "\n";
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

  // Queens

  foreach($state["Q"] as $item) {
    $cy = $item[0];
    $cx = $item[1];
    if ($cy + $cx == $y + $x) return true;
    if ($cy - $cx == $y - $x) return true;
    if ($cy == $y || $cx == $x) return true;
  }

  // Bishops

  foreach($state["B"] as $item) {
    $cy = $item[0];
    $cx = $item[1];
    if ($cy + $cx == $y + $x) return true;
    if ($cy - $cx == $y - $x) return true;
  }

  // Rooks

  foreach($state["R"] as $item) {
    if ($item[0] == $y || $item[1] == $x) return true;
  }

  // King

  foreach($state["K"] as $item) {
    foreach($king_rules as $p) {
      if ($item[0] + $p[0] == $y && $item[1] + $p[1] == $x)
        return true;
    }
  }

  // Knight

  foreach($state["N"] as $item) {
    foreach($knight_rules as $p) {
      if ($item[0] + $p[0] == $y && $item[1] + $p[1] == $x)
        return true;
    }
  }

  return false;
}

function is_attacking($y, $x, &$state, $type) {
  global $king_rules, $knight_rules;

  $types = array("K", "Q", "B", "R", "N");

  if ($type == "Q") {
  }
}

$state = array(
  "K" => array(),
  "Q" => array(array(3,4)),
  "B" => array(),
  "R" => array(),
  "N" => array(),
  "height" => 7,
  "width" => 7,
  "count" => 0,
  "items" => "RKK",
);

print_state($state);

// var_dump(is_under_attack(6, 0, $state));

function search($nb, &$state) {
  if ($nb == strlen($state["items"])) {
    print_state($state);
    $state["count"] += 1;
  }

}

?>


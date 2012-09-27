#!/opt/swt/bin/php
<?php

function readline() {
  fgets(STDIN);
}

$king_rules = array(
  array(-1,  0), array(+1,  0),
  array( 0, -1), array( 0, +1),
  array(-1, -1), array(+1, +1),
  array(+1, -1), array(-1, +1),
);

$bishop_rules = array(
  array(-1, -1), array(+1, +1),
  array(+1, -1), array(-1, +1),
);

$rook_rules = array(
  array(-1,  0), array(+1,  0),
  array( 0, -1), array( 0, +1),
);

$knight_rules = array(
  array(-2, -1), array(-2, +1),
  array(+2, -1), array(+2, +1),
  array(-1, -2), array(-1, +2),
  array(+1, -2), array(+1, +2),
);

function precalc_takes($h, $w, $rules, $steps = false) {
  $result = array();
  for ($y = 0; $y < $h; ++$y) {
    for ($x = 0; $x < $w; ++$x) {
      $cells = array();
      foreach($rules as $p) {
        $ny = $y;
        $nx = $x;
        do {
          $ny = $ny + $p[0];
          $nx = $nx + $p[1];
          if ($ny < 0 || $ny >= $h || $nx < 0 || $nx >= $w) break;
          array_push($cells, array($ny, $nx));
        } while ($steps);
      }
      $result[$y][$x] = $cells;
    }
  }
  return $result;
}

$w = 7;
$h = 7;

// King (K), Queen (Q), Bishop (B), Rook (R), Knight (N)

$items = "QQBBKKN";

$takes["K"] = precalc_takes($h, $w, $king_rules);
$takes["Q"] = precalc_takes($h, $w, $king_rules, true);
$takes["R"] = precalc_takes($h, $w, $rook_rules, true);
$takes["B"] = precalc_takes($h, $w, $bishop_rules, true);
$takes["N"] = precalc_takes($h, $w, $knight_rules);

function mark_takes($y, $x, &$field, &$takes, $direction, &$layout, $item) {
  $field[$y][$x] = $field[$y][$x] + $direction;
  $layout[$y][$x] = $field[$y][$x] > '0' ? $item : ".";
  assert($field[$y][$x] >= '0');
  foreach ($takes[$y][$x] as $take) {
    $py = $take[0];
    $px = $take[1];
    $field[$py][$px] = $field[$py][$px] + $direction;
    assert($field[$py][$px] >= '0');
  }
}

function is_attack($y, $x, &$takes, &$layout) {
  foreach ($takes[$y][$x] as $take) {
    $py = $take[0];
    $px = $take[1];
    if ($layout[$py][$px] != '.') return true;
  }
  return false;
}

$found = array();

function search(&$field, $pos, &$items, $h, $w, &$takes, &$layout, &$count) {
  global $found;
  if ($pos == strlen($items)) {
    if (!in_array($layout, $found)) {
      array_push($found, $layout);
      foreach($layout as $line) echo "$line\n";
      $count += 1;
      echo "\n";
    }
  } else {
    for ($y = 0; $y < $h; ++$y) {
      for ($x = 0; $x < $w; ++$x) {
        if ($field[$y][$x] != '0') continue;
        $type = $items[$pos];
        if (is_attack($y, $x, $takes[$type], $layout)) continue;
        mark_takes($y, $x, $field, $takes[$type], 1, $layout, $type);
        search($field, $pos + 1, $items, $h, $w, $takes, $layout, $count);
        mark_takes($y, $x, $field, $takes[$type], -1, $layout, $type);
      }
    }
  }
}

// mark_takes(0, 6, $s, $king_precalced_takes);
// mark_takes(0, 0, $s, $queen_precalced_takes);
// mark_takes(3, 3, $s, $rook_precalced_takes);
// mark_takes(0, 0, $s, $knight_precalced_takes);
// mark_takes(3, 3, $s, $takes["B"]);
// var_dump(

$field = array_fill(0, $h, str_repeat("0", $w));
$layout = array_fill(0, $h, str_repeat(".", $w));

$count = 0;
search($field, 0, $items, $h, $w, $takes, $layout, $count);

echo $count;
?>


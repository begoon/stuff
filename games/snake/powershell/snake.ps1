function Clear-Screen { Clear-Host }
function Reset-Cursor { [System.Console]::SetCursorPosition(0, 0) }
function Is-Key-Pressed { return $Host.UI.RawUI.KeyAvailable }
function Read-Key() { return $Host.UI.RawUI.ReadKey().Character }

function Initialize-Map {
  1..$script:height | ForEach { $script:map.Add("".PadRight($script:width, " ")) }
}

function Draw-Field {
  Reset-Cursor
  $line = "+" + "".PadRight($width, "-") + "+" | Write-Host
  $script:map | ForEach { 
    Write-Host "|" -NoNewline
    Write-Host $_ -NoNewline
    Write-Host "|"
  }
  $line = "+" + "".PadRight($width, "-") + "+" | Write-Host
}

function String-Put($s, $i, $c) {
  $r = $s.Substring(0, $i) + $c
  if (($i+1) -lt $s.Length) { $r += $s.Substring($i + 1) }
  return $r
}

function Map-Put($x, $y, $c = " ") {
  $script:map[$y] = String-Put ($script:map[$y]) $x $c
}

function Clear-Snake {
  $script:snake | ForEach { Map-Put $_[0] $_[1] }
}

function Draw-Snake($dx, $dy) {
  $first = $true
  $script:snake | ForEach { 
    $c = "@"
    if ($first) {
      if ($dx -eq 1 -and $dy -eq 0) { $c = ">" }
      elseif ($dx -eq -1 -and $dy -eq  0) { $c = "<" }
      elseif ($dx -eq  0 -and $dy -eq +1) { $c = "V" }
      elseif ($dx -eq  0 -and $dy -eq -1) { $c = "^" }
      $first = $false
    }
    Map-Put $_[0] $_[1] $c
  }
}

function Place-Rabbit {
  while ($true) {
    $x = Get-Random -Minimum 0 -Maximum ($script:width-1)
    $y = Get-Random -Minimum 0 -Maximum ($script:height-1)
    if ($script:map[$y][$x] -eq " ") { 
      Map-Put $x $y "+"
      return @($x, $y)
    }
  }
}

function Is-Hit-Snake($x, $y) {
  return $script:snake | Where-Object { $_[0] -eq $x -and $_[1] -eq $y }
}

function Move-Snake($dx, $dy) {
  Clear-Snake

  $x = $snake[0][0] + $dx
  $y = $snake[0][1] + $dy
  
  if ($x -lt 0) { $x = $script:width - 1 }
  elseif ($x -ge $script:width) { $x = 0 }
  
  if ($y -lt 0) { $y = $script:height - 1 }
  elseif ($y -ge $script:height) { $y = 0 }

  if (Is-Hit-Snake $x $y) {
     Write-Host "Bang!"
     Exit
  }
  
  $script:snake.Insert(0, @($x, $y))
  
  if ($script:growth -eq 0) {
    $script:snake.RemoveAt($script:snake.Count-1)
  } else {
    --$script:growth
  }

  if ($x -eq $script:rabbit[0] -and $y -eq $script:rabbit[1]) {
    $script:growth = $script:increase
    $script:increase *= 2
    $script:rabbit = Place-Rabbit
  }
  
  Draw-Snake $dx $dy
}

function Game {
  $dx = 1
  $dy = 0

  do {
    if (Is-Key-Pressed) { $key = Read-Key }
    switch($key) {
      { $_ -eq 'w' -and $dx -ne  0 -and $dy -ne +1 } { $dx =  0; $dy = -1; break }
      { $_ -eq 's' -and $dx -ne  0 -and $dy -ne -1 } { $dx =  0; $dy = +1; break }
      { $_ -eq 'a' -and $dx -ne +1 -and $dy -ne  0 } { $dx = -1; $dy =  0; break }
      { $_ -eq 'd' -and $dx -ne -1 -and $dy -ne  0 } { $dx = +1; $dy =  0; break }
    }
    Clear-Snake
    Move-Snake $dx $dy
    Draw-Snake $dx $dy
    Draw-Field
    Start-Sleep -Milliseconds 100
  } while ($key -ne 'q')
}

# -----------------------------------------------------------------------------------------

$width = 40
$height = 20

$map = New-Object System.Collections.ArrayList

$snake = New-Object System.Collections.ArrayList
$snake.AddRange((@(12, 10), @(11, 10), @(10, 10)))

$increase = 2
$growth = 0

Initialize-Map
$rabbit = Place-Rabbit

Clear-Screen
Game

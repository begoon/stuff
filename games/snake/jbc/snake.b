  snake_size = 1
  change_x = 0
  change_y = 0
  DIM coordinates_x(1000)
  MAT coordinates_x = 0
  DIM coordinates_y(1000)
  MAT coordinates_y = 0
  DIM a(100, 100)
  MAT a = ' '
  food_x = -1
  food_y = -1
  N = 13
  M = 17
  speed = 0.3
  game_over = 0
  increase = 0

  GOSUB standart_settings

  LOOP
    IF SYSTEM(14) NE 0 THEN GOSUB change_direction
    GOSUB next_step
    IF (food_x EQ -1) AND (food_y EQ -1) THEN GOSUB place_food
    GOSUB show_table
    SLEEP speed
  REPEAT

standart_settings:
  snake_size = 2
  coordinates_x(1) = 1
  coordinates_y(1) = 2
  coordinates_x(2) = 1
  coordinates_y(2) = 1
  change_x = 0
  change_y = 1
  RETURN

place_food:
  FOR i=1 TO 9 
    x = RND(N) + 1
    y = RND(M) + 1
    c = a(x,y)
    IF (c NE '@') AND (c NE 'v') AND (c NE '^') AND (c NE '<') AND (c NE '>') 
    THEN
      food_x = x
      food_y = y
      a(x, y) = '+'
      RETURN
    END
  NEXT
  RETURN

next_step:
  GOSUB clear_snake_on_table
  IF increase GT 0 THEN increase -= 1
  FOR i = snake_size TO 2 STEP -1
    coordinates_x(i) = coordinates_x(i - 1)
    coordinates_y(i) = coordinates_y(i - 1)
  NEXT
  coordinates_x(1) += change_x
  coordinates_y(1) += change_y
  GOSUB check_coordinates
  IF (coordinates_x(1) EQ food_x) AND (coordinates_y(1) EQ food_y) THEN
    snake_size++
    increase = 1
    food_x = -1
    food_y = -1
  END
  GOSUB show_snake_on_table
  GOSUB is_game_over
  IF game_over THEN
    CRT "You're looser!"
    EXIT(0)
  END
  RETURN

check_coordinates:
  IF coordinates_x(1) GT N THEN coordinates_x(1) = 1
  IF coordinates_x(1) LT 1 THEN coordinates_x(1) = N
  IF coordinates_y(1) GT M THEN coordinates_y(1) = 1
  IF coordinates_y(1) LT 1 THEN coordinates_y(1) = M
  RETURN

is_game_over:
  FOR i = 2 TO snake_size
    IF (coordinates_x(1) EQ coordinates_x(i)) AND \
       (coordinates_y(1) EQ coordinates_y(i)) THEN
      game_over = 1
      BREAK
    END
  NEXT
  RETURN

show_snake_on_table:
  c = '?'
  IF (change_x EQ 1) AND (change_y EQ 0) THEN c = 'v'
  IF (change_x EQ -1) AND (change_y EQ 0) THEN c = '^'
  IF (change_x EQ 0) AND (change_y EQ 1) THEN c = '>'
  IF (change_x EQ 0) AND (change_y EQ -1) THEN c = '<'
  a(coordinates_x(1), coordinates_y(1)) = c
  FOR i = 2 TO snake_size - increase
    a(coordinates_x(i), coordinates_y(i)) = '@'
  NEXT
  RETURN

clear_snake_on_table:
  FOR i = 1 TO snake_size - increase
    a(coordinates_x(i), coordinates_y(i)) = ' '
  NEXT
  RETURN

show_table:
  CRT @(-1):
  FOR i = 0 TO N + 1
    FOR j = 0 TO M + 1
      c = '.'
      IF (i EQ 0) OR (i EQ N+1) OR (j EQ 0) OR (j EQ M+1) THEN c = '#'
      IF c NE '#' THEN c = a(i, j)
      CRT c:
      IF j GT M THEN CRT
    NEXT
  NEXT
  RETURN

change_direction:
  c = KEYIN()
  INPUTCLEAR
  IF (c EQ 'w') AND (change_x NE 1) AND (change_y NE 0) THEN
    change_x = -1; change_y = 0
  END
  IF (c EQ 'a') AND (change_x NE 0) AND (change_y NE 1) THEN
    change_x = 0; change_y = -1
  END
  IF (c EQ 's') AND (change_x NE -1) AND (change_y NE 0) THEN
    change_x = 1; change_y = 0
  END
  IF (c EQ 'd') AND (change_x NE 0) AND (change_y NE -1) THEN
    change_x = 0; change_y = 1
  END
  IF c EQ 'q' THEN EXIT(1)
  RETURN

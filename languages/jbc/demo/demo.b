height = 24
width = 79

DIM S(24), M(24)
FOR I = 0 TO height-1
S(I) = SPACE(width)
M(I) = SPACE(width)
NEXT I

GOSUB build_gfx

time = 0

loop:

GOSUB refresh

FOR iy = 0 TO height-1
  y = 2.0 * iy / height - 1.0
  FOR ix = 0 TO width-1
    x = 2.0 * (ix - width/2) / width

    z = 0.08 * (SIN((COS(time)+x)*360) + COS((SIN(time)+y)*360))
    zdiv = 2 / (1 + z)

    u = x * zdiv
    v = y * zdiv

    iu = width/2 + INT(0.6 * u * width/2)
    iv = height/2 + INT(0.6 * v * height/2)

    ch = " "
    IF (iu >= 0) AND (iu < width) AND (iv >= 0) AND (iv < height) THEN
      ch = M(iv)[iu + 1, 1]
    END
    S(iy)[ix + 1, 1] = ch
  NEXT
NEXT

IF SYSTEM(14) > 0 THEN STOP

time = time + 0.5

GOTO loop

STOP

refresh:
CRT @(-2)
FOR I = 0 TO 23
CRT S(I):
CRT
NEXT I
RETURN

build_gfx:

M( 0) = "                                                                               "
M( 1) = "                                                                               " 
M( 2) = "                          XXXXXXXXXXXXXX              XXXXXXXXXXXXXXXXXXXXXX   "
M( 3) = "                          XXXXXXXXXXXXXX              XXXXXXXXXXXXXXXXXXXXXX   "
M( 4) = "                          XXX        XXX              XXX                      "
M( 5) = "                          XXX        XXX              XXX                      "
M( 6) = "                          XXX        XXX              XXX                      "
M( 7) = "                          XXX        XXX              XXX                      "
M( 8) = "                XXX       XXX        XXX              XXX                      "
M( 9) = "                XXX       XXX        XXX              XXX                      "
M(10) = "                          XXX        XXX              XXX                      "
M(11) = "                          XXXXXXXXXXXXXXXXXXXXX       XXX                      "
M(12) = "                XXX       XXXXXXXXXXXXXXXXXXXXX       XXX                      "
M(13) = "                XXX       XXX               XXX       XXX                      "
M(14) = "                XXX       XXX               XXX       XXX                      "
M(15) = "                XXX       XXX               XXX       XXXXXX                   "
M(16) = "                XXX       XXX               XXX       XXXXXX                   "
M(17) = "             XXXXXX       XXXXXX            XXX       XXXXXX                   "
M(18) = "             XXXXXX       XXXXXX            XXX       XXXXXX                   "
M(19) = "             XXXXXX       XXXXXX            XXX       XXXXXX                   "
M(20) = "             XXXXXX       XXXXXX            XXX       XXXXXX                   "
M(21) = "    XXXXXXXXXXXXXXX       XXXXXXXXXXXXXXXXXXXXX       XXXXXXXXXXXXXXXXXXXXXX   "
M(22) = "    XXXXXXXXXXXXXXX       XXXXXXXXXXXXXXXXXXXXX       XXXXXXXXXXXXXXXXXXXXXX   "
M(23) = "                                                                               "

RETURN
                                                                                       
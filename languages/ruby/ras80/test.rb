require './radio86'

class Test < Radio86
  org 256

  mvi a, 11
  mvi b, 11
  mvi c, 11
  mvi d, 11
  mvi e, 11
  mvi h, 11
  mvi l, 11
  mvi m, 11

  jmp 1234

  mov a, a
  mov a, m
  mov a, b
  mov a, c
  mov a, d
  mov a, e
  mov a, h
  mov a, l

  mov m, a
  mov m, m
  mov m, b
  mov m, c
  mov m, d
  mov m, e
  mov m, h
  mov m, l

  mov b, a
  mov b, m
  mov b, b
  mov b, c
  mov b, d
  mov b, e
  mov b, h
  mov b, l

  mov c, a
  mov c, m
  mov c, b
  mov c, c
  mov c, d
  mov c, e
  mov c, h
  mov c, l

  mov d, a
  mov d, m
  mov d, b
  mov d, c
  mov d, d
  mov d, e
  mov d, h
  mov d, l

  mov e, a
  mov e, m
  mov e, b
  mov e, c
  mov e, d
  mov e, e
  mov e, h
  mov e, l

  mov h, a
  mov h, m
  mov h, b
  mov h, c
  mov h, d
  mov h, e
  mov h, h
  mov h, l

  mov l, a
  mov l, m
  mov l, b
  mov l, c
  mov l, d
  mov l, e
  mov l, h
  mov l, l

  lxi h, 123
  lxi b, 4567
  lxi d, 8912
  lxi sp, 1296

  sta 1234
  lda 4321

  lhld 1234
  shld 4321

  ldax d
  stax d
  ldax b
  stax b

  nop
  ret
  xchg
  sphl
  xthl
  cma
  rlc
  rrc
  ral
  rar
  ei
  di
  stc
  cmc
  pchl
  daa

  dad b
  dad d
  dad h
  dad sp

  call 1234

  push b
  push d
  push h
  push psw

  pop b
  pop d
  pop h
  pop psw

  add a
  add b
  add c
  add d
  add e
  add h
  add l
  add m

  adc a
  adc b
  adc c
  adc d
  adc e
  adc h
  adc l
  adc m

  sub a
  sub b
  sub c
  sub d
  sub e
  sub h
  sub l
  sub m

  sbb a
  sbb b
  sbb c
  sbb d
  sbb e
  sbb h
  sbb l
  sbb m

  ana a
  ana b
  ana c
  ana d
  ana e
  ana h
  ana l
  ana m

  xra a
  xra b
  xra c
  xra d
  xra e
  xra h
  xra l
  xra m

  ora a
  ora b
  ora c
  ora d
  ora e
  ora h
  ora l
  ora m

  cmp a
  cmp b
  cmp c
  cmp d
  cmp e
  cmp h
  cmp l
  cmp m

  adi 12
  aci 12
  sui 12
  sbi 12
  ani 12
  xri 12
  ori 12
  cpi 12

  self.in 14
  out 15

  inr a
  inr b
  inr c
  inr d
  inr e
  inr h
  inr l
  inr m

  dcr a
  dcr b
  dcr c
  dcr d
  dcr e
  dcr h
  dcr l
  dcr m

  inx b
  inx d
  inx h
  inx sp

  dcx b
  dcx d
  dcx h
  dcx sp

  rst 0
  rst 1
  rst 2
  rst 4
  rst 5
  rst 6
  rst 7

  jnc 1
  jc  2
  jnz 3
  jz  4
  jp  5
  jm  7
  jpo 8
  jpe 9

  cnc 1
  cc  2
  cnz 3
  cz  4
  cp  5
  cm  7
  cpo 8
  cpe 9

  rnc 1
  rc  2
  rnz 3
  rz  4
  rp  5
  rm  7
  rpo 8
  rpe 9

  call here+3

  store psw, h, d
  restore

  call label0

label0 label
  
  binary "test.bin"
  
end

puts "", "Done"

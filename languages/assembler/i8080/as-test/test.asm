  cpu 8080
  org 0

monitor equ 0f86ch

msg macro addr
  push h
  lxi h, addr
  call 0f816h
  pop h
  endm

  msg hello_msg
  jmp monitor

hello_msg db "HELLO!", 0

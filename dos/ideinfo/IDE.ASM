		.286
CSEG		SEGMENT
		ORG	100H
		ASSUME	CS:CSEG,DS:CSEG,ES:CSEG,SS:CSEG
START:
;-WRITE-LOGO------------
                MOV     DX,OFFSET LOGO_STR
                CALL    WRI_STR
;-ANALYZE-COM-STRING----
                MOV     SI,81H
AN_CO_1:
                LODSB
                CMP     AL,CR
                JZ      GET_IDE
                CMP     AL,'?'
                JZ      GET_HELP
                CMP     AL,'1'
                JB      AN_CO_1
                CMP     AL,'2'
                JG      AN_CO_1
                SUB     AL,31H
                SHL     AL,4
                MOV     BYTE PTR DS:IDE_NUMBA,AL
                JMP     GET_IDE
;-DISPLAY-HELP----------
GET_HELP:
                MOV     DX,OFFSET HELP_STR
                JMP     WRI_STR
;-GET-IDE-INFORMATION---
GET_IDE:
                CLI
                MOV     DX,1F6H
                MOV     AL,BYTE PTR DS:IDE_NUMBA
                OR      AL,0A0H
                OUT     DX,AL
                INC     DX
                MOV     AL,0ECH
                OUT     DX,AL
GE_ID_1:
                IN      AL,DX
                TEST    AL,80H
                JNZ     GE_ID_1
                SUB     DL,7
                MOV     DI,OFFSET IDE_TABLE
                MOV     CX,100H
GE_ID_2:
                IN      AX,DX
                STOSW
                LOOP    GE_ID_2
;-WRITE-IDE-INFORMATION-
                MOV     SI,OFFSET IDE_TABLE
        ;-CHECK-ERROR---
                MOV     AX,WORD PTR DS:[SI]
                OR      AX,AX
                JNZ     GE_CF_1
                MOV     DX,OFFSET ERR_STR
                CALL    WRI_STR
                MOV     AL,BYTE PTR DS:IDE_NUMBA
                SHR     AL,4
                ADD     AL,31H
                INT     29H
                MOV     DX,OFFSET ERR1_STR
                CALL    WRI_STR
                INT     20H
        ;-GENERAL-CFG---
GE_CF_1:
                MOV     DX,OFFSET IN1_STR
                CALL    WRI_STR
                MOV     DX,OFFSET IN5_STR
                CALL    WRI_STR_
                MOV     DX,SI
                ADD     DX,14H
                MOV     CX,0AH
                CALL    TRN_STR
                MOV     DX,OFFSET IN6_STR
                CALL    WRI_STR_
                MOV     DX,SI
                ADD     DX,2EH
                MOV     CX,4
                CALL    TRN_STR
                MOV     DX,OFFSET IN7_STR
                CALL    WRI_STR_
                MOV     DX,SI
                ADD     DX,36H
                MOV     CX,14H
                CALL    TRN_STR
                MOV     DX,OFFSET IN2_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI]
                PUSH    AX
                AND     AX,0000000001000000B
                CALL    WRI_YN
                MOV     DX,OFFSET IN3_STR
                CALL    WRI_STR_
                POP     AX
                PUSH    AX
                AND     AX,0000000010000000B
                CALL    WRI_YN
                MOV     DX,OFFSET IN4_STR
                CALL    WRI_STR_
                POP     AX
                AND     AX,1000000000000000B
                CALL    WRI_YN
                CALL    CONT_
        ;-MEDIA-INFO----
                MOV     DX,OFFSET ME1_STR
                CALL    WRI_STR
                MOV     DX,OFFSET ME2_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+2]
                PUSH    AX
                CALL    WO2DEC
                MOV     DX,OFFSET ME3_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+6]
                PUSH    AX
                CALL    WO2DEC
                MOV     DX,OFFSET ME4_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+0CH]
                PUSH    AX
                CALL    WO2DEC
                MOV     DX,OFFSET ME5_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+8]
                CALL    WO2DEC
                MOV     DX,OFFSET ME6_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+0AH]
                CALL    WO2DEC
                MOV     DX,OFFSET ME7_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+6CH]
                CALL    WO2DEC
                MOV     DX,OFFSET ME8_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+6EH]
                CALL    WO2DEC
                MOV     DX,OFFSET ME9_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+70H]
                CALL    WO2DEC
                MOV     DX,OFFSET ME10_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+74H]
                MOV     DX,WORD PTR DS:[SI+72H]
                CALL    DW2DEC
                MOV     DX,OFFSET ME11_STR
                CALL    WRI_STR_
                MOV     DX,WORD PTR DS:[SI+7AH]
                MOV     AX,WORD PTR DS:[SI+78H]
                CALL    DW2DEC
                CALL    CONT_
        ;-FEATURES------
                MOV     DX,OFFSET FC1_STR
                CALL    WRI_STR
                MOV     DX,OFFSET FC2_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+28H]
                CALL    WO2DEC
                MOV     DX,OFFSET FC3_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+2AH]
                CALL    WO2DEC
                MOV     DX,OFFSET FC4_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+2CH]
                CALL    WO2DEC
                MOV     DX,OFFSET FC5_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+5EH]
                PUSH    AX
                CBW
                CALL    WRI_SUNS
                POP     AX
                CBW
                JZ      FEAT_1
                PUSH    AX
                MOV     DX,OFFSET FC6_STR
                CALL    WRI_STR_
                POP     AX
                CALL    BY2DEC
FEAT_1:
                MOV     DX,OFFSET FC14_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+76H]
                PUSH    AX
                AND     AX,0000000100000000B
                CALL    WRI_YN
                MOV     DX,OFFSET FC15_STR
                CALL    WRI_STR_
                POP     AX
                CALL    BY2DEC
                MOV     DX,OFFSET FC7_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+60H]
                CALL    WRI_YN
                MOV     DX,OFFSET FC8_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+62H]
                PUSH    AX
                AND     AX,0000000100000000B
                CALL    WRI_SUNS
                MOV     DX,OFFSET FC9_STR
                CALL    WRI_STR_
                POP     AX
                PUSH    AX
                AND     AX,0000001000000000B
                CALL    WRI_SUNS
                MOV     DX,OFFSET FC10_STR
                CALL    WRI_STR_
                POP     AX
                PUSH    AX
                AND     AX,0000010000000000B
                CALL    WRI_YN
                MOV     DX,OFFSET FC11_STR
                CALL    WRI_STR_
                POP     AX
                PUSH    AX
                AND     AX,0000100000000000B
                CALL    WRI_SUNS
                MOV     DX,OFFSET FC12_STR
                CALL    WRI_STR_
                POP     AX
                PUSH    AX
                AND     AX,0001000000000000B
                CALL    WRI_SUNS
                MOV     DX,OFFSET FC13_STR
                CALL    WRI_STR_
                POP     AX
                AND     AX,0010000000000000B
                CALL    WRI_SUNS
                CALL    CONT_
        ;-EXTENDED-INFO-
                MOV     DX,OFFSET EX1_STR
                CALL    WRI_STR
                MOV     DX,OFFSET EX2_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+6AH]
                PUSH    AX
                AND     AX,0000000000000001B
                CALL    WRI_YN
                MOV     DX,OFFSET EX3_STR
                CALL    WRI_STR_
                POP     AX
                AND     AX,0000000000000010B
                CALL    WRI_YN
                MOV     DX,OFFSET EX4_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+66H]
                XCHG    AL,AH
                CALL    BY2DEC
                MOV     DX,OFFSET EX5_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+68H]
                XCHG    AL,AH
                CALL    BY2DEC
                MOV     DX,OFFSET EX6_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+7CH]
                PUSH    AX
                CALL    WRI_SUNS
                MOV     DX,OFFSET EX7_STR
                CALL    WRI_STR_
                POP     AX
                XCHG    AL,AH
                CALL    WRI_ENDI
                MOV     DX,OFFSET EX8_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+7EH]
                PUSH    AX
                CALL    WRI_SUNS
                MOV     DX,OFFSET EX9_STR
                CALL    WRI_STR_
                POP     AX
                XCHG    AL,AH
                CALL    WRI_ENDI
                MOV     DX,OFFSET EX10_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+80H]
                CALL    WRI_SUNS
                MOV     DX,OFFSET EX11_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+82H]
                CALL    WO2DEC
                MOV     DX,OFFSET EX12_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+84H]
                CALL    WO2DEC
                MOV     DX,OFFSET EX13_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+86H]
                CALL    WO2DEC
                MOV     DX,OFFSET EX14_STR
                CALL    WRI_STR_
                MOV     AX,WORD PTR DS:[SI+88H]
                CALL    WO2DEC
                MOV     DX,OFFSET EX15_STR
                CALL    WRI_STR_
                MOV     DX,WORD PTR DS:[SI+8AH]
                MOV     AX,WORD PTR DS:[SI+8CH]
                CALL    DW2DEC
                CALL    CONT_
;-EXIT------------------
                INT     20H
;-TRANSFER-STRING-------
TRN_STR:
                PUSH    SI
                MOV     SI,DX
                MOV     DI,OFFSET TRAN_BUF
                MOV     DX,DI
TR_ST_1:
                LODSW
                XCHG    AL,AH
                CMP     AL,21H
                JB      TR_ST_2
                STOSB
TR_ST_2:
                XCHG    AL,AH
                CMP     AL,21H
                JB      TR_ST_3
                STOSB
TR_ST_3:
                LOOP    TR_ST_1
                MOV     AX,0A0DH
                STOSW
                MOV     AL,'$'
                STOSB
                POP     SI
;-WRITE-STRING----------
WRI_STR:
                MOV     AH,9
                INT     21H
                RETN
;-YES-NO----------------
WRI_YN:
                MOV     DX,OFFSET YES_STR
                OR      AL,AL
                JNZ     WR_YN_1
                MOV     DX,OFFSET NO_STR
WR_YN_1:
                CALL    WRI_STR
                JMP     WRI_CRLF
;-ENABLED-DISABLED------
WRI_ENDI:
                MOV     DX,OFFSET ENA_STR
                OR      AL,AL
                JNZ     WR_ED_1
                MOV     DX,OFFSET DIS_STR
WR_ED_1:
                CALL    WRI_STR
                JMP     WRI_CRLF
;-SUPPORTED-NOT-SUPPORT-
WRI_SUNS:
                MOV     DX,OFFSET SUP_STR
                OR      AL,AL
                JNZ     WR_SN_1
                MOV     DX,OFFSET NSP_STR
WR_SN_1:
                CALL    WRI_STR
                JMP     WRI_CRLF
;-ALL-TO-DEC------------
BY2DEC:
                XOR     AH,AH
WO2DEC:
                XOR     DX,DX
DW2DEC:
                PUSH    BX CX
                CALL    DWORD2DEC
                POP     CX BX
WRI_CRLF:
                MOV     AL,CR
                INT     29H
                MOV     AL,LF
                INT     29H
                RETN
DWORD2DEC:
                MOV     BX,0AH
DW_DE_2:
                XCHG    CX,AX
                XCHG    DX,AX
                XOR     DX,DX
                DIV     BX
                XCHG    CX,AX
                DIV     BX
                XCHG    CX,DX
                PUSH    CX
                OR      AX,AX
                JNZ     DW_DE_3
                OR      DX,DX
                JZ      DW_DE_1
DW_DE_3:
                CALL    DW_DE_2
DW_DE_1:
                POP     AX
                ADD     AL,30H
                INT     29H
                RETN
;-CONTINUE?-------------
CONT_:
                MOV     DX,OFFSET CONT_STR
                CALL    WRI_STR
                XOR     AX,AX
                INT     16H
                CMP     AL,1BH
                JNZ     CONT_1
                INT     20H
CONT_1:
                RETN
;-EXTENDED-WRITE--------
WRI_STR_:
                CALL    WRI_STR
                MOV     AH,3
                XOR     BX,BX
                INT     10H
                XOR     DH,DH
                MOV     CX,58
                SUB     CX,DX
WR_ST_1:
                MOV     AL,'ú'
                INT     29H
                LOOP    WR_ST_1
                MOV     AL,' '
                INT     29H
                RETN
;-PROGRAM-DATA----------
CR              EQU     0DH
LF              EQU     0AH
EOS             EQU     '$'
IDE_NUMBA       DB      0
LOGO_STR        DB      'IDEiNFO v.1.00 by SkullC0DEr',CR,LF,EOS
HELP_STR        DB      'Usage: IDENFO [?][1][2]',CR,LF,EOS
ERR_STR         DB      'HDD No.$'
ERR1_STR        DB      ' is not IDE-ATA or not present!',CR,LF,EOS
YES_STR         DB      'Yes$'
NO_STR          DB      'No$'
ENA_STR         DB      'Enabled$'
DIS_STR         DB      'Disabled$'
NSP_STR         DB      'Not '
SUP_STR         DB      'Supported$'
IN1_STR         DB      CR,LF,'þ General configuration þ',CR,LF,EOS
IN2_STR         DB      'Not removable controller and|or device$'
IN3_STR         DB      'Removable media device$'
IN4_STR         DB      'Non-magnetic device$'
IN5_STR         DB      'Serial number$'
IN6_STR         DB      'Firmware revision$'
IN7_STR         DB      'Model number$'
ME1_STR         DB      CR,LF,'þ Media configuration þ',CR,LF,EOS
ME2_STR         DB      'Number of logical cylinders$'
ME3_STR         DB      'Number of logical heads$'
ME4_STR         DB      'Number of logical sectors per logical track$'
ME5_STR         DB      'Number of unformatted bytes per track$'
ME6_STR         DB      'Number of unformatted bytes per sector$'
ME7_STR         DB      'Number of current logical cylinders$'
ME8_STR         DB      'Number of current logical heads$'
ME9_STR         DB      'Number of current logical sectors per track$'
ME10_STR        DB      'Current capacity in sectors$'
ME11_STR        DB      'Total number of user addressable sectors$'
FC1_STR         DB      CR,LF,'þ Features and capabilities þ',CR,LF,EOS
FC2_STR         DB      'Buffer type$'
FC3_STR         DB      'Buffer size in 512 byte increments$'
FC4_STR         DB      '# of ECC bytes avail on read/write long commands$'
FC5_STR         DB      'Read|Write multiple commands$'
FC6_STR         DB      'Read|Write multiple capacity in sectors$'
FC7_STR         DB      'Can perform doubleword I/O$'
FC8_STR         DB      'DMA transfer$'
FC9_STR         DB      'LBA transfer$'
FC10_STR        DB      'IORDY can be disabled$'
FC11_STR        DB      'IORDY$'
FC12_STR        DB      'Advanced PIO mode$'
FC13_STR        DB      'Standby timer values specify$'
FC14_STR        DB      'Multiple sector setting is valid$'
FC15_STR        DB      'Current Read|Write multiple capacity in sectors$'
EX1_STR         DB      CR,LF,'þ Extended information þ',CR,LF,EOS
EX2_STR         DB      'Extended information fields are valid$'
EX3_STR         DB      'DMA transfer information fields are valid$'
EX4_STR         DB      'PIO data transfer cycle timing mode$'
EX5_STR         DB      'DMA data transfer cycle timing mode$'
EX6_STR         DB      'Single word DMA transfer modes$'
EX7_STR         DB      'Single word DMA transfer mode$'
EX8_STR         DB      'Multiword DMA transfer modes$'
EX9_STR         DB      'Multiword DMA transfer mode$'
EX10_STR        DB      'Advanced PIO Transfer Modes$'
EX11_STR        DB      'Minimum Multiword DMA Transfer Cycle in nsec$'
EX12_STR        DB      'Recommended Multiword DMA Transfer Cycle in nsec$'
EX13_STR        DB      'Min PIO Transfer Cycle w/o Flow Control in nsec$'
EX14_STR        DB      'Min PIO Transfer Cycle with IORDY Flow Control in nsec$'
EX15_STR        DB      'Advanced PIO mode support value$'
CONT_STR        DB      CR,LF,'Press any key to continue or <ESC> to exit',CR,LF,EOS
TRAN_BUF        DB      256 DUP (?)
IDE_TABLE       DB      512 DUP (?)
CSEG		ENDS
		END	START

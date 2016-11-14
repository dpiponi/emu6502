        .org    $0

        LDA     $8000
        SEC
        SBC     #'0'
        STA     NUM1
        STA     NUM2
        JSR     MUL
        JSR     wrdec
        lda     #'.'
        STA     $8000
        lda     #10
        STA     $8000
HALT:   JMP     HALT

DIV:    LDA     #0
        LDX     #8
        ASL     NUM
L1:     ROL
        CMP     DEN
        BCC     L2
        SBC     DEN
L2:     ROL     NUM
        DEX
        BNE     L1
        RTS

MUL:
        LDA #$00
        BEQ enterLoop

doAdd:
        CLC
        ADC NUM1

loop:
        ASL NUM1
enterLoop:
        ; For an accumulating multiply (.A = .A + num1*num2)
        ; set up num1 and num2, then enter here
        LSR NUM2
        BCS doAdd
        BNE loop
        RTS

wrdec:  cmp     #0
        bne     wrdec1
        lda     #'0'
        sta     $8000
        rts
wrdec1: 
        cmp     #0
        beq     endwr
        sta     NUM
        lda     #10
        sta     DEN
        jsr     DIV
        pha
        LDA     NUM
        JSR     wrdec1
        pla
        clc
        adc     #'0'
        sta     $8000
endwr:  rts

NUM:    .BYTE   0
DEN:    .BYTE   0
NUM1:   .BYTE   0
NUM2:   .BYTE   0

        .END



        .org    $0

init:
        JSR     rddec
        STA     NUM1
        STA     NUM2
        JSR     MUL
        JSR     wrdec
        lda     #'.'
        STA     $8000
        lda     #10
        STA     $8000
HALT:   JMP     init

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

rddec:
        lda     #0
        sta     ACC
rddec1:
        lda     $8000
        cmp     #10
        beq     rddec2
        pha
        lda     ACC
        sta     NUM1
        lda     #10
        sta     NUM2
        jsr     MUL
        sta     ACC
        pla
        sec
        sbc     #'0'
        clc
        adc     ACC
        sta     ACC
        jmp     rddec1
rddec2:
        lda     ACC
        rts


NUM:    .BYTE   0
DEN:    .BYTE   0
NUM1:   .BYTE   0
NUM2:   .BYTE   0
ACC:    .BYTE 0

        .END



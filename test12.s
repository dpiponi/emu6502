        .org    $0
        ldx     #0
        ldy     #4
        clc
loop:
        lda     num1,x
        adc     num2,x
        sta     num3,x
        inx
        dey
        bne     loop

        brk
num1:   .dword  $12345678
num2:   .dword  $fedcba98
num3:   .dword  0

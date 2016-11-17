        .org 0

        lda     #1      ; 1
        clc
        adc     #2      ; 3
        sec
        sbc     #8      ; 0xfb
        clc
        ror     a       ; 0x7d
        sec
        rol     a       ; 0xfb
        sta     <tmp1
        lda     #0      ; 0
        lda     <tmp1   ; 0xfb
        rol     a
        rol     a
        rol     a
        rol     a
        rol     a
        rol     a
        rol     a
        rol     a
        rol     a       ; 0xfb
        lsr     a       ; 0x7d
        adc     #0      ; 0x7e
        sta     tmp3
        and     #0
        ldx     #2
        lda     tmp1, x ; 0x7e
        sta     tmp4
        ldy     #3
        lda     #0
        lda     tmp1, y ; 0x7e
        sta     tmp3
        ldx     #2
        ldy     tmp1, x
        lda     #0
        tya             ; 0x7e
        sta     tmp2
        ldy     #1
        ldx     tmp1, y
        lda     #0
        txa             ; 0x7e
        tay
        ldx     #$ff
        txs
        tya
        pha
        lda     #0
        tsx
        inx
        lda     $100, x ; 0x7e
        clv
        bvc     cont1
        lda     #0
        brk
cont1:
        pla             ; 0x7e
        eor     #$53    ; 0x2d
        and     #$ce    ; 0x0c
        ora     #$01    ; 0x0d
        sta     tmp1
        lda     #>cont2
        pha
        lda     #<cont2
        pha
        sec
        php
        rti
        lda     #0
        brk
cont2:
        lda     tmp1
        brk

tmp1:   .byte 0
tmp2:   .byte 0
tmp3:   .byte 0
tmp4:   .byte 0

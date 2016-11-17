        .org    0
; Look up string in dictionary
        lda     #<table
        sta     <ptr
        lda     #>table
        sta     <ptr+1

oloop:
        ldx     #0  ; pointer into query string
        ldy     #0
        lda     (<ptr), y
        beq     fail
iloop:
        lda     (<ptr), y
        cmp     query, x
        bne     nomatch
        cmp     #0
        beq     match
        inx
        clc
        lda     <ptr
        adc     #1
        sta     <ptr
        lda     <ptr+1
        adc     #0
        sta     <ptr+1
        jmp     iloop

nomatch:
        ; skip to next word

        clc
        lda     <ptr
        adc     #1
        sta     <ptr
        lda     <ptr+1
        adc     #0
        sta     <ptr+1
        lda     (<ptr), y
        bne     nomatch
        clc
        lda     <ptr
        adc     #2
        sta     <ptr
        lda     <ptr+1
        adc     #0
        sta     <ptr+1
        jmp     oloop

match:
        ldy     #1
        lda     (<ptr), y
        clc
        brk

fail:
        sec
        brk

ptr:    .word   $0000

query:  .asciiz "charlie"

table:  .asciiz "alice"
        .byte   11
        .asciiz "bob"
        .byte   22
        .asciiz "charlie"
        .byte   33
        .asciiz "dan"
        .byte   44
        .byte   0

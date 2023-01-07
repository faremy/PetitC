.text
.globl main

f_0_putchar:
    pushq %rbp
    movq %rsp, %rbp
    andq $(-16), %rsp
    call putchar
    movq %rbp, %rsp
    popq %rbp
    ret

f_1_malloc:
    pushq %rbp
    movq %rsp, %rbp
    andq $(-16), %rsp
    call malloc
    movq %rbp, %rsp
    popq %rbp
    ret

main:
    movq $97, %rdi
    call f_0_putchar
    pushq $40
    movq $98, %rdi
    call f_0_putchar
    movq $99, %rdi
    call f_0_putchar
    movq $10, %rdi
    call f_0_putchar
    popq %rbx
    ret


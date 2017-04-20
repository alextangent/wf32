\ $Id: extend,v 1.27 2007/09/10 19:59:49 alex_mcdonald Exp $
\ extend the kernel

cr .( -- KERNEL EXTEND )

    include optinline   \ inline optimiser
    include primutil
    include recognizers \ recognizers
    include case        \ eaker case
    include estring     \ extended UTF-8 string support
    include numdispl    \ extended general number displays
    include interpif    \ interpretive conditionals
    include asm/asmx64  \ the assembler

    include optliterals32   \ literals optimiser
    include callback32    \ windows callback support
    include utils       \ odds&sods utility words
    include module      \ scoping support for modules
    include time        \ load time utility words
    include dotwords    \ dot support words
    include locals      \ locals support
    include disx64      \ load the disassembler
    include err         \ windows exception handling
    include imageman    \ fsave, application & turnkey words
    include uconsole      
    include forget      \ forget words
    include sortsearch  \ sorts & binary search
    include words
    include shell       \ load SHELL utility words
    include 2words      \ 2value etc
    include environ     \ environment

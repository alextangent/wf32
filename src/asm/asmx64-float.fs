\ --------------------------- Change Block -------------------------------
\
\
\ ------------------------- End Change Block -----------------------------
\
\ Experimental: a fully optimising, STC based, ANS Forth compliant kernel
\
\
\    Copyright [c] 2005, 2017 by Alex McDonald (alex at rivadpm dot com)
\
\    Redistribution and use in source and binary forms, with or without
\    modification, are permitted provided that the following conditions are
\    met:
\
\    1. Redistributions of source code must retain the above copyright
\    notice, this list of conditions and the following disclaimer.
\
\    2. Redistributions in binary form must reproduce the above copyright
\    notice, this list of conditions and the following disclaimer in the
\    documentation and/or other materials provided with the distribution.
\
\    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
\    IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
\    TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
\    PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
\    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
\    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
\    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
\    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
\    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (
\    INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
\    THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. ))
\
\
\
\ ------------------------------------------------------------------------
\
\    This is a complete rewrite of the original Win32Forth assembler.
\    All of the original code has been replaced, and in the belief that
\    there is no code from the original version, this code is being issued
\    under the BSD 2-Clause license.
\
\ ------------------------------------------------------------------------
\
\ Float support
\
\ ------------------------------------------------------------------------


DECIMAL

only forth
  also assembler
  also asm-hidden
  also forth

in-hidden
( define standard words to support floats )
      
freg             bit? freg?
freg memory + oddsize + constant freg/mem/odd

 80bit freg greg: /rf ( regs like st0 )

s" float register must be st0"         asmmsg: _?mstst0

: ?fl-st0 ( -- ) hold-reg @ _?mstst0 ; ( 1st reg must be zero )
    
: ?odds-chk ( n m -- )
    size-odd @ ?dup if
      -rot between 0= _?invosz
    else _?reqosz then ;

: ?odds-16>32   ( -- ) [ 16bit 32bit ] 2literal ?odds-chk ;
: ?odds-16>64   ( -- ) [ 16bit 64bit ] 2literal ?odds-chk ;
: ?odds-32>64   ( -- ) [ 32bit 64bit ] 2literal ?odds-chk ;
: ?odds-32>80   ( -- ) [ 32bit 80bit ] 2literal ?odds-chk ;

in-asm

( declare float registers )
  0   80bit freg  register: st0 ' st0 alias st
  1   80bit freg  register: st1
  2   80bit freg  register: st2
  3   80bit freg  register: st3
  4   80bit freg  register: st4
  5   80bit freg  register: st5
  6   80bit freg  register: st6
  7   80bit freg  register: st7

in-hidden

( float operations with no operands )

      $9b     ' simple     op: fwait
    $d9d0     ' simple     op: fnop
  $9bdbe2     ' simple     op: fclex
    $dbe2     ' simple     op: fnclex
    $dbe3     ' simple     op: fninit
  $9bdbe3     ' simple     op: finit
    $d9e8     ' simple     op: fld1
    $d9e9     ' simple     op: fldl2t
    $d9ea     ' simple     op: fldl2e
    $d9eb     ' simple     op: fldpi
    $d9ec     ' simple     op: fldlg2
    $d9ed     ' simple     op: fldln2
    $d9ee     ' simple     op: fldz
    $d9f1     ' simple     op: fyl2x
    $d9f3     ' simple     op: fpatan
    $d9f5     ' simple     op: fprem1
    $d9f6     ' simple     op: fdecstp
    $d9f7     ' simple     op: fincstp
    $d9f8     ' simple     op: fprem
    $d9f9     ' simple     op: fyl2xp1
    $d9fd     ' simple     op: fscale
    $dae9     ' simple     op: fucompp
    $ded9     ' simple     op: fcompp

( implied st0 )

    $d9e0     ' simple     op: fchs
    $d9e1     ' simple     op: fabs
    $d9f0     ' simple     op: f2xm1
    $d9e4     ' simple     op: ftst
    $d9e5     ' simple     op: fxam
    $d9f2     ' simple     op: fptan
    $d9f4     ' simple     op: fxtract
    $d9fa     ' simple     op: fsqrt
    $d9fb     ' simple     op: fsincos
    $d9fc     ' simple     op: frndint
    $d9fe     ' simple     op: fsin
    $d9ff     ' simple     op: fcos

\ ----------- Arithmetic opcodes -------------------------
\
\  33sp0 xrm
\    p -- POP bit
\    s -- st(i) bit
\    r -- /0  FiADDp        FADD/FADDP/FIADD—Add
\         /1  FiMULp        FMUL/FMULP/FIMUL—Multiply
\         /2  FiCOM         FCOM/FICOM—Compare
\         /3  FiCOMPp       FCOMP/FICOMP/FCOMPP—Compare
\         /4  FiSUBp        FSUB/FSUBP/FISUB—Subtract
\         /5  FiSUBRp       FSUBR/FSUBRP/FISUBR—Reverse Subtract
\         /6  FiDIVp        FDIV/FDIVP/FIDIV—Divide
\         /7  FiDIVRp       FDIVR/FDIVRP/FIDIVR—Reverse Divide
\
\    When mod-r/m is 3rm, then m is st(i).
\    Note well the nature of FxxxR and Fxxx opcodes:
\     r=/4 or /6, s=0 ST(0) <- ST(0) - ST(i)  FSUB  ST(0), ST(i)
\     r=/4 or /6, s=1 ST(i) <- ST(0) - ST(i)  FSUBR ST(i), ST(0)
\     r=/5 or /7, s=0 ST(0) <- ST(i) - ST(0)  FSUBR ST(0), ST(i)
\     r=/5 or /7, s=1 ST(i) <- ST(i) - ST(0)  FSUB  ST(i), ST(0)
\
\   i (integer) forms only accept a memory operand, s is size.
\   All others accept 2 regs or 1 memory operand

: fl-size64 ( op -- op' ) ( mark sti if double or extended )
    size-odd @ 64bit = if $4 or then ;
: fl-size16 ( op -- op' ) ( mark if 16 bit operand )
    size-odd @ 16bit = if $4 or then ;
: fl-mem ( op - op' ) ( memory op )
    unpack2 swap hold-reg ! ;

:noname ( op - op' ) ( word/dword mem )
    fl-mem
    ?odds-16>32 fl-size16 ;
  1 memory oddsize + 0 0 0
  opgrp: grp-fint

    $da00     ' grp-fint    op: fiadd    ( word/dword mem )
    $da01     ' grp-fint    op: fimul
    $da02     ' grp-fint    op: ficom
    $da03     ' grp-fint    op: ficomp
    $da04     ' grp-fint    op: fisub
    $da05     ' grp-fint    op: fisubr
    $da06     ' grp-fint    op: fidiv
    $da07     ' grp-fint    op: fidivr
    $db02     ' grp-fint    op: fist

:noname ( op - op' ) ( stn st0 )
    hold-base @ _?mstst0 ( reg2 must be zero )
    op+reg ;
  2 freg freg 0 0
  opgrp: grp-falp

    $dec0     ' grp-falp    op: faddp    ( stn st0 )
    $dec8     ' grp-falp    op: fmulp
    $dee8     ' grp-falp    op: fsubp
    $def8     ' grp-falp    op: fdivp
    $dee0     ' grp-falp    op: fsubrp ' fsubrp alias fsubpr
    $def0     ' grp-falp    op: fdivrp ' fdivrp alias fdivpr

:noname ( op - op' ) ( st0 stn )
    ?fl-st0 hold-base @ or ( add in float reg to opcode )
    -1 hold-base ! operands- ;
  2 freg freg 0 0
  opgrp: grp-flcm

    $d9c8     ' grp-flcm    op: fxch     ( st0 stn )
    $dac0     ' grp-flcm    op: fcmovb
    $dac8     ' grp-flcm    op: fcmove
    $dad0     ' grp-flcm    op: fcmovbe
    $dad8     ' grp-flcm    op: fcmovu
    $dbc0     ' grp-flcm    op: fcmovnb
    $dbc8     ' grp-flcm    op: fcmovne
    $dbd0     ' grp-flcm    op: fcmovnbe
    $dbd8     ' grp-flcm    op: fcmovnu
    $dbf0     ' grp-flcm    op: fcomi
    $dff0     ' grp-flcm    op: fcomip
    $dbe8     ' grp-flcm    op: fucomi
    $dfe8     ' grp-flcm    op: fucomip
    $dde0     ' grp-flcm    op: fucom
    $dde8     ' grp-flcm    op: fucomp

:noname ( op - op' ) ( single|double mem | st0 stn | stn st0 )
    unpack2 ( n $dx )
    memory? if
      opnd-count 1 > _?2mnyop
      ?odds-32>64 fl-size64
    else ( reg, reg form )
      opnd-count 2 < _?2fewop
      hold-reg @ if ( stn st0 case )
        base<->reg ( swap into base )
        $04 or ( make stn st0 )
      then
      ?fl-st0 ( reg1 or reg2 must be zero )
    then
    swap hold-reg ! ;
  1 freg/mem/odd freg 0 0
  opgrp: grp-falu
    
    $d800     ' grp-falu    op: fadd    ( single|double mem | st0 stn | stn st0 )
    $d801     ' grp-falu    op: fmul
    $d802     ' grp-falu    op: fcom
    $d803     ' grp-falu    op: fcomp
    $d804     ' grp-falu    op: fsub
    $d805     ' grp-falu    op: fsubr
    $d806     ' grp-falu    op: fdiv
    $d807     ' grp-falu    op: fdivr

' fl-mem
  1 memory 0 0 0
  opgrp: grp-fctl

    $d904     ' grp-fctl    op: fldenv   ( anysize mem )
    $d905     ' grp-fctl    op: fldcw
  $9bd906     ' grp-fctl    op: fstenv
    $d906     ' grp-fctl    op: fnstenv
  $9bd907     ' grp-fctl    op: fstcw
    $d907     ' grp-fctl    op: fnstcw
    $dd04     ' grp-fctl    op: frstor
  $9bdd06     ' grp-fctl    op: fsave
    $dd06     ' grp-fctl    op: fnsave
    $df04     ' grp-fctl    op: fbld
    $df06     ' grp-fctl    op: fbstp
  $0fae00     ' grp-fctl    op: fxsave
  $0fae01     ' grp-fctl    op: fxrstor

: fxsave64  fxsave  rex!w ;
: fxrstor64 fxrstor rex!w ;

' op+reg ( op - op' ) ( stn )
  1 freg 0 0 0 
  opgrp: grp-stn

    $ddc0     ' grp-stn     op: ffree       ( stn )
    $dfc0     ' grp-stn     op: ffreep

create fl-tab1 ( table for oddball opcodes )
\             reg     16fp     32fp     64fp     80fp    ( see fsize )
( fld )     $d9c0 w,     0 w, $d900 w, $dd00 w, $db05 w, \ 0
( fstp )    $ddd8 w,     0 w, $d903 w, $dd03 w, $db07 w, \ 10
( fst  )    $ddd0 w,     0 w, $d902 w, $dd02 w,     0 w, \ 20
( fild )        0 w, $df00 w, $db00 w, $df05 w,     0 w, \ 30
( fistp )       0 w, $df03 w, $db03 w, $df07 w,     0 w, \ 40
( fisttp )      0 w, $df01 w, $db01 w, $dd01 w,     0 w, \ 50
align

: fsize ( -- n ) ( 1=16bit 2=32bit 3=64bit 4=80bit )
    size-odd @ dup msbit swap 80bit = - ;

: flookup ( op -- op' ) ( memory opnd )
    fsize 2* + w@ ( index into table )
    fl-mem ;

:noname ( op - op' ) ( reg or 32 thru 80 memory )
    freg? if w@ op+reg else ?odds-32>80 flookup then ;
  1 freg/mem/odd 0 0 0
  opgrp: grp-f1

:noname ( op - op' ) ( reg or 32 thru 64 memory )
    freg? if w@ op+reg else ?odds-32>64 flookup then ;
  1 freg/mem/odd 0 0 0
  opgrp: grp-f2

:noname ( op - op' ) ( 16 thru 64 memory )
    ?odds-16>64 flookup ;
  1 memory oddsize + 0 0 0
  opgrp: grp-f3

    fl-tab1  #0   +  ' grp-f1      op: fld
    fl-tab1  #10  +  ' grp-f1      op: fstp
    fl-tab1  #20  +  ' grp-f2      op: fst
    fl-tab1  #30  +  ' grp-f3      op: fild
    fl-tab1  #40  +  ' grp-f3      op: fistp
    fl-tab1  #50  +  ' grp-f3      op: fisttp

:noname ( op -- op' )
    pfx-opnd off
    size-mem @ 16bit <> if _?invosz then
    unpack2 reg-count if
      hold-reg @ _?invreg ( must be ax reg )
      2+ nip 4
    else swap then install-/r ;
  1 rreg/mem 0 0 0
  opgrp: grp-fstsw

    $9bdd07 '    grp-fstsw op: fstsw
      $dd07 '    grp-fstsw op: fnstsw

in-forth

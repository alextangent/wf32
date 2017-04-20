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
\    This is a complete rewrite of the original Win32Forth disassembler.
\    All of the original code has been replaced, and in the belief that
\    there is no code from the original version, this code is being issued
\    under the BSD 2-Clause license.
\
\ ------------------------------------------------------------------------
\
\  The disassembly output buffer is formatted as
\  ( $off )  opcode  operands                          \ hex dump
\
\  Example of disassembly of DUP
\
\  see dup
\  code dup ( 1 -- 2 )
\  \ dup is defined in src/kernel/gkernel32.fs at line 124
\  \ ' nseopt compiled
\  \ code=$401135 len=6 type=129
\  ( $0 )    mov     dword { $-4 ebp } eax             \ 8945FC
\  ( $3 )    sub     ebp $4                            \ 83ED04
\  ( $6 )    ret                                       \ C3 ( end ) ok
\
\ DIS ( addr -- )    disassembles from a specific address one line at
\                    a time. To stop the disassembly, press Q
\ SEE name           Full disassembly of the name. See above for DUP
\ 'SEE ( xt -- )     For unnnamed XTs; similar to DIS but assumes that
\                    the code is an XT and is DESCRIBEable.
\ DESCRIBE ( xt -- ) Provides a narrative for the XT; see DUP above
\
\ ------------------------------------------------------------------------

\ 32 and 64 bit disassembler

cr .( Loading x64 disassembler...)

only forth
  also assembler
  also asm-hidden
  also forth definitions

\ -------------------------- code to name ---------------------------

\ Given a possible address somewhere in an xt, tries to find the nearest nfa
\ by searching all the vocabularies; returns the nfa in the header or zero.
\ The best is the lowest that's >= to the supplied address.

: _best-addr ( addr best-xt nfa -- addr best-xt'  true )
    name>interpret rot dup>r over       ( a b x -> b x a x r: a )
    >= if max else drop then
    r> swap true ;
    
: code>name     ( addr -- nfa | 0 )
    dup cdp 2@ between 0= if            \ origin + highest addr for section
      drop 0                            \ it's zero if not in a code section
    else
      0                                 \ ( addr best-xt )
      voc-link @                        \ iterate vocabs
        [: ['] _best-addr swap          \ find closest xt
           traverse-wordlist ;]    \ traverse wordlist
      list-apply nip >name
    then ;

in-hidden

base @ octal

: oper-col #10 #col ; \ set to operation field
: opnd-col #18 #col ; \ set to operand field
: cmnt-col #52 #col ; \ set to comment field

: .st ( addr len -- )
    type space ;
: .ss ( off n addr len -- ) ( type text from array )
    oper-col drop -rot dup>r * + r> -trailing .st ;
: .so ( off n addr len -- ) ( add opcode )
   oper-col .ss ;
: .sopfx ( off n addr len -- ) ( add opcode )
   oper-col
   pfx-inst @ ?dup if $f0 - 5 s" lock ???  repnzrepz " .ss opnd-col then
   .ss ;

0 value dis-base
: .soff ( -- ) ( print offset )
    s" (" .st dup dis-base - $. s" )" .st ;

: .scale ( n -- ) ( scale for index ) 1- 2 s" *2*4*8" .ss ;
: .cond ( op addr len -- ) ( condition code )
    type $f and 2 s" o nob aee nebea s nsp npl geleg " .so ;

: .reg8  ( n -- ) ( 8 bit register )
    pfx-rex @ $40 and 
    over $4 $8 within and if
      4 - 3 s" splbplsildil"
    else
      4 s" al  cl  dl  bl  ah  ch  dh  bh  r8b r9b r10br11br12br13br14br15b"
    then .ss ;
: .reg16 ( n -- ) ( 16 bit register )
    4 s" ax  cx  dx  bx  sp  bp  di  si  r8w r9w r10wr11wr12wr13wr14wr15w" .ss ;
: .reg32 ( n -- ) ( 32 bit register )
    4 s" eax ecx edx ebx esp ebp esi edi r8d r9d r10dr11dr12dr13dr14dr15d" .ss ;
: .reg64 ( n -- ) ( 64 bit register )
    3 s" raxrcxrdxrbxrsprbprsirdir8 r9 r10r11r12r13r14r15" .ss ;

create reg-t
    ' .reg8  , ' .reg16 , ' .reg32 , ' .reg64 ,

: .reg ( n sz -- ) ( split based on reg size )
    msbit reg-t +cells perform ;

: .size ( sz -- ) ( size of memory object )
    msbit 6 s" byte  word  dword qword dqwordqqword" .ss ;

: show-name       ( xt -- )
    dup code>name ?dup if          \ see if we can find the nfa
      s" '" .st
      dup name>string .st                   \ name
      name>interpret - ?dup if $s. s" +" .st then  
    else $s.
    then ;

: dis-loc ( addr -- )                                \ display disasm location
    dup ." ( " $. show-name ." ) " ;

\ ----------- Generate REX mod-r/m and sib bytes  ---------------

: d-triple ( ss-iii-bbb -- bbb iii ss )
    $8 /mod $8 /mod ;
    
\ REX prefix encoding: 0100wrxb (binary)
\
\     rex byte
\       7                           0
\     +---+---+---+---+---+---+---+---+
\     | 0   1   0   0 | w | r | x | b |
\     +---+---+---+---+---+---+---+---+
\
\     w: Sets 64-bit operand size (default is still 32-bit)
\     r: Bit 3 of Reg in ModRM (regs 8 thru 15 )
\     x: Bit 3 of Index in SIB
\     b: Bit 3 of Base in ModRM or SIB
\     z: 0100 0000 (hex 40) is used to indicate SPL,
\        BPL, DIL and SIL in 64 bit mode.
\

: d-rex ( rex -- )
    dup pfx-rex !
    ?dup if ( add in 8 for mode64 regs )
      dup $1 and if $8 hold-base  ! then
      dup $2 and if $8 hold-index ! then
      dup $4 and if $8 hold-reg   ! then
          $8 and if 64bit size-mem ! then
    then ;

: fixup-sizes ( -- )
    pfx-addr @ if ( prefix is set, set right addr size )
      mode32? if 16bit else 32bit then
    else mode @
    then size-addr !
    pfx-opnd @ if ( opnd size is set )
      16bit
    else
      size-mem @ dup 0= if
        drop def-size-mem @
      then
    then size-mem ! ;

: d-immed ( addr -- addr' )
    size-immed @ 0= if ( set immediate size if not set )
      size-mem @ 32bit min size-immed !
    then
    has/64? if dup asm-d@ var-extra ! 4 + then
    dup size-immed @
    dup>r case
      8bit  of opnd-8bit? if asm-b@ else asm-sb@ then endof
      16bit of asm-w@  endof
      32bit of asm-d@  endof
    endcase var-immed !
    r> + immed+ ;

: d-disp ( addr -- addr' )
    has/64? if dup asm-d@ var-extra ! 4 + then
    dup disp-8bit? if
      asm-sb@ else asm-d@
    then var-disp !
    size-disp @ + ;

\ Mod-R/M Encoding
\
\     mod-r/m byte
\       7                           0
\     +---+---+---+---+---+---+---+---+
\     |  mod  |    reg    |     rm    |
\     +---+---+---+---+---+---+---+---+
\
\     xxrrrmmm (binary), best viewed as 3 octal digits:
\         x: modifier flag (indicates one of four cases)
\         r: register (normally the source operand)
\         m: reg/mem (normally the destination operand)
\     All shown in OCTAL
\     Encoding        Reg/Mem     Addressing Mode Notes
\     xr4 sib         SIB         Can't use ESP as indexed
\     0r5 disp32      disp32      [RIP] in 64bit
\     1rm disp8       disp8 [base]
\     2rm disp32      disp32 [base]
\     3rm             reg
\     0rm             [base]      Can't use [ESP] or [EBP] as base w/o disp
\
\ SIB (Scale*Index+Base) Encoding
\
\     sib byte
\       7                           0
\     +---+---+---+---+---+---+---+---+
\     | scale |   index   |    base   |
\     +---+---+---+---+---+---+---+---+
\
\     ssiiibbb (binary), best viewed as 3 octal digits:
\         s: Scale (multiplier for the Index register)
\         i: Index register
\         b: Base register
\     All shown in OCTAL
\     Encoding         Reg/Mem Addressing Mode    Notes
\     0r4 045 disp32   disp32                     absolute in mode64
\     0r4 si5 disp32   disp32 [index*n]
\     0r4 sib          [base] [index*n]
\     1r4 sib disp8    disp8 [base] [index*n]
\     2r4 sib disp32   disp32 [base] [index*n]
\
\ For 64bit registers, rex.r, rex.b and rex.x are set
\


: d-mod-r/m ( addr -- addr' ) ( decode mod-r/m )
    fixup-sizes
    count d-triple ( mem r mod )
    swap hold-reg +! ( mem mod )
    dup case ( mem mod )
      0 of over xbp? if disp-32bit! mode64? if hasrip+ then else based+ then endof
      1 of based+ disp-8bit!  endof
      2 of based+ disp-32bit! endof
    endcase over hold-base +!
    dup>r 3 <> swap xsp? and if ( if xr4 then has a sib )
      count ( sib )
      d-triple  ( sib byte as b i s )
      ?dup if hold-scale ! scaled+ then 
      dup xsp? if drop else hold-index +! indexed+ then
      dup xbp? r@ 0= and if ( if it's 0r4 si5 then disp32 )
        drop based- disp-32bit! 
      else
        4 - hold-base +! based+ ( remove esp from the base reg )
      then
    then r>drop
    disp? if d-disp then ( process displacement ) ;

: p-reg  ( -- ) opnd-col hold-reg @ size-mem @ .reg ;

: p-size ( -- ) opnd-col fixup-sizes size-mem @ .size ;

: p-branch ( addr -- addr' )   ( branch )
    opnd-col d-immed
    imm-8bit? if s" short" .st then
    dup var-immed @ + show-name ;

: p-mem ( addr -- addr ) ( mem )
    s" {" .st
    pfx-seg @ ?dup if $64 - 3 s" fs:gs:" .ss then
    disp? if
      var-disp @ 
      has/64? if
        $. var-extra @ $.
      else
        mode64? memory? disp = hasrip? and and if over + s" rip" .st then ( disp only, RIP )
        show-name
      then
    then
    based?   if hold-base  @ size-addr @ .reg then
    indexed? if hold-index @ size-addr @ .reg then
    scaled?  if hold-scale @ .scale then
    s" }" .st ;

: p-szmem ( addr -- addr ) ( mem or base reg )
    opnd-col memory? if
      p-size p-mem
    else hold-base @ size-mem @ .reg then ;

: p-reg-only ( addr -- addr' ) fixup-sizes opnd-col hold-base @ size-mem @ .reg ;
: p-imm-only ( addr -- addr' ) opnd-col d-immed var-immed @ has/64? if $. var-extra @ $. else show-name then ;
: p-mem-only ( addr -- addr' ) d-mod-r/m p-szmem ;
: p-reg-mem  ( addr -- addr' ) d-mod-r/m p-reg p-szmem ;
: p-mem-reg  ( addr -- addr' ) p-mem-only p-reg ;
: p-mr/rm    ( addr -- addr' ) direct? if p-reg-mem else p-mem-reg then ;
: p-reg-imm  ( addr -- addr' ) p-reg-only p-imm-only ;
: p-mem-imm  ( addr -- addr' ) d-mod-r/m p-szmem p-imm-only ;

: p-moffset ( addr -- addr' ) ( moffset, no mod-r/m )
    fixup-sizes disp-32bit! d-disp ( has/64? will override 32bit if needed )
    hold-reg off ( set eax )
    direct? if p-szmem p-reg else p-reg p-szmem then ;

\ ----------- Opcode decode  ---------------

: dis+sign   ( op -- ) ( sign extend immediate, bit 1 )
    002 and if imm-8bit! then ;
: dis+width  ( op -- ) ( if not 8 bit, set full width, bit 0 )
    001 and 0= if 8bit size-mem ! then ;
: dis+direct ( op -- ) ( set direction, bit 1 )
    002 and if direct+ then ;
: dis+reg    ( xpm -- p ) ( extract p )
    070 and 3 rshift ;

defer dis-more

: |osz ( addr n -- addr' ) pfx-opnd ! dis-more ;
: |asz ( addr n -- addr' ) pfx-addr ! dis-more ;
: |pfx ( addr n -- addr' ) pfx-inst ! dis-more ;
: |srg ( addr n -- addr' ) pfx-seg !  dis-more ;

: 1op   ( -<name>- ) create parse-str ", does> count .st drop ;

1op |??? "???"

\ -------------------- ALU Opcodes --------------------

: .alu ( op -- )
    dis+reg 3 s" addor adcsbbandsubxorcmp" .so ;

: |alu ( addr op -- addr' )
    dup dis+direct
    dup dis+width
    .alu p-mr/rm ;

: |ali ( addr op -- addr' )
    dup dis+sign
        dis+width
    dup b@ .alu p-mem-imm ;

: |ala ( addr op -- addr' )
    hold-reg off
    dup dis+width
    .alu p-reg-imm ;

: |F6. ( addr op -- addr' )
    dis+width
    dup b@ dis+reg dup 4 s" test??? not neg mul imuldiv idiv" .so
    if p-mem-only else p-mem-imm then ;

: |im3 ( addr op -- addr' ) ( 3addr imul )
    dis+sign s" imul" .st p-reg-mem p-imm-only ;

\ -------------------- push/pop inc/dec --------------------

: |s64 ( -- ) ( make 64bit if not 16bit )
    mode64? size-mem @ 16bit <> and if 64bit size-mem ! then ;

: |psr ( addr n -- addr' ) ( common inc/dec/push/pop reg form )
    |s64
    d-triple drop
    4 s" inc dec pushpop " .so
    hold-base +!
    p-reg-only ;

: |ppa ( addr n -- addr' )
    mode32? if
      1 and 6 s" *pusha*popa " .so p-size
    else |??? then ;

: |ppf ( addr n -- addr' )
    mode32? if
      1 and 6 s" *pushf*popf " .so p-size
    else |??? then ;

: |pom ( addr n -- addr' ) drop s" pop" .st p-mem-only ;
: |psi ( addr n -- addr' ) dis+sign s" push" .st p-size p-imm-only ;
: |inc ( addr n -- addr' ) mode32? if |psr else d-rex dis-more then ;
    
\ ----------------jcc/jmp/call opcodes --------------------

: .jmp ( addr n -- addr' ) 1 and 4 s" calljmp " .so p-branch ;
: |jmp ( addr n -- addr' ) imm-32bit! .jmp ;
: |jms ( addr n -- addr' ) imm-8bit! .jmp ;
: |jcc ( addr n -- addr' ) s" j" .cond imm-8bit! p-branch ;

: |ff. ( addr op -- addr' )
    dis+width
    dup b@ dis+reg dup 4 s" inc dec callcalljmp jmp push??? " .so
    1 > if |s64 then ( adjust size if not inc/dec )
    p-mem-only ;

: |loo ( addr op -- addr' )
    3 and 6 s" loopnzloopz loop  jcxz  " .so
    1 hold-base +! p-reg-only imm-8bit! p-branch ;

\ -------------------- mov opcodes --------------------

: |lea ( addr n -- addr' ) drop s" lea" .st p-reg-mem ;
: |mmi ( addr n -- addr' ) dis+width s" mov" .st p-mem-imm ;

: |m64 ( -- )
    opnd-64bit? if has/64+ then ;

: |mov ( addr n -- addr' )
    dup dis+direct
    dup dis+width s" mov" .st
    240 < if p-mr/rm else |m64 p-moffset then ;

: |mri ( addr n -- addr' )
    d-triple drop
    $6 = if 8bit size-mem ! then
    hold-base +!
    s" mov" .st |m64 p-reg-imm ;

\ -----------------test/xchg opcodes --------------------

: |tsi ( addr op -- addr' )
    dis+width
    s" test" .st p-reg-imm ;

: |xar ( addr n -- addr' ) ( xchg eax reg format )
    s" xchg" .st
    p-reg-only  ( eax )
    007 and hold-base ! p-szmem ;

: |txc ( addr n -- addr' )
    dup dis+width
    $84 - 1 rshift 4 s" testxchg" .so p-reg-mem ;

1op |nop "nop"

\ -----------------ret/int opcodes --------------------

: |ret ( addr op -- addr' )
    dup $8 and if s" ret.far" else s" ret" then .st
    $1 and 0= if 16bit size-immed ! p-imm-only then ;

: |int ( addr n -- addr' )
    s" int" .st $cc = if 
      3 $. else imm-8bit! p-imm-only 
    then ;

\ -----------------string opcodes --------------------

: |str ( addr n -- addr' )
    dup dis+width
    $a4 - 1 rshift 4 s" movscmps??? stoslodsscas" .sopfx p-size ;

\ -----------------shift opcodes --------------------

: |shf  ( addr op -- addr' )
    imm-8bit!
    dup>r dis+width
    dup b@ dis+reg 3 s" rolrorrclrcrshlshrsalsar" .so
    p-mem-only
    r> 022 and
    case
        0 of p-imm-only endof
       20 of 1 $.       endof
       22 of s" cl" .st endof
    endcase ;

\ -----------------simple opcodes --------------------

: |06. ( addr op -- addr' ) 
    mode32? if
      dup 1 and 4 s" pushpop " .ss
      opnd-col 3 rshift 3 s" *es*cs*ss*ds" .ss
    else |??? then ;
: |26. ( addr op -- addr' ) 
    mode32? if
      $26 - dup $1 and swap 2 rshift or
      4 s" *es:*daa*cs:*das*ss:*aaa*ds:*aas" .ss
    else |??? then ;
: |9e. ( addr op -- addr' ) 
    mode32? if
      $9e - 5 s" *sahf*lahf" .ss
    else |??? then ;
: |d4. ( addr op -- addr' ) 
    mode32? if
      $d4 - 4 s" *aad*aam" .ss imm-8bit! p-imm-only
    else |??? then ;

1op |xla "*xlat"
1op |hlt "*hlt"
1op |cmc "cmc"

: |f8. ( addr op -- addr' ) $f8 - 3 s" clcstcclisticldstd" .ss ;

\ -----------------CBW/CWD etc opcodes --------------------

: |cwd ( addr op -- addr' )
    fixup-sizes
    1 and 3 * ( select half )
    size-mem @ msbit 1- ( select size )
    + 4 s" cbw cwdecdqecwd cdq cqo " .ss ;

\ ----------------- MOVSX/ZX/SXD opcodes ------------------

: |mvx ( addr op -- addr' )
    dup $63 = if ( movsxd )
      drop 32bit ( 32bit ) 1 ( movsx )
    else
      dup 1 and 1+ ( 2=16bit 1=8bit )
      swap 010 and 3 rshift ( 0=movzx 1=movsx )
    then
    5 s" movzxmovsx" .ss
    >r d-mod-r/m p-reg
    r> size-mem ! p-szmem ;

\ ---------------------------- Opcode tables  -----------------------------

: ops $10 0 do ' , loop ;

create op1-table

\      0    1    2    3     4    5    6    7     8    9    A    B     C    D    E    F

ops  |alu |alu |alu |alu  |ala |ala |06. |06.  |alu |alu |alu |alu  |ala |ala |06. |??? \ 0. 
ops  |alu |alu |alu |alu  |ala |ala |06. |06.  |alu |alu |alu |alu  |ala |ala |06. |06. \ 1. 
ops  |alu |alu |alu |alu  |ala |ala |26. |26.  |alu |alu |alu |alu  |ala |ala |26. |26. \ 2. 
ops  |alu |alu |alu |alu  |ala |ala |26. |26.  |alu |alu |alu |alu  |ala |ala |26. |26. \ 3. 
                                                                                             
ops  |inc |inc |inc |inc  |inc |inc |inc |inc  |inc |inc |inc |inc  |inc |inc |inc |inc \ 4. 
ops  |psr |psr |psr |psr  |psr |psr |psr |psr  |psr |psr |psr |psr  |psr |psr |psr |psr \ 5. 
ops  |ppa |ppa |??? |mvx  |srg |srg |osz |asz  |psi |im3 |psi |im3  |??? |??? |??? |??? \ 6. 
ops  |jcc |jcc |jcc |jcc  |jcc |jcc |jcc |jcc  |jcc |jcc |jcc |jcc  |jcc |jcc |jcc |jcc \ 7. 
                                                                                             
ops  |ali |ali |??? |ali  |txc |txc |txc |txc  |mov |mov |mov |mov  |??? |lea |??? |pom \ 8. 
ops  |nop |xar |xar |xar  |xar |xar |xar |xar  |cwd |cwd |??? |???  |ppf |ppf |9e. |9e. \ 9.
ops  |mov |mov |mov |mov  |str |str |str |str  |tsi |tsi |str |str  |str |str |str |str \ A. 
ops  |mri |mri |mri |mri  |mri |mri |mri |mri  |mri |mri |mri |mri  |mri |mri |mri |mri \ B. 
                                                                                             
ops  |shf |shf |ret |ret  |??? |??? |mmi |mmi  |??? |??? |ret |ret  |int |int |??? |??? \ C. 
ops  |shf |shf |shf |shf  |d4. |d4. |??? |xla  |??? |??? |??? |???  |??? |??? |??? |??? \ D. <-- floats
ops  |loo |loo |loo |loo  |??? |??? |??? |???  |jmp |jmp |??? |jms  |??? |??? |??? |??? \ E. 
ops  |pfx |??? |pfx |pfx  |hlt |cmc |f6. |f6.  |f8. |f8. |f8. |f8.  |f8. |f8. |ff. |ff. \ F.

\ ---------------------------- $0F Opcodes -------------------------------

1op |ud1 "ud1"
1op |ud2 "ud2"
1op |emm "emms"

: |cmv ( addr op -- addr' ) s" cmov"    .cond p-reg-mem ;
: |set ( addr op -- addr' ) s" set"     .cond 8bit size-mem ! p-mem-only ;
: |jcl ( addr op -- addr' ) s" j"       .cond imm-32bit! p-branch ;
: |im2 ( addr op -- addr' ) s" imul"    .st drop p-reg-mem ;
: |bsw ( addr op -- addr' ) s" bswap"   .st 007 and hold-base +! p-reg-only ;
: |xad ( addr op -- addr' ) s" xadd"    .st dis+width p-mem-reg ;
: |cxc ( addr op -- addr' ) s" cmpxchg" .st dis+width p-mem-reg ;
: |bsx ( addr op -- addr' ) 1 and 3 s" bsfbsr" .ss p-reg-mem ;
: |bt  ( op -- )            dis+reg 4 - 3 s" bt btsbtrbtc" .ss ;
: |btx ( addr op -- addr' ) |bt p-mr/rm ;
: |bti ( addr op -- addr' ) drop dup b@ |bt imm-8bit! p-mem-imm ;

: |shl ( addr op -- addr' )
    dup>r 010 and 3 rshift 4 s" shldshrd" .ss p-mem-reg
    r> 1 and if s" cl" .st else imm-8bit! p-imm-only then ;

: |poc ( addr op -- addr' )
    pfx-inst @ $f3 <> if |??? else drop s" popcnt" .st p-reg-mem then ;

: |cxn ( addr op -- addr' )  \ *** not quite correct
    mode32? negate #10 s" cmpxchg16bcmpxchg8b " .ss drop d-mod-r/m p-mem  ;

: |?0f s" ?0f" .st drop p-reg-mem ;

create $0f-table

\      0    1    2    3     4    5    6    7     8    9    A    B     C    D    E    F

ops  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |ud2  |?0f |?0f |?0f |?0f  \ 0.
ops  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  \ 1.
ops  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  \ 2.
ops  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  \ 3.

ops  |cmv |cmv |cmv |cmv  |cmv |cmv |cmv |cmv  |cmv |cmv |cmv |cmv  |cmv |cmv |cmv |cmv  \ 4.
ops  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  \ 5.
ops  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  \ 6.
ops  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |emm  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  \ 7.

ops  |jcl |jcl |jcl |jcl  |jcl |jcl |jcl |jcl  |jcl |jcl |jcl |jcl  |jcl |jcl |jcl |jcl  \ 8.
ops  |set |set |set |set  |set |set |set |set  |set |set |set |set  |set |set |set |set  \ 9.
ops  |?0f |?0f |?0f |btx  |shl |shl |?0f |?0f  |?0f |?0f |?0f |btx  |shl |shl |?0f |im2  \ A.
ops  |cxc |cxc |?0f |btx  |?0f |?0f |mvx |mvx  |poc |ud1 |bti |btx  |bsx |bsx |mvx |mvx  \ B.

ops  |xad |xad |?0f |?0f  |?0f |?0f |?0f |cxn  |bsw |bsw |bsw |bsw  |bsw |bsw |bsw |bsw  \ C.
ops  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  \ D.
ops  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  \ E.
ops  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  |?0f |?0f |?0f |?0f  \ F.

: dis-optab ( addr table -- addr' op )
    >r count ( fetch code & increment address )
    dup cells r> + perform ( fetch op and execute ) ;

: dis-op$0f ( addr op -- addr' ) drop $0f-table dis-optab ;
: dis-op1   ( addr -- addr' )         op1-table dis-optab ;

' dis-op$0f op1-table $0f cells+ !
' dis-op1 is dis-more

\ -----------------------------------------------------------------------
\ User interface
\ -----------------------------------------------------------------------

: dis-op  ( adr -- adr' )
    /reset
      hold-reg off
      hold-base off
      hold-index off
    .soff oper-col
    dup dis-op1
    cmnt-col s" \" .st
    tuck swap ?do i b@ h.2 loop ;

: desc-stack    ( n -- )
                dup 0< if drop ." ? " else #. then ;

variable desc-list 0 desc-list !         \ list of desc-hows

: desc-how      ( xt type -- )           \ how to see an xt
    here >r 0 , , , r>                   \ link, type, xt
    desc-list add-link ;                 \ add in the link

:noname ( xt nfa -- ) ." code " .id drop                               ; tpri desc-how \ for primitives
:noname ( xt nfa -- ) swap execute . ." value " .id                    ; tval desc-how \ for value
:noname ( xt nfa -- ) ." variable " .id ."  ( is " execute @ #. ." ) " ; tvar desc-how \ for variable
:noname ( xt nfa -- ) swap >body . ." constant " .id                   ; tcon desc-how \ for constant
:noname ( xt nfa -- ) swap execute up0 - . ." user " .id               ; tusr desc-how \ for user
:noname ( xt nfa -- ) ." defer " .id ."  ( is " defer@ .name ."  )"    ; tdef desc-how \ for defer
:noname ( xt nfa -- ) name>string ." : " type drop                     ; tcol desc-how \ for :
:noname ( xt nfa -- ) ." :noname " 2drop                               ; tnon desc-how \ for :noname
:noname ( xt nfa -- ) ." vocabulary " .id drop                         ; tvoc desc-how \ for vocabulary
:noname ( xt nfa -- ) ." create " .id ."  ( addr " >body $. ." ) "     ; tcre desc-how \ for create
:noname ( xt nfa -- ) 0 rot execute . ." offset " .id                  ; toff desc-how \ for offset
:noname ( xt nfa -- ) dup (in/out@) drop . ." import: " .id drop       ; timp desc-how \ for import:
:noname ( xt nfa -- ) ." library " .id drop                            ; tlib desc-how \ for library
:noname ( xt nfa -- ) ." include " .id drop                            ; tfil desc-how \ for files

: desc-type     ( xt -- )                        \ find entry and execute
    dup >name
    dup head.tfa b@
    desc-list                            \ fetch entry from linked
    begin @ dup                          \ list and cehck if match on type
    while
      2dup cell+ @ = if                  \ type entry, check type
        nip 2 cells+ @ execute exit      \ matches, so execute
      then
    repeat
    2drop ." ? " .id drop ;              \ default

also forth definitions

: dis  ( adr -- )
    cr dup to dis-base
    dup dis-loc
    begin
      dup
      cr dis-op
      key upc dup
    while
      case
        'Q' of 2drop exit endof
        rot drop
      endcase
    repeat 3drop ;

: describe ( xt -- )
    cr
    dup desc-type                       \ do the type of the name
    >name

    \ stack effects
    dup (in/out@) swap                      \ nfa, get in/out
      ."  ( " desc-stack ." -- " desc-stack ." ) "

    \ tell user where the word was loaded
    dup cr ." \ " dup name>string type ."  is defined" .viewinfo
    \ compile information
    cr ." \ "
    dup head.comp @ dup                      \ fetch the comp xt
    case
      ['] xt-call,   of ." call compiled" endof
      ['] inline, of ." inlined" endof
      dup show-name ." compiled"
    endcase drop

    dup name>compile                          \ get the xts of the compile words
    case
      ['] compile, of                  endof
      ['] execute  of  ." , IMMEDIATE" endof
      dup ." , " show-name ." is non-std compilation part"
    endcase drop

    \ misc head info
    cr ." \ code=" dup name>interpret $.
    dup ." len=" head.ofa w@ #.
        ." type=" head.tfa b@ #.
    ;

: 'see ( xt -- )
    dup dup
    dup cell- @ -cell = if          \ it's a :noname as comp is at offset -4
      cr ." :noname \ " $.          \ print the hex address
      oper-col ." ( Q to exit )"
      dis drop exit
    then
    describe
    dup >name head.ofa w@ over +         \ length to disassemble
    swap
    dup to dis-base
    begin
      2dup - 0>                       \ anything left?
    while
      cr dis-op
    repeat
    b@ $c2 $c3 between if cr dis-op then
    ."  ( end )" drop
    dup >name head.tfa b@ tdef <> if    \ if it's not a defer
      drop
    else
      defer@ recurse                 \ do again for deferred word
    then ;

: see ( <name> -- ) ' 'see ;

only forth also definitions
base !

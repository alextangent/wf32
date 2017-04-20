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
\ This code is completely untested on its 64bit code generation
\ capabilities, but the majority of the code required to support it is
\ in place. Here are some known issues;
\ 
\ 1 Generating 64bit code while running the assembler on a 32bit system,
\ or 32bit code running on a 64bit system will probably not work.
\ Although there are attempts at cell independence, there are definitely
\ system size dependencies.
\ 
\ 2 Only rudimentary documentation is provided. You are expected to
\ understand how to code x86 and x64 assembler.
\
\ All the code is in vocabulary ASSEMBLER; support words are in ASM-HIDDEN.
\ There are some name collisions with the FORTH vocabulary. They are
\    CMOVE LOOP
\

( Code marked *** in comments may need further attention )

base @ hex

in-asm

( address and data sizes )
( for byte length, operand type and number )
   $1 constant   8bit
   $2 constant  16bit
   $3 constant  24bit
   $4 constant  32bit
   $8 constant  64bit
   $a constant  80bit
  $10 constant 128bit
  $20 constant 256bit

( operand types )

in-hidden

( operand type bits )
( all operands are <= $0fff and > $20 is reserved for register types )
( operand mask )
    $0fff constant operands    ( used to mask out real operands )
( based, indexed and disp are values 1 thru 7; don't change! )
      $01 constant disp        ( a displacement operand )
      $02 constant based       ( base register )
      $04 constant indexed     ( index register )
      $07 constant memory      ( a memory operand )
      $08 constant immed       ( an immediate operand )
      $10 constant regcl       ( allows CL as immediate operand )
( registers; must all be > $20 which is 256bit flag )
      $40 constant rreg        ( register )
      $80 constant freg        ( float reg )
     $100 constant mreg        ( mmx register )
     $200 constant xreg        ( xmm register )
     $400 constant yreg        ( ymm register )
( operand size modifier bits, must be <= $ffff )
    $4000 constant modsize     ( byte word dword etc )
    $8000 constant oddsize     ( odd size memory )
( operand modifiers set during parse, must be > $ffff )
   $10000 constant segreg      ( segment register )
   $20000 constant scaled      ( scale )
   $40000 constant direct      ( direction bit )
   $80000 constant in{}        ( in effective address )
  $100000 constant has/64      ( has 64 bit disp/imm )
  $200000 constant hasrip      ( has rip address )
  $400000 constant reloc       ( relocate displacements )
( commonly used compounds )
  rreg memory +          constant rreg/mem   ( register or memory )
  rreg/mem immed +       constant rreg/mem/i ( any reg, mem or immed )

variable mode           ( mode )
variable def-size-mem  ( default data size )

in-forth

( support for 3 modes )
: mode32/32 ( -- ) ( set mode=32, data size 32 )
    32bit mode ! 
    32bit def-size-mem ! ;
: mode64/32 ( -- ) ( set mode=64, data size 32 )
    64bit mode !
    32bit def-size-mem ! ;
: mode64/64 ( -- ) ( set mode=64, data size 64 )
    64bit mode !
    64bit def-size-mem ! ;

mode32/32 ( only mode tested so far! )

in-hidden

: mode32? ( -- f ) mode @ 32bit = ;
: mode64? ( -- f ) mode @ 64bit = ;
: ?mode32 ( -- ) mode32? _?inv32b  ;
: ?mode64 ( -- ) mode64? _?inv64b ;

( count values )
0 value  opnd-count     ( count of operands seen )
0 value  opnd-min       ( min of operands )
0 value  reg-count      ( count of registers )

( states during parse )
variable curr-state    ( current state )
variable allow-state   ( ptr to allowed states list in opgroup: )

: curr-state? ( bits -- n ) ( get bit in current state )
    curr-state @ and ;
: curr-state+ ( bits -- )      ( set bits in current state )
    curr-state @ or curr-state ! ;

: flag-set?  ( f -- f ) ( check if set )
    allow-state @ data-w@ and ;
: flag-notset?  ( f -- f ) ( check if not set )
    flag-set? 0= ;

: bit? ( f -- ) ( test bit mask )
    create data-d, does> data-d@ curr-state? ;
: bit+ ( f -- ) ( set bit mask )
    create data-d, does> data-d@ curr-state+ ;
: bit- ( f -- ) ( reset bit mask )
    create invert data-d, does> data-d@ curr-state? curr-state ! ;

( set curr-state flags )
  rreg       bit+ rreg+
  based      bit+ based+
  indexed    bit+ indexed+
  scaled     bit+ scaled+
  disp       bit+ disp+
  immed      bit+ immed+
  direct     bit+ direct+
  modsize    bit+ modsize+
  regcl      bit+ regcl+
  segreg     bit+ segreg+
  in{}       bit+ in{}+
  has/64     bit+ has/64+
  hasrip     bit+ hasrip+
  reloc      bit+ reloc+

( reset curr-state flags )
  disp       bit- disp-
  based      bit- based-
  immed      bit- immed-
  operands   bit- operands-
  direct     bit- direct-
  in{}       bit- in{}-

( basic 1 bit curr-state query flags )
  based      bit? based?
  indexed    bit? indexed?
  scaled     bit? scaled?
  disp       bit? disp?
  immed      bit? immed?
  regcl      bit? regcl?

( during parse query flags )
  in{}       bit? in{}?
  direct     bit? direct?
  modsize    bit? modsize?
  segreg     bit? segreg?
  has/64     bit? has/64?
  hasrip     bit? hasrip?
  reloc      bit? reloc?

( multiple bit query flags )
  rreg/mem   bit? rreg/mem?
  memory     bit? memory?

( check allowed, aborts if not )
: ?allow-mem memory  flag-notset? _?notmem ;

( during parse check & set flags )
( check if allowed, check if set, set flag )
: ?in{}+    ?allow-mem  in{}?           _?dupopr in{}+ ;
: ?disp+    ?allow-mem  disp?           _?dupopr disp+ ;
: ?based+   ?allow-mem  based?          _?dupopr based+ ;
: ?indexed+ ?allow-mem  indexed?        _?dupopr indexed+ ;
: ?scaled+  ?allow-mem  scaled?         _?dupopr scaled+ ;
: ?immed+   immed flag-notset?            _?notimm
                       immed?           _?dupopr immed+ ;
: ?modsize+ memory modsize + flag-notset? _?notmod
                       modsize?         _?dupopr
                       in{}?            _?notmod modsize+ ;
: ?segreg+  in{}? 0=                    _?notmem
                       segreg?          _?dupopr segreg+ ;

: immed! ( -- ) ( make immed only ) operands- immed+ ;

variable sp-save    ( stack pointer save for whole instruction )
: save-depth ( -- )     depth sp-save ! ;
: depth-change ( -- n ) depth sp-save @ - ;

variable {}sp-save  ( stack pointer save in { } memory operand )
: {}save-depth ( -- )     depth {}sp-save ! ;
: {}depth-change ( -- n ) depth {}sp-save @ - ;

( the instruction build area, in order of generation required ) 
( many of these variables are shared by the disassembler )

variable pfx-addr     ( 1   address size prefix )
variable pfx-opnd     ( 1   operand size prefix )
variable pfx-seg      ( 1   segment override prefix )
variable pfx-inst     ( 1   instruction prefix lock or repcc )
variable pfx-rex.w    ( 1   rex.w override prefix )
variable pfx-rex      ( 1   rex override prefix, merged with pfx-rex.w )
variable operation    ( 1-3 the operation )
variable mod-r/m      ( 1   mod-r/m byte )
variable sib          ( 1   sib byte )
variable var-disp     ( 1/4 displacement value )
variable var-immed    ( 1/2/4 immediate value )
variable var-extra    ( for 64bit immediate & disp )

( hold vars for instruction area registers )
variable hold-reg     ( hold for dest reg value )
variable hold-regt    ( hold for dest reg type )
variable hold-regsz   ( hold for dest reg size )
variable hold-base    ( hold for src or base reg )
variable hold-baset   ( hold for dest reg type )
variable hold-basesz  ( hold for dest reg size )
variable hold-index   ( hold for indexed reg )
variable hold-scale   ( hold for scaled reg )
variable hold-3rd     ( vex: hold for 3rd reg value )
variable hold-3rdt    ( vex: hold for 3rd reg type )
variable hold-3rdsz   ( vex: hold for 3rd reg size )

( various sizes )
variable size-addr    ( addr size )
variable size-disp    ( displacement size )
variable size-immed   ( immediate size )
variable size-mem     ( dest data size operand )
variable size-odd     ( for odd memory sizes )

: disp-8bit? ( -- f ) size-disp @  8bit = ;
: disp-32bit! ( -- )  disp+ 32bit size-disp ! ;
: disp-8bit! ( -- )   disp+  8bit size-disp ! ;
: imm-8bit? ( -- f )  size-immed  @  8bit = ;
: imm-8bit! ( -- )    8bit  size-immed ! ;
: imm-32bit! ( -- )   32bit size-immed ! ;
: 1byte? ( n -- f )   $-80 $80 within ;

\ ----------- Code generator  -------------------

variable gen-op ( code generator )

0 value asm-start     ( start of code address )
0 value asm-disp      ( start of displacement )
0 value asm-len       ( length of code )
0 value asm-base      ( base of code address )

: ?asm-c, ( n -- ) ?dup if asm-c, then ;

create nbyte-table ( build data based on size )
  ' asm-c, data-d,
  ' asm-w, data-d,
  0        data-d,
  ' asm-d, data-d,

: asm-nbyte, ( n sz -- ) ( not used for >32bit values )
    [ nbyte-table cell - ] literal +cells perform ;

: 2pack ( a b -- n )     $8 lshift or ;
: 3pack ( a b c -- n )   2pack 2pack ;
: unpack2 ( n -- a b )   dup $ff and swap $8 rshift ;
: unpack3 ( n -- a b c ) unpack2 unpack2 ;

: >rel ( disp -- disp' size ) ( relocate displacement to a relative offset )
    var-disp @ asm-here               ( relative to end of opcode )
    immed? if size-immed @ + then     ( possible immediate... )
    size-disp @ dup>r + -
    r@ 8bit = if dup 1byte? 0= _?invrsz then r> ;  ( adjust )

defer fwd-mark ' noop is fwd-mark

: build ( -- ) ( build the instruction )
    pfx-addr @ ?asm-c,
    pfx-opnd @ ?asm-c,
    pfx-seg  @ ?asm-c,
    operation @ unpack3 dup $ff00 and if ( has a prefix? )
      unpack2 asm-c,
    else
      pfx-inst @ ?asm-c,
    then
    pfx-rex @ pfx-rex.w @ or ?asm-c,
    ?asm-c, ?asm-c, asm-c,  ( rest of opcode )
    rreg/mem?   if mod-r/m   @               asm-c, then
    indexed?    if sib       @               asm-c, then
    asm-here to asm-disp     ( first byte of possible displacement )
    reloc?      if >rel                      asm-nbyte,
    else disp?  if var-disp  @ size-disp  @  asm-nbyte,
    then then
    immed?      if var-immed @ size-immed @  asm-nbyte, then
    has/64?     if var-extra @               asm-d,     then
    asm-here asm-start - to asm-len
    fwd-mark ;

in-asm

: /reset ( -- )  \ start from first operand
\ note that areas are either 0 or -1
\ this only works because variables are contiguous
  [ pfx-addr var-extra  cell+ over - ] 2literal erase
  [ hold-reg hold-3rdsz cell+ over - ] 2literal -1 fill
  [ size-addr size-odd  cell+ over - ] 2literal erase

   0 to opnd-count
   0 to opnd-min
   0 to reg-count

   curr-state off
   curr-state allow-state !   

   gen-op off

   asm-here to asm-start
   save-depth ;

/reset

\ ----------- Generate REX mod-r/m and sib bytes  ---------------

\ REX prefix encoding: 0100wrxb (binary)
\
\     REX byte
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
\         m: rreg/mem (normally the destination operand)
\     All shown in OCTAL
\     Encoding        rreg/mem       Addressing Mode Notes
\     3rm             reg
\     0rm             [base]         Can't use [ESP] or
\                                      [EBP] as base w/o disp
\     0r5 disp32      disp32         [RIP] in 64bit
\     xr4 sib         SIB            Can't use ESP as indexed
\     1rm disp8       disp8 [base]
\     2rm disp32      disp32 [base]
\
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
\     Encoding         rreg/mem Addressing Mode    Notes
\     0r4 sib          [base] [index*n]
\     0r4 si5 disp32   disp32 [index*n]
\     0r4 045 disp32   disp32                      absolute in mode64
\     1r4 sib disp8    disp8 [base] [index*n]
\     2r4 sib disp32   disp32 [base] [index*n]
\     xr4 04b          [base]                      Only used for [ESP]
\     xr4 044          [ESP]
\     xr4 s4b          ---                         Undefined if s > 0
\
\ For 64bit registers, rex.r, rex.b and rex.x are set
\
\   All base encodings;
\     ..b     if not [esp] or [index]
\     ..4 044 [esp] only
\     ..4 ..b
\
\   All index encodings;
\     ..4 si. not [esp]
\
\   All disp encodings;
\     0.5     disp32 alone
\     0.4 ..5 disp32 if [index] only
\     1..     disp8  if [base]
\     2..     disp32 if [base]
\
\   Table:                                  Notes
\   [                    ]  3rm             reg,reg type
\   [ based              ]  0rm             but not ESP & EBP needs d
\   [ based         disp ]  nrm disp        xrm d but not ESP
\   [ based indexed      ]  0r4 sib         0r4 sib
\   [ based indexed disp ]  nr4 sib disp    0r4 sib d
\   [       indexed      ]  0r4 si5 disp32  0r4 si5 d32
\   [       indexed disp ]  0r4 si5 disp32  0r4 si5 d32
\   [               disp ]  0r5 disp32      0r5 d32 or 0r4 045 d32
\
\ --------------------------------------------------------------

octal

in-hidden

: eax? ( -- f ) hold-reg @ 0= ; ( test if al/ax/eax/rax reg )
: esp? ( reg -- f )       4 = ; ( 4 is esp )
: xsp? ( reg -- f ) 7 and 4 = ; ( 4 is esp/rsp, 12 is r12 )
: xbp? ( reg -- f ) 7 and 5 = ; ( 5 is ebp/rbp, 13 is r13 )

: rex! ( n -- ) ( set rex prefix )
    dup if ?mode32 then
    pfx-rex @ or pfx-rex ! ;
: rex!w  ( -- ) ( set width bit )
    ?mode32 110 pfx-rex.w ! ;
: rex!z  ( -- ) ( set rex byte for sil dil bpl spl )
    100 rex! ;
: rex!n> ( reg rex -- reg' ) ( set rex.n for 64bit regs, return 3bit reg )
    over 7 > if rex! else drop then 7 and ;
: rex!b> ( -- reg ) ( set rex.b, return 3bit reg )
    hold-base @ 101 rex!n> ;
: rex!x> ( -- reg ) ( set rex.x, return ssiii000 reg )
    hold-index @ 102 rex!n> 3 lshift ( process index )
    scaled? if hold-scale @ 6 lshift or then ; ( add in shifted scale )
: rex!r> ( -- reg ) ( set rex.r, return 3bit reg )
    hold-reg @ 104 rex!n> ;

( process mod-r/m and sib )
: (0rx) ( b -- )   rex!r> 3 lshift or mod-r/m ! ;
: (0rm)            rex!b> (0rx) ;
: (0r4)            004 (0rx) ;
: (0r5)            005 (0rx) ;
: (six) ( b -- )   rex!x> or sib ! ;
: (sib)            rex!b> (six) ;
: (si5)            005 (six) ;
: (d)              disp+ disp-8bit? if 100 else 200 then mod-r/m +! ;
: (dnz)            var-disp @ if (d) else disp- then ; ( don't generate 0 disp )
: (dsz)            hold-base @ xbp? if (d) else (dnz) then ; ( disp if reg is xbp )

: (3rm)            rex!b> 300 or (0rx) ;
: (nrm-disp)       hold-base @ dup xsp? if
                     hold-index ! indexed+ (0r4) (sib)
                   else drop (0rm) then (dsz) ;
: (nr4-sib-disp)   hold-index @ dup esp? if ( parse doesn't allow scale with esp )
                     hold-base dup @ dup esp? _?espinx
                     hold-index ! ! ( swap regs )
                   else drop then
                   (0r4) (sib) (dsz) ;
: (0r4-si5-disp32) (0r4) hold-scale dup @ 1 = if ( if scale *2, use base )
                     off hold-index @ hold-base ! based+ (sib) (dsz)
                   else drop (si5) disp-32bit! then ;
: (0r5-disp32)     disp-32bit! mode64? if
                     hasrip? if
                       (0r5) reloc+ ( relative disp )
                     else
                       (0r4) 045 sib ! indexed+ ( abs-addr )
                     then
                   else (0r5) then ;

create gen-table $8 cells allot ( 3 bit index )
  ' (3rm)            gen-table                                !
  ' (nrm-disp)       gen-table based                  cells + !
  ' (nrm-disp)       gen-table based         disp +   cells + !
  ' (nr4-sib-disp)   gen-table based indexed      +   cells + !
  ' (nr4-sib-disp)   gen-table based indexed disp + + cells + !
  ' (0r4-si5-disp32) gen-table       indexed          cells + !
  ' (0r4-si5-disp32) gen-table       indexed disp +   cells + !
  ' (0r5-disp32)     gen-table               disp     cells + !

: gen-r/m-sib ( -- ) ( generate mod-r/m and sib )
    rreg/mem? if ( has a mod-r/m )
      memory?   ( mask & get the final state, reg,reg is 0 )
      gen-table +cells perform
    then ;

\ Set the prefixes for data and address
\
\ ------------- Address size prefix...  ------------------
\
\   Mode    Default  Required  Prefix
\    64       64        64       --
\                       32       67
\    32       32        32       --
\                       16       67
\
\   16bit address mode is not encodable in 64bit mode.
\   Use of this prefix is rare, as the addressing mode
\   is fixed by the OS. It is used for the following
\   pointers and counters;
\
\       16                32              64
\   cmps  si, di, cx   esi, edi, ecx   rsi, rdi, rcx
\   ins   di, cx       edi, ecx        rdi, rcx
\   jcxz  cx           ecx             rcx
\   lods  si, cx       esi, ecx        rsi, rcx
\   loop  cx           ecx             rcx
\   movs  si, di, cx   esi, edi, ecx   rsi, rdi, rcx
\   outs  si, cx       esi, ecx        rsi, rcx
\   rep   cx           ecx             rcx
\   scas  di, cx       edi, ecx        rdi, rcx
\   stos  di, cx       edi, ecx        rdi, rcx
\   xlat  bx           ebx             rbx
\

: /size-addr ( addrsz -- )
    dup 32bit mode @ between 0= _?invasz ( invalid address size )
    size-addr @ ?dup if
      over <> _?misasz ( mismatched address size )
    then
    size-addr ! ;

\ For data, there are two possible operand sizes; the source and the destination.
\ for most instructions, these must match if they are specified. A 0 zero
\ indicates no size has been set. For instructions where they must not match,
\ for instance MOVZX or MOVSX, the data size sets the "odd" size, not the
\ source size.
\
\ Some opcodes violate the following rules.
\
\   Opcode              opnd1         opnd2       notes
\   general             8 16 32 64    same        <= mode
\   LEA                   16 32 64    same        <= mode
\   MOVSX/ZX            8 16 32       16 32 64    opnd1 < opnd2 <= mode
\   JMP/Jcc             8    32
\   FILD FISTP FISTTP     16 32 64                in any mode
\   FIxxx                 16 32                   in any mode
\   FLD FSTP                 32 64 80             in any mode

\ ------------- Operand size prefixes...  ------------------

\   Mode    Default  Required  Prefix   REX.W
\    64       32        64       --      48
\                       32       --      --
\                       16       66      --
\    32       32        32       --      --
\                       16       66      --
\
\   Some opcodes do not have a default 32bit operand in 64bit mode.
\   These default to a 64bit and can't be encoded in 32bit operand.
\   No REX.W is required. They are:
\    call   enter   jcc     jcxz/jecxz/jrcxz
\    jmp    leave   loop/loopcc
\    pop    push    popf    pushf  ret
\   (But FAR calls and jumps can have REX.W, although FAR operands
\   are not currently supported.)

: opnd-8bit?    ( -- f ) size-mem @  8bit = ;
: opnd-32bit?   ( -- f ) size-mem @ 32bit = ;
: opnd-64bit?   ( -- f ) size-mem @ 64bit = ;
: opnd-128bit?  ( -- f ) size-mem @ 128bit = ;
: ?opnd-8only   ( -- ) opnd-8bit? 0= _?invosz ;

: ?opnd-16>mode ( -- ) size-mem @ 16bit mode @ between 0= _?invosz ;

: _size-set ( opndsz var -- ) ( set data size for memory )
    dup>r @ ?dup if over <> _?misosz then r> ! ;
: /size-mem ( opndsz -- ) size-mem _size-set ; ( set data size for memory )
: /size-odd ( opndsz -- ) size-odd _size-set ; ( set data size for odd )

: mod-size ( opndsz -- ) \ data size modifiers
    create data-d,
    does> ( -- ) data-d@ ?modsize+
      oddsize flag-set? if /size-odd else /size-mem then ;

in-asm

    8bit mod-size   byte
   16bit mod-size   word
   32bit mod-size   dword
   32bit mod-size   single     ' single   alias float
   64bit mod-size   qword
   64bit mod-size   double
   80bit mod-size   extended   ' extended alias tbytes
  128bit mod-size   dqword
  256bit mod-size   qqword

in-hidden

\ ----------- Code "parser"  -------------------

: next-opnd ( -- ) ( next operand )
    depth-change _?unexim ( unexpected immed )
    opnd-count 1+ to opnd-count -2 allow-state +! ;

: mov-op? ( -- f ) ( is this a 64bit mov op )
    operation @ 1+ 0= ;

in-asm

: /# ( ? -- ) ( process immediate )
    in{}? _?miscl}             ( immediate must be outside of address )
    ?immed+ var-immed !
    mov-op? opnd-64bit? and if ( 64bit mov )
      depth-change if var-extra ! has/64+ else true _?misi64 then
    then next-opnd ;

: /i ( index -- ) ( indexed register operand { reg *n } )
    ?indexed+ hold-index ! ;

: /b ( base -- ) ( set base otherwise index )
    based? if /i else based+ hold-base ! then ;

: /d ( ? -- ) ( displacements )
    ?disp+ var-disp !
    mov-op? if ( 64bit mov )
      {}depth-change if var-extra ! has/64+ then
    then ;

: /d32 ( ? -- ) ( force 32bit disp )
    /d disp-32bit! ;

: /s ( scale -- ) ( scale *n )
    ?scaled+
    indexed? 0= if ( not indexed )
      based? if ( base, set base -> index )
        based- -1 hold-base dup @ /i !
        else true _?sclnoi then ( scale with no index register )
    then
    hold-index @ xsp? _?espinx ( can't use ESP/RSP )
    hold-scale ! ;

in-hidden

: s: ( scale -- ) ( scale *n )
    create data-c,  does> data-b@ /s ;

in-asm

1 s: *2 ( scale factors in sib )
2 s: *4
3 s: *8
def-size-mem @ 32bit = [if] ' *4 [else] ' *8 [then] alias *cell

in-hidden

: ?valid-reg ( reg type -- ) ( check for valid register; mmx/float max is 8 )
    [ mreg freg or ] literal and mode32? or if #8 else #16 then
    0 swap within 0= _?invreg ;

: /reg ( reg size type -- ) ( set reg & right size )
    >r over r@ ?valid-reg r>           ( check valid reg # )
    in{}? if                           ( if in memory operand )
      rreg <> _?invrty                 ( check register type is general purpose rreg )
      ?dup if /size-addr then /b       ( set addrsize & base )
    else ( process bare reg; any reg type )
      dup curr-state+                  ( update state with regtype )
      2dup                             ( size type )
      flag-notset? _?notreg            ( is this type of reg allowed? )
      oddsize flag-set? if             ( is not a gp reg or is an oddsize? )
        /size-odd else /size-mem
      then
      reg-count case
        0 of         hold-regt  ! hold-regsz  ! hold-reg  !  endof
        1 of direct+ hold-baset ! hold-basesz ! hold-base !  endof
        2 of         hold-3rdt  ! hold-3rdsz  ! hold-3rd  !  endof
      endcase
      1 +to reg-count next-opnd ( flag this as a reg )
    then  ;

: greg: ( size type -- ) ( general register )
    create 2pack data-d, does> data-d@ unpack2 /reg ;

in-asm

  8bit rreg greg: /rb ( regs like al )
 16bit rreg greg: /rw ( regs like ax )
 32bit rreg greg: /rd ( regs like eax )
 64bit rreg greg: /rq ( regs like rax )
 64bit mreg greg: /rm ( regs like mm1 )
128bit xreg greg: /rx ( regs like xmm1 )
256bit xreg greg: /ry ( regs like ymm1 )

: { ( -- ) ( start effective address )
    ?in{}+ ( check & set )
    {}save-depth
    opnd-count if direct+ then ;

: } ( ? -- ) ( possible displacements on stack )
    in{}? 0= _?misop{ ( no opening { )
    {}depth-change if /d then ( process possible displacement )
    memory? 0= _?mempty  ( empty effective address )
    hasrip? if ( if there's a rip addr )
      memory? disp <> has/64? or _?notrip ( *** FIX and other than 32bit displacement )
    then
    in{}- next-opnd ;

in-hidden

: setup-sizes ( -- )
    size-addr @ dup 0= if drop mode @ dup size-addr ! then
    mode @ <> if $67 pfx-addr ! then   ( switch )

    size-mem @ dup 0= if ( set rex.w or 66 prefix )
      drop def-size-mem @ dup size-mem !
    then
    dup 16bit = if $66 pfx-opnd ! drop else
        64bit = if rex!w               then then

    size-disp @ 0= if ( set displacement size if not set )
      var-disp @ 1byte? if 8bit else 32bit then size-disp !
    then

    size-immed @ 0= if ( set immediate size if not set )
      var-immed @ 1byte? if
        8bit else size-mem @ 32bit min ( might be 16bit )
      then size-immed !
    then ;

in-asm

: a; ( -- ) ( assemble the code )
    gen-op @ if
      depth-change if /# then        ( process poss immed )
      opnd-count opnd-min < _?2fewop ( too few operands )
      setup-sizes                    ( sort out data sizes )
      operation @ gen-op @ execute operation ! ( run the opcode generator )
      gen-r/m-sib                    ( generate the mod-r/m )
      build                          ( write out the instruction )
    then /reset ;

in-hidden
octal

\ -------------- Operand tables  -------------------

: register: ( reg# size type -- )
    create 3pack data-d,
    does> ( -- reg# size type ) data-d@ unpack3 /reg ;

\ OPGRP: Operand groups specify a common routine and a set of check bits
\ for processing & validating opcodes that use this group
\ xt-gen            Generator for this set of opcodes, often a :NONAME
\ min               Minimum number of operands
\ op1 op2 op3 op4   Operand types -- see operand type bits above. If the
\                   value is zero, no operand is allowed from this position on
\ <-name->          Name of the opcode group, used by OP:
((
begin-structure %opgrp
    wfield: opgrp.op4
    wfield: opgrp.op3
    wfield: opgrp.op2
    wfield: opgrp.op1
    lfield: opgrp.xt
    wfield: opgrp.min
    2 +
end-structure
))
: opgrp: ( xt-gen min op1 op2 op3 op4  <-name-> -- )
    create
      data-w, data-w, data-w, data-w, ( allowed states )
      swap
      data-d, ( xt-gen )
      data-w, ( min operand count )
      0 data-w, ( pad )
    does>
      dup #6 + allow-state ! ( reversed list, op1 leftmost )
      dup [ 2 cells ] literal + data-d@ gen-op !
          [ 3 cells ] literal + data-w@ to opnd-min ;

\ OP: Opcode bytes are generated from left to right as they are specified.
\ Extended opcodes with $F2, $F3 or $66 mandatory prefixes are
\ specified with the prefix as the first byte in a 32bit number;
\   PHADDW is the sequence 66 0F 38 01 specified as $660F3801
\   POPCNT is the sequence    F3 0F B8 specified as $F3000FB8
\                                      and *not* as $00F30FB8
\ The remaining bytes are not generated if they are zero
\ except for the last byte; so it possible to encode
\ for xx yy 00 but not xx 00 yy

: op: ( opcode opgroup -- )
    get-current >r in-asm ( create in asm wordlist )
    create
      swap data-d, data-d,
      r> set-current
    does>
      >r a; r>        ( terminate previous instruction )
      2@ operation ! execute ; \ set the gen field, set up group for this opcode

: seg: ( seg -- ) ?segreg+ pfx-seg ! ; ( process seg reg fs: or gs: )

in-asm

\ ------------------ Registers -----------------------

: rip ( set relative to ip addr )
    ?mode32 in{}? 0= _?notmem hasrip? _?dupopr hasrip+ ;

( mode 32bit or 64bit )
( val | dt-sz or addr-sz | type      | op )
  0     8bit  rreg register:       al
: cl ( -- ) ( special immed reg )
    regcl flag-set? if regcl+ next-opnd else 1 /rb then ;
  2     8bit  rreg  register:       dl
  3     8bit  rreg  register:       bl
  4     8bit  rreg  register:       ah
  5     8bit  rreg  register:       ch
  6     8bit  rreg  register:       dh
  7     8bit  rreg  register:       bh
((
: ah ( -- ) ?mode64 4 /rb ; ( *** not correct, only invalid if used with REX )
: ch ( -- ) ?mode64 5 /rb ;
: dh ( -- ) ?mode64 6 /rb ;
: bh ( -- ) ?mode64 7 /rb ;
))

  0    16bit  rreg  register:       ax
  1    16bit  rreg  register:       cx
  2    16bit  rreg  register:       dx
  3    16bit  rreg  register:       bx
  4    16bit  rreg  register:       sp
  5    16bit  rreg  register:       bp
  6    16bit  rreg  register:       si
  7    16bit  rreg  register:       di

  0    32bit  rreg  register:      eax
  1    32bit  rreg  register:      ecx
  2    32bit  rreg  register:      edx
  3    32bit  rreg  register:      ebx
  4    32bit  rreg  register:      esp
  5    32bit  rreg  register:      ebp
  6    32bit  rreg  register:      esi
  7    32bit  rreg  register:      edi

( mode 64bit only )
( special 64bit mode 8bit registers, requires rex.z set )
: spl ( -- ) rex!z 4 /rb ;
: bpl ( -- ) rex!z 5 /rb ;
: sil ( -- ) rex!z 6 /rb ;
: dil ( -- ) rex!z 7 /rb ;
 10     8bit  rreg  register:      r8b
 11     8bit  rreg  register:      r9b
 12     8bit  rreg  register:     r10b
 13     8bit  rreg  register:     r11b
 14     8bit  rreg  register:     r12b
 15     8bit  rreg  register:     r13b
 16     8bit  rreg  register:     r14b
 17     8bit  rreg  register:     r15b

 10    16bit  rreg  register:      r8w
 11    16bit  rreg  register:      r9w
 12    16bit  rreg  register:     r10w
 13    16bit  rreg  register:     r11w
 14    16bit  rreg  register:     r12w
 15    16bit  rreg  register:     r13w
 16    16bit  rreg  register:     r14w
 17    16bit  rreg  register:     r15w

 10    32bit  rreg  register:      r8d
 11    32bit  rreg  register:      r9d
 12    32bit  rreg  register:     r10d
 13    32bit  rreg  register:     r11d
 14    32bit  rreg  register:     r12d
 15    32bit  rreg  register:     r13d
 16    32bit  rreg  register:     r14d
 17    32bit  rreg  register:     r15d

  0    64bit  rreg  register:      rax
  1    64bit  rreg  register:      rcx
  2    64bit  rreg  register:      rdx
  3    64bit  rreg  register:      rbx
  4    64bit  rreg  register:      rsp
  5    64bit  rreg  register:      rbp
  6    64bit  rreg  register:      rsi
  7    64bit  rreg  register:      rdi
 10    64bit  rreg  register:       r8
 11    64bit  rreg  register:       r9
 12    64bit  rreg  register:      r10
 13    64bit  rreg  register:      r11
 14    64bit  rreg  register:      r12
 15    64bit  rreg  register:      r13
 16    64bit  rreg  register:      r14
 17    64bit  rreg  register:      r15

: fs: $64 seg: ;
: gs: $65 seg: ;

in-hidden

\ ----------- Opcode and opcode generators --------

\ ----------- Opcode modifications  ---------------

: exch-reg&base ( -- ) ( exchange reg and base fields )
    hold-reg   hold-base   exchange
    hold-regt  hold-baset  exchange
    hold-regsz hold-basesz exchange ;

: base<->reg ( -- ) ( swap base & reg if no mem references )
    memory? 0= if exch-reg&base then ;
: op+sign   ( op -- op+s ) ( sign extend immediate, bit 1 )
    imm-8bit? if 002 or then ;
: op+width  ( op -- op+w ) ( if not 8 bit, set full width, bit 0 )
    opnd-8bit? 0= if 001 or then ;
: op+direct ( op -- op+d ) ( set direction, bit 1 )
    direct? if 002 or then ;
: op+reg ( op -- op+reg ) ( reg operand in opcode )
    base<->reg rex!b> or operands- ;
: install-/r ( r -- ) ( install r of mod-r/m )
    hold-reg reg-count if ( if reg occupied, move to base first )
      dup @ hold-base !
    then ! ;

\ ----------- No operand simple opcodes ------------

:noname ( op -- op ) pfx-rex.w off ( reset rex.w ) ;
    0 0 0 0 0
    opgrp: simple

in-asm

    $90 ' simple           op: nop
    $9b ' simple           op: wait
    $f8 ' simple           op: clc
    $fc ' simple           op: cld
    $f5 ' simple           op: cmc
    $f9 ' simple           op: stc
    $fd ' simple           op: std
  $0f0b ' simple           op: ud2
  $0fa2 ' simple           op: cpuid
  $0f31 ' simple           op: rdtsc
    $9c ' simple           op: pushf
    $9d ' simple           op: popf

( *** no prefix checks are done )
    $f3 ' simple           op: rep    ' rep dup alias repe alias repz
    $f2 ' simple           op: repne  ' repne alias repnz
    $f0 ' simple           op: lock

in-hidden

\ ----------- Group 1 opcodes ---------------------

\ Arithmetic and Logic (ALU)
\   ??? dest, src ( typeG1 )
\
\ Eight instructions following a pattern, with three different forms.
\   0p0+dw xrm      op r/m, reg
\   200+sw xpm imm  op r/m, imm  202 (sign extend byte->byte) is invalid
\   0p4+w imm       op acc, imm  acc is al, ax, eax, rax
\
\   p     0    1    2    3    4    5    6    7
\         add  or   adc  sbb  and  sub  xor  cmp
\
\ Immediates can be 8, 16 or 32bits in size. The shortest (including
\ any required prefix) is generated.
\
\ 8bit
\    0p4 i                 2        8bit  al
\    200 xx i              3+       otherwise
\
\ 16bit
\    203 xx d  i           3/4      abs i <= 127, d <= disp8
\    0p5 i  i              3        ax
\    201 xx ss i  i        4/5      otherwise
\
\ 32 & 64bit
\    203 xx d  i           3/4      abs i <= 127, d <= disp8
\    0p5 i  i  i  i        5        eax/rax
\    201 xx i  i  i  i     5+       otherwise

:noname ( p -- op ) ( gen-op grp1 code, op is p )
    immed? if ( immed operand )
      disp-8bit? imm-8bit? and opnd-8bit? 0= and if
        install-/r 200 op+sign
      else
        eax? if
          3 lshift 004 or immed! ( set as accumulator , immed )
        else
          install-/r 200 opnd-8bit? 0= if op+sign then
        then
      then
    else ( rreg/mem operand )
      3 lshift op+direct 
    then
    op+width ;
    2 rreg/mem rreg/mem/i 0 0 opgrp: grp1

     $0 ' grp1             op: add     \ lock
     $1 ' grp1             op: or      \ lock
     $2 ' grp1             op: adc     \ lock
     $3 ' grp1             op: sbb     \ lock
     $4 ' grp1             op: and     \ lock
     $5 ' grp1             op: sub     \ lock
     $6 ' grp1             op: xor     \ lock
     $7 ' grp1             op: cmp     \ NO LOCK!

\ ----------- INC/DEC opcodes ---------------------
\ 376 x0m    INC rreg/mem
\ 100+r      INC reg ( in mode32 only, not 8 bit )
\ 376 x1m    DEC rreg/mem
\ 110+r      DEC reg ( in mode32 only, not 8 bit )

:noname ( op -- op' )
    memory? mode64? opnd-8bit? or or if
      010 and 3 rshift install-/r 376 op+width
    else op+reg then ;
    1 rreg/mem 0 0 0 ( inline opcode gen )
    opgrp: grp-incdec

    $40 ' grp-incdec       op: inc
    $48 ' grp-incdec       op: dec

\ ----------- LEA opcode ---------------------------
\  Can have data prefix and rex!w set; reg can be 16, 32 or 64bit
\    215 xrm  ( RM )      LEA Load Effective Address

' ?opnd-16>mode 2 rreg memory 0 0 opgrp: grp-lea

    $8d ' grp-lea           op: lea

\ ----------- Group 2 opcodes ---------------------
\ Shift/Rotate
\   ??? dest, imm | CL ( M1 MC MI )
\
\ Eight instructions following a pattern, with three different forms.
\   300+w xpm imm8        op r/m, imm   Rotate by a number (modulo opsize)
\   320+w xpm             op r/m, 1     Rotate by one
\   322+w xpm             op r/m, CL    Rotate by value in CL register
\
\       p     0    1    2    3    4    5    6*   7
\             rol  ror  rcl  rcr  shl  shr  sal  sar
\                                 sal       shl

:noname ( op -- op' )
    install-/r 300 op+width
    regcl? if
      immed- 022 or
    else
      var-immed @ 1 = if immed- 020 or else imm-8bit! then
    then ;
    2 rreg/mem immed regcl + 0 0 ( inline opcode gen )
    opgrp: grp2

     $0 ' grp2               op: rol
     $1 ' grp2               op: ror
     $2 ' grp2               op: rcl
     $3 ' grp2               op: rcr
     $4 ' grp2               op: shl \ same as sal
     $5 ' grp2               op: shr
     $6 ' grp2               op: sal \ same as shl
     $7 ' grp2               op: sar

\ ----------- SHL|RD opcodes ----------------------
\ Four instructions following a pattern, with two different forms.
\   017 244 xrm imm8  SHLD r/m, reg, imm  shift left imm bits double
\   017 245 xrm       SHLD r/m, reg, cl   shift left cl bits double
\   017 254 xrm imm8  SHRD r/m, reg, imm  shift right imm bits double
\   017 255 xrm       SHRD r/m, reg, cl   shift right cl bits double

:noname ( op -- op' )
    base<->reg
    regcl? if immed- 001 or else imm-8bit! then ;
    3 rreg/mem rreg immed regcl + 0 ( inline opcode gen )
    opgrp: shd-compile

  $0fa4 ' shd-compile      op: shld
  $0fac ' shd-compile      op: shrd

\ ----------- Group 3 opcodes ---------------------
\ ??? r/m ( RM )
\ imul is 1 op group 3; see grp3b for 2/3 op forms.
\   366+w xpm         op r/m
\
\     p     0    1    2    3    4    5    6    7
\                     not  neg  mul  imul div  idiv
\
\   017 257 xrm      IMUL reg, mem
\   151+s xrm imm    IMUL reg, mem, imm8|imm32

: grp3a-gen ( p -- op ) ( group 3 opcodes, 1 operand, p is opcode )
    install-/r 366 op+width ;

' grp3a-gen 1 rreg/mem 0 0 0 opgrp: grp3a

     $2 ' grp3a              op: not        \ lock
     $3 ' grp3a              op: neg        \ lock
     $4 ' grp3a              op: mul
     $6 ' grp3a              op: div
     $7 ' grp3a              op: idiv

:noname ( 5 -- op ) ( group 3 opcode for IMUL )
    opnd-count dup
    1 = if drop grp3a-gen
    else ( count is 2 or 3 )
      ?opnd-16>mode
      memory? direct? 0= and _?reg1st ( first operand must be register )
      2 = if ( 2 operands, reg, mem )
        eax? if
          exch-reg&base grp3a-gen ( make imul 1op )
        else drop $0faf then
      else
        drop $69 op+sign ( 3 operands, reg, mem, imm )
      then
    then ;
    1 rreg/mem rreg/mem immed 0 
    opgrp: grp3b

     $5 ' grp3b             op: imul    \ takes 1-3 operands

\ ----------- MOV opcodes -------------------------
\ MOV dest, src ( typeG1 )
\ No support for debug, segment registers
\
\ Immediates:
\   26r    imm8               MOV reg8, imm8
\   27r    imm32              MOV reg16/32/64, imm8/16/32
\   REX.W 27r imm64           MOV reg64, imm64  64bit mode has 64bit immediate
\   306+w  xrm imm8/16/32     MOV r/m, imm      sign extend if r/m 64bits
\
\ 64bit mode:
\   If the immediate is 64bits in size, generate REX.W 27r imm64 (10 bytes). 
\   Otherwise generate as in the 32bit case.
\   The immediate field is only 1 cell long (that is 32bits on a 32bit system). To
\   generate a 64bit immediate, a double IMMED is used. The signature for this is
\   MOV RBX $11223344 $55667788 which generates (hex) 48 BB 88 77 66 55 44 33 22 11
\
\ Examples in mode64:
\ 32bit register destinations E.X are zero extended into 64bit R.X
\ 64bit memory destinations are 32bit immediate sign extended
\     mov ebx , -1             ( zero extend to RBX 00 00 00 00 FF FF FF FF )
\     mov rbx , -1             ( full RBX           FF FF FF FF FF FF FF FF )
\     mov qword { mem } , -1   ( sign extend        FF FF FF FF FF FF FF FF )
\ To place a 64bit immediate in memory;
\     mov rcx, -1   ( 64bit immediate )
\     mov qword { rdx } rcx
\ To sign extend into a 64bit register;
\     movsx rbx [ 1 2 or 4 byte rreg/mem ]
\
\ 32bit mode:
\   Immediates can be 8, 16 or 32bits in size. The shortest (including any required
\   prefix) is generated; 26r and 27r are shortest.

: mov-imm ( -- ) ( generate mov imm )
    reg-count if
      270 op+reg immed!
      opnd-64bit? 0= if
        pfx-rex.w off
        opnd-8bit? if [ 010 invert ] literal and then
      else
        rex!w
      then
    else
      0 install-/r 306 op+width
    then 32bit size-mem @ min size-immed ! ;

\ Other addressing modes.
\   210+dw xrm                MOV r/m, reg
\   240+dw disp8/32           MOV AL/AX/EAX, disp8/32
\   240+dw disp64             MOV RAX, disp64   64bit mode has 64bit displacement
\ Direction bit is swapped on 240+dw
\
\ 64bit mode:
\   If the register is RAX and disp only: REX.W 240+dw disp64.
\
\   Otherwise, for RIP addressing:
\      If RIP is specified, 210+dw 0r5 disp32
\      If no RIP, generate 210+dw 0r4 045 disp32
\   RIP and a 64bit displacement are mutually exclusive; see gen-mod-r/m for
\   handling of the RIP when generating RIP addresses. RIP can only be specified
\   with a displacement.
\
\   To generate a 64bit displacement, a double DISP is used. The signature for this is
\   MOV { $11223344 $55667788 } RAX which generates (hex) 48 A3 88 77 66 55 44 33 22 11
\
\ 32bit mode:
\   Due to memory addresses commonly requring 4 bytes, the 5 byte form ACC form
\   is generally the shortest
\     240+dw d              5           MOV acc, mem   direction bit inverted
\     210+dw xrm            3/4         MOV r/m, reg   no disp or disp 8
\     210+dw xrm            6+          otherwise

: mov-240 ( -- ) ( generate absolute 240+dw d )
    immed! imm-32bit! ( ensure 32bit immed for build )
    var-disp @ var-immed ! ( move to immed )
    direct? if direct- else direct+ then ( swap for acc, imm )
    240 ;

: mov-oth ( -- )
    eax? memory? disp = and if ( acc,mem )
      has/64? mode32? or if
        mov-240 ( use absolute )
      else
        hasrip? pfx-rex.w @ or if 210 else mov-240 then
      then
    else
      has/64? _?not/64 210
    then op+direct op+width ;

:noname ( op -- op' )
    drop immed? if mov-imm else mov-oth then ;
    2 rreg/mem rreg/mem/i 0 0
    opgrp: grp-mov

    -1 ' grp-mov           op: mov  ( -1 is signal to test for 64bit )

\ ----------- Group 8 opcodes ---------------------
\ Bit Test
\   ??? dest, reg | imm ( typeG8 )
\
\ Four instructions following a pattern, with two different forms.
\ ( 017 is 2byte opcode $0f )
\   017 2p3 xrm           op r/m, reg
\   017 272 xpm imm8      op r/m, imm8
\
\       p     0    1    2    3    4    5    6    7
\                                 bt   bts  btr  btc

:noname ( op -- op' )
    base<->reg  ( swap if reg reg )
    ?opnd-16>mode ( check 16bit or bigger )
    immed? if
      3 rshift 7 and install-/r ( ..xxx... bits )
      $0fba imm-8bit! ( new opcode & force 8 bit immediate )
    then ;
    2 rreg/mem rreg immed + 0 0
    opgrp: grp8

  $0fa3 ' grp8             op: bt
  $0fab ' grp8             op: bts     \ lock
  $0fbb ' grp8             op: btc     \ lock
  $0fb3 ' grp8             op: btr     \ lock

\ ----------- MOVZX/MOVSX opcodes ------------------
\   017 266+w        MOVZX reg, r/m   Zero-extend byte to word
\   017 276+w        MOVSX reg, r/m   Sign-extend byte to word
\   143              MOVSXD reg, r/m  Sign extend dword to qword
\   reg data size must be greater than the data size of r/m
\ No default size is specified; a modifier must be used. The
\ opcode MOVSXD is not supported; use MOVSX with 64,32 operands.
\ For MOVZX 64,32 use MOV 32,32

:noname ( op -- op' )
    size-odd @ dup
    dup 0= _?reqosz ( requires modifier )
    8bit size-mem @ within 0= _?invosz
    dup size-mem !
    32bit = if
      $0fb6 = _?invosz ( error if movzx and 32bit )
      $63 ( make movsxd )
    else
      op+width
    then ;
    2 rreg rreg/mem oddsize + 0 0
    opgrp: grp-movx

  $0fbe ' grp-movx         op: movsx
  $0fb6 ' grp-movx         op: movzx

\ ----------- BSWAP opcode -------------------------
\   017 310+rd      BSWAP reg 32 or 64bit reg only

:noname ( op -- op' )
    size-mem @ 32bit < _?invosz ( must be 32/64bit register )
    op+reg ;
    1 rreg 0 0 0
    opgrp: grp-bswap

  $0fc8 ' grp-bswap       op: bswap

\ ----------- MOVBE opcode -------------------------
\   017 070 360+d xrm  MOVBE not 8bit, reg,mem or mem,reg

:noname ( op - op' )
    ?opnd-16>mode
    memory? 0= _?memreq ( one operand must be memory )
    op+direct ;
    2 rreg/mem rreg/mem 0 0
    opgrp: grp-movbe

$0f38f0 ' grp-movbe       op: movbe

\ ----------- INT/RET opcodes ---------------------
\ A bare RET or RET 0 generates $C3; not $C2000
\ INT 3 generates $CC form

:noname ( op -- op' )
    pfx-rex.w off
    var-immed @ if 1- 16bit size-immed ! else immed- then ;
    0 immed 0 0 0
    opgrp: grp-ret

    $c3 ' grp-ret          op: ret
    $cb ' grp-ret          op: ret.far

:noname ( op -- op' )
    pfx-rex.w off
    var-immed @ 3 = if 1- immed- else imm-8bit! then ;
    1 immed 0 0 0
    opgrp: grp-int

    $cd ' grp-int          op: int
    $cc ' simple           op: int3

\ ----------- STRING opcodes ---------------------
\ Opcode of the format
\    244+tw  t    0     2      4      6      8     10
\                movs  cmps          stos   lods  scas
\ No default size is specified; a modifier must be used.

:noname ( op -- op' )
    modsize? 0= _?invosz ( string size not specified )
    op+width ;
    0 modsize 0 0 0
    opgrp: grp-str   ( string )

    $a4 ' grp-str          op: movs    \ rep
    $a6 ' grp-str          op: cmps    \ repe repne repz repnz
    $aa ' grp-str          op: stos    \ rep
    $ac ' grp-str          op: lods    \ rep
    $ae ' grp-str          op: scas    \ repe repne repz repnz

\ ----------- SETCC opcodes -----------------------
\ 16 instructions following a pattern
\   017 220+cc x0m         op reg/mem8

:noname ( op -- op' ) ?opnd-8only 0 install-/r ;
    1 rreg/mem 0 0 0
    opgrp: grp-set

in-asm

  $0f90 ' grp-set          op: seto
  $0f91 ' grp-set          op: setno
  $0f92 ' grp-set          op: setb   ' setb  dup alias setc  alias setnae
  $0f93 ' grp-set          op: setae  ' setae dup alias setnb alias setnc
  $0f94 ' grp-set          op: sete   ' sete      alias setz
  $0f95 ' grp-set          op: setne  ' setne     alias setnz
  $0f96 ' grp-set          op: setbe  ' setbe     alias setna
  $0f97 ' grp-set          op: seta   ' seta      alias setnbe
  $0f98 ' grp-set          op: sets
  $0f99 ' grp-set          op: setns
  $0f9a ' grp-set          op: setp   ' setp      alias setpe
  $0f9b ' grp-set          op: setnp  ' setnp     alias setpo
  $0f9c ' grp-set          op: setl   ' setl      alias setnge
  $0f9d ' grp-set          op: setge  ' setge     alias setnl
  $0f9e ' grp-set          op: setle  ' setle     alias setng
  $0f9f ' grp-set          op: setg   ' setg      alias setnle

in-hidden

\ ----------- TEST opcodes ------------------------
\ Read-only AND; sets FLAGS but discards its result.
\   204+w xrm           TEST reg, r/m
\   250+w imm           TEST EAX, imm
\   366+w x0m imm       TEST r/m, imm

:noname ( op -- op' )
    drop immed? if
      size-mem @ 32bit min size-immed !
      eax? if immed! 250 else 0 install-/r 366 then
    else 204 then op+width ;
    2 rreg/mem rreg/mem/i 0 0
    opgrp: grp-test

      0 ' grp-test         op: test

\ ----------- XCHG opcodes ------------------------
\   206+w xrm        XCHG reg, r/m
\   22r              XCHG EAX, reg (XCHG EAX,EAX = 220 = NOP )

:noname ( 0 -- ) ( 2 operand, opcode is 0 )
    drop reg-count 1 > if
      hold-reg @ hold-base @ 2dup * 0= if ( is one reg eax? )
        + hold-reg ! 220 op+reg exit
      then 2drop
    then 206 op+width ;
    2 rreg rreg/mem 0 0
    opgrp: grp-xchg

      0 ' grp-xchg         op: xchg        \ lock

\ ----------- CMPXCHG/XADD opcodes -----------------
\   017 300+w xrm          XADD    r/m, reg
\   017 260+w xrm          CMPXCHG r/m, reg

:noname ( op -- op' ) base<->reg op+width ;
    2 rreg/mem rreg 0 0
    opgrp: grp-cx

  $0fc0 ' grp-cx           op: xadd
  $0fb0 ' grp-cx           op: cmpxchg   \ lock

\ ----------- CMPXCHG8/16B opcodes -----------------
\   017 307 xrm     CMPXCHG8B    mem
\   017 307 xrm     CMPXCHG16B   mem

:noname ( op -- op ) 1 install-/r ;
    1 memory 0 0 0
    opgrp: grp-cxn

: cx8b   ( -- ) grp-cxn dword ;
: cx16b  ( -- ) grp-cxn qword ;

  $0fc7 ' cx8b             op: cmpxchg8b \ lock
  $0fc7 ' cx16b            op: cmpxchg16b \ lock

\ ----------- Cxx sign extend opcodes ---------------------

' noop 0 modsize 0 0 0 opgrp: c-compile

: cw-compile ( -- ) c-compile [ also assembler ] word [ previous ] ;
: cd-compile ( -- ) c-compile dword ;
: cq-compile ( -- ) c-compile qword ;

    $98 ' cw-compile        op: cbw   ( AX <- sign-extend of AL )
    $98 ' cd-compile        op: cwde  ( EAX <- sign-extend of AX )
    $98 ' cq-compile        op: cdqe  ( RAX <- sign-extend of EAX )
    $99 ' cw-compile        op: cwd   ( DX:AX <- sign-extend of AX )
    $99 ' cd-compile        op: cdq   ( EDX:EAX <- sign-extend of EAX )
    $99 ' cq-compile        op: cqo   ( RDX:RAX <- sign-extend of RAX )

\ ----------- CMOVcc, bit opcodes ---------------------

' ?opnd-16>mode 2 rreg rreg/mem 0 0 opgrp: grp-cmov

in-asm

  $0fbc ' grp-cmov          op: bsf
  $0fbd ' grp-cmov          op: bsr

  $0f40 ' grp-cmov          op: cmovo
  $0f41 ' grp-cmov          op: cmovno
  $0f42 ' grp-cmov          op: cmovnae ' cmovnae dup alias cmovb  alias cmovc
  $0f43 ' grp-cmov          op: cmovnc  ' cmovnc  dup alias cmovae alias cmovnb
  $0f44 ' grp-cmov          op: cmove
                                    also assembler ' cmove previous alias cmovz
  $0f45 ' grp-cmov          op: cmovne  ' cmovne      alias cmovnz
  $0f46 ' grp-cmov          op: cmovna  ' cmovna      alias cmovbe
  $0f47 ' grp-cmov          op: cmovnbe ' cmovnbe     alias cmova
  $0f48 ' grp-cmov          op: cmovs
  $0f49 ' grp-cmov          op: cmovns
  $0f4a ' grp-cmov          op: cmovp   ' cmovp       alias cmovpe
  $0f4b ' grp-cmov          op: cmovnp  ' cmovnp      alias cmovpo
  $0f4c ' grp-cmov          op: cmovnge ' cmovnge     alias cmovl
  $0f4d ' grp-cmov          op: cmovnl  ' cmovnl      alias cmovge
  $0f4e ' grp-cmov          op: cmovng  ' cmovng      alias cmovle
  $0f4f ' grp-cmov          op: cmovnle ' cmovnle     alias cmovg

in-hidden

\ ----------- PUSH/POP opcodes ---------------------
\   150+s imm       PUSH imm    8 16 or 32 bits
\   12r             PUSH reg    16, 32 or 64
\   377 x6m         PUSH r/m    16, 32 or 64
\   13r             POP  reg
\   217 x0m         POP  r/m
\     rex!w byte is not encoded in 64bit mode; it's assumed
\     For all, 32/64bit reg or mem is invalid in 64/32bit mode
\     For all, 8 bit reg is invalid

: reset-rexw ( -- )
    mode @ size-addr @ <> _?invasz ( ensure right addr size )
    ?opnd-16>mode ( ensure right data size )
    pfx-rex.w off ;

: pp-op ( op op2 i -- op' ) ( set push/pop opcode )
    reg-count if
      2drop op+reg
    else
      install-/r nip
    then reset-rexw ;

:noname ( op -- ) ( 1 operand, 130 )
    217 0 pp-op ;
    1 rreg/mem 0 0 0
    opgrp: pop-compile

:noname ( op -- ) ( 1 operand, 120 )
    immed? if
      drop 150 op+sign
    else
      377 6 pp-op
    then ;
    1 rreg/mem/i 0 0 0
    opgrp: push-compile

    $50 ' push-compile     op: push
    $58 ' pop-compile      op: pop

\ ----------- CALL/JMP opcodes ---------------------
\   350 disp        CALL imm32      Relative displacement
\   377 x2m         CALL r/m        Absolute address
\   351+d disp      JMP imm8/imm32  Relative
\   377 x4m         JMP r/m         Absolute
\   160+cc disp     Jcc imm8        Relative
\   017 20+cc disp  Jcc imm32       Relative
\     Relative disp is indicated by an immediate value
\     rex!w byte is not encoded in 64bit mode; it's assumed
\     For all, 32/64 reg or mem is invalid in 64/32bit mode
\
\   CALL imm       ->  relative $e8 dd dd dd dd
\   CALL { disp }  ->  absolute indirect $ff 15 dd dd dd dd
\   CALL rreg/mem   ->  absolute indirect $ff xx ..
\
\ Specifying SHORT if the branch is too long will generate an error
\ *** needs work, want to generate 8bit jmp if we can even though
\ short not requested?

: cj-abs ( op n -- op' ) ( common install & reset rex for $FF opcodes )
    install-/r drop 377 reset-rexw ;
    
: cj-rel ( sz -- ) ( make immediate into a reloc )
    size-disp ! ( don't set disp+ ; there's no mod-r/m )
    curr-state off reloc+     ( request a relocated disp )
    var-immed @ var-disp ! ; 

:noname ( op -- op' ) ( generate call )
    immed? if 32bit cj-rel else 2 cj-abs then ;
    1 rreg/mem/i 0 0 0
    opgrp: call-compile

    $e8 ' call-compile     op: call

:noname ( op -- op' ) ( generate call.far )
    immed? if 32bit cj-rel else 3 cj-abs then ;
    1 rreg/mem/i 0 0 0
    opgrp: call.far-compile

    $9a ' call.far-compile     op: call.far

:noname ( op -- op' ) ( generate jump )
    immed? if ( relative call )
      opnd-8bit? if 002 + 8bit else 32bit then cj-rel
    else 4 cj-abs then ;
    1 rreg/mem/i 0 0 0
    opgrp: jmp-compile

    $e9 ' jmp-compile      op: jmp

:noname ( 160+cc -- op' )
    opnd-8bit? if 8bit else $0f10 + 32bit then cj-rel ;
    1 immed modsize + 0 0 0
    opgrp: grp-jcc

in-asm

: far true _?unsfar ;     \ far unsupported
' byte alias short
' dword alias long

    $70 ' grp-jcc          op: jo
    $71 ' grp-jcc          op: jno
    $72 ' grp-jcc          op: jb     ' jb   dup alias jnae  alias jc
    $73 ' grp-jcc          op: jae    ' jae  dup alias jnb   alias jnc
    $74 ' grp-jcc          op: je     ' je       alias jz
    $75 ' grp-jcc          op: jne    ' jne      alias jnz
    $76 ' grp-jcc          op: jbe    ' jbe      alias jna
    $77 ' grp-jcc          op: ja     ' ja       alias jnbe
    $78 ' grp-jcc          op: js
    $79 ' grp-jcc          op: jns
    $7a ' grp-jcc          op: jp     ' jp       alias jpe
    $7b ' grp-jcc          op: jnp    ' jnp      alias jpo
    $7c ' grp-jcc          op: jl     ' jl       alias jnge
    $7d ' grp-jcc          op: jge    ' jge      alias jnl
    $7e ' grp-jcc          op: jle    ' jle      alias jng
    $7f ' grp-jcc          op: jg     ' jg       alias jnle

in-hidden

\ ----------- LOOP/JCXZ opcodes ---------------------
\   340 disp8     LOOPNE/NZ
\   341 disp8     LOOPE/Z
\   342 disp8     LOOP
\   343 disp8     JCXZ/JECXZ/JRCXZ

:noname ( op -- op' ) ( check the operand vs mode )
    8bit cj-rel ;
    1 immed 0 0 0
    opgrp: grp-loop

in-asm

    $e0 ' grp-loop         op: loopne ' loopne   alias loopnz
    $e1 ' grp-loop         op: loope  ' loope    alias loopz
    $e2 ' grp-loop         op: loop
    $e3 ' grp-loop         op: jcxz   ' jcxz dup alias jecxz alias jrcxz


base !    

: /debug ( -- )
 base @ decimal
    cr ." mode: " mode @ #.
    cr ." op: " operation @ $.      \ the operation
       ." gen-op: " gen-op @ ?dup if .name then
    cr ." opnd-count: " opnd-count .
    cr ." state: " curr-state @ dup $.8 space binary #24 .r
    cr ." allow: " allow-state @ data-w@ dup $.8 space #24 .r decimal
    cr ." pfx-inst: " pfx-inst @ h.2    ( prefix )
       ."  pfx-seg: " pfx-seg @ h.2    ( segreg override prefix )
       ."  pfx-rex: " pfx-rex @ pfx-rex.w @ or h.2    ( rex override prefix )
    cr ." pfx-addr: " pfx-addr @ h.2    ( prefix )
       ."  size-addr: " size-addr @ .       \ addr size
    cr ." pfx-opnd: " pfx-opnd @ h.2    ( prefix )
       ."  size-mem: " size-mem @ .         \ data size
       ." size-odd: " size-odd @ .         \ odd data size
    cr ." direct: " direct? 0<> abs . ." reg-count " reg-count #.
    cr ." reg:  " hold-reg @ #. ." type: " hold-regt @ $. ." size: " hold-regsz @ #.
    cr ." base: " hold-base @ #.  ." type: " hold-baset @ $. ." size: " hold-basesz @ #.
    cr ." index: " hold-index @ #. ." scale: " hold-scale @ #.
    cr ." disp: " var-disp @ $. ." dispsz: " size-disp @ .
       ." immed: " var-immed @ $. ." size-immed: " size-immed @ . ." var-extra:" var-extra @ $.
    cr ." mod-r/m: " mod-r/m @ h.2 ."  sib: " sib @ h.2
    asm-start asm-len dump
    cr ." -----------------------------------------------------"

 base !
      ;

\s
\ Not implemented from basic set
\    pusha     pushad
\    popa      popad
\    xlat      xlatb
\    enter     leave
\ : grp-32b ( -- )
\     ?mode64 simple ;
\    $27 ' grp-32b       op: daa
\    $2f ' grp-32b       op: das
\    $37 ' grp-32b       op: aaa
\    $3f ' grp-32b       op: aas
\    $9e ' grp-32b       op: sahf
\    $9f ' grp-32b       op: lahf
\    $ce ' grp-32b       op: into
\  $d50a ' grp-32b       op: aad
\  $d40a ' grp-32b       op: aam




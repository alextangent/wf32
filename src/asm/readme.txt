1. Introduction

  This is a complete rewrite of the original Win32Forth assembler.
  All of the original code has been replaced, and in the belief that
  there is no code form the original version, this code is being issued
  under the BSD 2-Clause license.
  
2. License

  For the files
    AMSX64.F
    AMSX64-WF32.F
    ASMX64-CORE.F
    ASMX64-FLOAT.F
    ASMX64-EXTEND.F

  (http://www.opensource.org/licenses/BSD-2-Clause)

  Copyright (c) 2005, 2017 Alex McDonald
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:
  
      Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
     
      Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in
      the documentation and/or other materials provided with the
      distribution.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

3. Using the Assembler

  3.1 Files & their function
    AMSX64.F
    -- ASMX64-CORE.F
       -- ASMX64-MSGS.F
    -- ASMX64-EXTEND.F
    -- AMSX64-WF32.F

  3.2 ANS compliance

  3.3 Options and Assembler Directives
    
    The assembler supports 3 modes.

      MODE32/32  32bit address mode and a 32bit cell size
      MODE64/32  64bit address mode and a 32bit cell size
      MODE64/64  64bit address mode and a 64bit cell size

      MODE32/32 is the default mode.

4. Assembler Syntax

  4.1 Vocabularies

    All the assembler words are held in 2 vocabularies; ASSEMBLER and
    ASM-HIDDEN (defined in ASSEMBLER).

  4.2 Starting & ending assembly
  
    The ANS standard words CODE <name> and END-CODE are used to enter
    and exit the assembler. See "Macro facilities" for words that
    extend the utility of the assembler.

  4.3 Format of assembler instructions

    In the description that follows, := < > [ ] | * " are BNF-like meta-
    symbols.

    <instruction> := [ <label:> ] <op> [ <operands> ] [ A; ]
    <label:>      := @@0: @@1: ... @@9:
    <operands>    := [ <operand>*4 ]
    <operand>     := <label> | <reg> | [ <modifier> ] <memory> | <imm>
    <label>       := @@0 @@1 ... @@9
    <memory>      := { [ <segreg> ] [ <reg> ]
                       [ <reg> [ <scale> ] ] [ <disp> ] } |
                     { RIP <disp> } |
                     { <disp64> }
    <segreg>      := "FS:" | "GS:"
    <scale>       := "*2" | "*4" | "*8"
    <disp>        := n
    <disp64>      := n n
    <imm>         := n | <imm64>
    <imm64>       := n n

    n and n n
      is a 32bit number or cell in /32 modes; n n is either two 32bit
      numbers or a double celled value.

    <label:> and <label>
      A label for instructions, such as CALL, JMP and conditional
      jumps. The format is @@0: @@1: ... thru @@9:. The corresponding
      label used by the opcode is @@0 @@1 ... thru @@9. See Labels
      below.

    <op>
      The opcode. See below for a list of supported opcodes. Although
      some Forth assemblers permit the opcodes to follow the operands,
      in this assembler opcodes must precede the operands.

    <operands>
      0 to 4 space separated operands. The number and format of these
      are specified by the opcodes.

    <reg>
      The register name. In the core assembler, these are

        In MODE32/32:

        8bit:  al bl cl dl ah bh ch dh
        16bit: ax bx cx dx si di bp sp
        32bit: eax ebx ecx edx esi edi ebp esp

        In MODE64/32 or MODE64/64 the additional registers:

        8bit:  sil dil spl bpl
               r8b r9b r10b r11b r12b r13b r14b r15b
        16bit: r8w r9w r10w r11w r12w r13w r14w r15w
        32bit: r8d r9d r10d r11d r12d r13d r14d r15d
        64bit: rax rbx rcx rdx rsi rdi rbp rsp
               r8  r9  r10 r11 r12 r13 r14 r15

    <modifier>
      This modifies the size of the memory operand that follows it.
      Valid values are:

         byte word dword qword oword    8 16 23 64 and 128 bits
         single float extended tbyte    32 64 80 and 80 bits

    <memory> { ... }
      A memory operand refers to an effective address in memory. It's
      enclosed by { } (curly braces) and, where allowed, can only be
      specified once in the opcode's operands. There are 3 different
      memory operand types;

      { <disp64> } is a 64bit displacement. It's only specified on
      MOV <reg> opcodes. See MOV for details.

      { RIP <disp> } only applies in 64bit modes. This refers to
      an RIP relative address, as opposed to an absolute address.
      See RIP encoding below for details.

      { [ <segreg> ] [ <reg> ] [ <reg> [ <scale> ] ] [ <disp> ] }
      Only segment registers FS: and GS: are supported. At least one
      of the memory sub-operands, in addition to any segment register
      or scale if used, must be specified. If a scale is specified,
      it scales the register to its immediate left. All the other
      operands can be specified in any order.

  4.4 Opcodes

  In general, the opcodes and permitted operands follow the Intel/AMD
  documents with some minor variations.

    a. No comma is used to separate operands
    b. Memory operands are enclosed in curly braces { }
    c. For opcodes that move data, the target or desination is the
       first operand; the source is the second (Intel notation).

  4.4.1 Condition codes in opcodes
    
    Opcodes such as Jcc, SETcc and CMOVcc share a common set of
    condition codes. They are:
    
    cc  condition           aliases
    --  ---------           -------
    o   overflow            
    no  not overflow        
    a   above               nbe not below or equal
    b   below               nae not above or equal   c   carry
    ae  above or equal      nae not below            nc  no carry
    e   equal               z   zero
    ne  not equal           nz  not zero
    be  below or equal      na  not above
    s   signed              
    ns  not signed          
    p   parity              pe  parity even
    no  no parity           po  parity odd
    l   less than           nge not greater or equal
    ge  greater or equal    nl  not less than
    le  less than or equal  ng  not greater than
    g   greater than        nle not less than or equal

  4.4.2 Data Transfer Instructions

    MOV         CMOVcc      XCHG
    BSWAP       XADD        CMPXCHG/CMPXCHG8B
    PUSH        POP         CBW/CWDE/CDQE/CWD/CDQ/CQO
    MOVSX/MOVZX

    Not implemented: PUSHA/PUSHAD POPA/POPAD
    Implemented differently: MOVSXD

    MOV (64bit mode)
      64bit immediates are required when the target register is 64bit
      (registers RAX thru R15).
          mov rbx $11223344 $55667788

      A 64bit displacement is available on a MOV with specifically the
      register RAX only.
          mov rax { $11223344 $55667788 }
      generates a qword move from the absolute 64bit address.
          mov rax { RIP $11223344 }
      generates a qword move from an RIP+offset address.

    XCHG
      For <reg> rAX forms, the opcode is changed to generate rAX <reg>

    MOVSX and MOVZX
      The second memory operand must have a modifier specified, which
      needs to be smaller than the data size of the destination
      register.
          movsx eax byte { ebx }

    MOVSXD is implemented as MOVSX
      MOVSXD is implemented as a 32bit to 64bit MOVSX.
          movsx rbx dword { rcx }

  4.4.3 Binary Arithmetic & Logical Instructions

    ADD  ADC  SUB  SBB  IMUL MUL
    IDIV DIV  INC  DEC  NEG  CMP
    AND  OR   XOR  NOT
    
    IMUL 1 2 and 3 operand forms
      IMUL can take 1 to 3 operands. For 2 operand forms, if the
      destination regsiter is rAX, the opcode is modified to the
      equivalent 1 operand form.
          imul ebx              ( 1 operand, result in eax )
          imul eax ebx          ( same as above )
          imul ecx ebx          ( 2 operand )
          imul ecx ebx 4        ( 3 operand )

  4.5 Labels and macro labels

    Label references are of the form @@n, and labels are @@n: with a
    maximum of 10 labels (@@0 to @@9) and up to 100 forward
    references. Due to the way that code is generated, the reference
    to a forward label needs to be generated after the label is
    resolved. For instance

           mov { @@1 } 10   ...some code... @@1: ...

    The code generated by the MOV is

           C7 05 xx xx xx xx 0A 00 00 00 .. ..

    where xx xx xx xx represents the eventual address of @@1:. We
    pass a dummy address on the invocation of @@1, and mark the entry
    for @@1:  to do a later resolve by memoizing.

    Only 1 label per instruction can be requested, so this will
    generate the wrong code;

          mov { @@1 } @@2 ...
    
    Macros use exactly the same labels, but they are private to that
    macro.


5. Error Messages

          invalid in 64bit mode
          invalid in 32bit mode
          memory operand not allowed
          register type not allowed
          modifier not allowed
          immediate not allowed
          RIP register not allowed
          /64 not allowed
          operand duplicated
          invalid operand size
          invalid address size
          invalid register type
          can't use esp/rsp as index
          unexpected operand
          invalid register #
          mismatched address size       
          mismatched operand size
          scale with no index register
          no opening { found            
          no closing } found
          unexpected disp/immed
          too few operands              
          too many operands
          empty { }
          memory operand required
          register must be first operand
          short branch offset too big   
          too many open fwd refs        
          too many macro labels         
          unresolved fwd refs

6. The Disassembler

     The disassembly output buffer is formatted as
     ( $off )  opcode  operands                          \ hex dump
    
     Example of disassembly of DUP
    
     see dup
     code dup ( 1 -- 2 )
     \ dup is defined in src/kernel/gkernel32.fs at line 124
     \ ' nseopt compiled
     \ code=$401135 len=6 type=129
     ( $0 )    mov     dword { $-4 ebp } eax             \ 8945FC
     ( $3 )    sub     ebp $4                            \ 83ED04
     ( $6 )    ret                                       \ C3 ( end ) ok
    
    DIS ( addr -- )    disassembles from a specific address one line at
                       a time. To stop the disassembly, press Q
    SEE name           Full disassembly of the name. See above for DUP
    'SEE ( xt -- )     For unnnamed XTs; similar to DIS but assumes that
                       the code is an XT and is DESCRIBEable.
    DESCRIBE ( xt -- ) Provides a narrative for the XT; see DUP above


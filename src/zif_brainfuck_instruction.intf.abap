"! <p class="shorttext synchronized" lang="en">Brainfuck Instruction</p>
INTERFACE zif_brainfuck_instruction
  PUBLIC .

  TYPES t_instruction_type_char TYPE c LENGTH 1.

  TYPES:
    BEGIN OF ENUM t_instruction_type STRUCTURE instruction_type BASE TYPE t_instruction_type_char,
      unknown         VALUE IS INITIAL,
      plus            VALUE '+',
      minus           VALUE '-',
      right           VALUE '>',
      left            VALUE '<',
      put_char        VALUE '.',
      read_char       VALUE ',',
      jmp_if_zero     VALUE '[',
      jmp_if_not_zero VALUE ']',
      comment         VALUE '0',
      debugger        VALUE '#',
    END OF ENUM t_instruction_type STRUCTURE instruction_type.

  TYPES tt_instructions TYPE STANDARD TABLE OF REF TO zif_brainfuck_instruction WITH EMPTY KEY.

  DATA:
    type                 TYPE t_instruction_type READ-ONLY,
    source_code_location TYPE i,
    repeated             TYPE i,
    argument             TYPE i.

  CLASS-METHODS create
    IMPORTING
      i_instruction   TYPE t_instruction_type
      i_location      TYPE i DEFAULT 0
      i_repeated      TYPE i DEFAULT 1
    RETURNING
      VALUE(r_result) TYPE REF TO zif_brainfuck_instruction
    RAISING
      zcx_brainfuck_error.
ENDINTERFACE.

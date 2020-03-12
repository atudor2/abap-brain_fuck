"! <p class="shorttext synchronized" lang="en">Brainfuck Instruction</p>
INTERFACE zif_brainfuck_instruction
  PUBLIC .

  "! Instruction Type Enum (base type)
  TYPES t_instruction_type_char TYPE c LENGTH 1.
  "! Memory Cell Type
  TYPES t_memory_cell TYPE int1.
  "! Memory Cells
  TYPES tt_memory_cells TYPE STANDARD TABLE OF t_memory_cell WITH EMPTY KEY.

  TYPES:
    "! Instruction Type Enum
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

  "! Table of instructions
  TYPES tt_instructions TYPE STANDARD TABLE OF REF TO zif_brainfuck_instruction WITH EMPTY KEY.

  DATA:
    "! Instruction Type
    type                 TYPE t_instruction_type READ-ONLY,
    "! Original Source Code Location
    source_code_location TYPE i,
    "! Number of consecutive instructions
    repeated             TYPE i,
    "! Generic Instruction argument
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

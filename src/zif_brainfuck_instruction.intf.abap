"! <p class="shorttext synchronized" lang="en">Brainfuck Instruction</p>
INTERFACE zif_brainfuck_instruction
  PUBLIC .

  TYPES t_instruction_char TYPE c LENGTH 1.

  TYPES:
    BEGIN OF ENUM t_instruction STRUCTURE instructions BASE TYPE t_instruction_char,
      unknown         VALUE IS INITIAL,
      plus            VALUE '+',
      minus           VALUE '-',
      right           VALUE '>',
      left            VALUE '<',
      put_char        VALUE '.',
      read_char       VALUE ',',
      jmp_if_zero     VALUE '[',
      jmp_if_not_zero VALUE ']',
      comment         VALUE '#',
    END OF ENUM t_instruction STRUCTURE instructions.

  TYPES tt_instructions TYPE STANDARD TABLE OF zif_brainfuck_instruction=>t_instruction WITH EMPTY KEY.

  CLASS-METHODS get_as_instruction
    IMPORTING i_instruction   TYPE t_instruction_char
    RETURNING VALUE(r_result) TYPE zif_brainfuck_instruction=>t_instruction.

  CLASS-METHODS get_instruction_as_string
    IMPORTING i_instruction   TYPE zif_brainfuck_instruction=>t_instruction
    RETURNING VALUE(r_result) TYPE t_instruction_char.
ENDINTERFACE.

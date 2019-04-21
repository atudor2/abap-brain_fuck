"! <p class="shorttext synchronized" lang="en">Brainfuck Instruction</p>
CLASS zcl_brainfuck_instruction DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    INTERFACES zif_brainfuck_instruction.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS constructor
      IMPORTING
        i_instruction TYPE zif_brainfuck_instruction=>t_instruction_type
        i_location    TYPE i .
ENDCLASS.

CLASS zcl_brainfuck_instruction IMPLEMENTATION.
  METHOD zif_brainfuck_instruction~create.
    r_result = NEW zcl_brainfuck_instruction( i_instruction = i_instruction i_location = i_location ).
    r_result->repeated = COND #( WHEN i_repeated < 1 THEN 1 ELSE i_repeated ).
  ENDMETHOD.

  METHOD constructor.
    me->zif_brainfuck_instruction~type = i_instruction.
    me->zif_brainfuck_instruction~source_code_location = i_location.
  ENDMETHOD.
ENDCLASS.

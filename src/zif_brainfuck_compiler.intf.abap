"! <p class="shorttext synchronized" lang="en">Brainfuck Compiler</p>
INTERFACE zif_brainfuck_compiler
  PUBLIC .

  TYPES tt_instructions TYPE zif_brainfuck_instruction=>tt_instructions.

  METHODS compile
    IMPORTING
      i_code           TYPE string
      i_allow_debugger TYPE abap_bool DEFAULT abap_true
    EXPORTING
      et_instructions  TYPE tt_instructions
    RAISING
      zcx_brainfuck_error.
ENDINTERFACE.

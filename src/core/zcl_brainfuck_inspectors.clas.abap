CLASS zcl_brainfuck_inspectors DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS get_echo_inspector
      IMPORTING
        ir_output       TYPE REF TO zif_brainfuck_output_stream
      RETURNING
        VALUE(r_result) TYPE REF TO zif_brainfuck_inspector.

    CLASS-METHODS get_abap_breakpoint_inspector
      RETURNING
        VALUE(r_result) TYPE REF TO zif_brainfuck_inspector.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_brainfuck_inspectors IMPLEMENTATION.

  METHOD get_echo_inspector.
    r_result = NEW lcl_echo_inspector( ir_output ).
  ENDMETHOD.

  METHOD get_abap_breakpoint_inspector.
    r_result = NEW lcl_abap_breakpoint_inspector( ).
  ENDMETHOD.
ENDCLASS.

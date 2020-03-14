*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_echo_inspector DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_brainfuck_inspector.

    METHODS constructor
      IMPORTING
        ir_output TYPE REF TO zif_brainfuck_output_stream.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA output TYPE REF TO zif_brainfuck_output_stream.

    METHODS write_string_to_output
      IMPORTING
        i_string  TYPE string
        ir_output TYPE REF TO zif_brainfuck_output_stream.
ENDCLASS.

CLASS lcl_echo_inspector IMPLEMENTATION.

  METHOD zif_brainfuck_inspector~on_debug_instruction.
    output->flush( ).

    DATA(dbg_msg) = |IP = { i_state-instruction_pointer }, DP = { i_state-data_pointer }, Instruction = { CONV string( i_state-instruction->type ) }|.

    write_string_to_output( i_string = dbg_msg ir_output = output ).
  ENDMETHOD.

  METHOD write_string_to_output.
    DATA(i) = -1.
    DO strlen( i_string ) TIMES.
      i = i + 1.

      DATA(c) = i_string+i(1).
      ir_output->write_character( cl_abap_conv_out_ce=>uccpi( char = c ) ).
    ENDDO.

    ir_output->flush( ).
  ENDMETHOD.

  METHOD zif_brainfuck_inspector~start_of_instruction.
    RETURN.
  ENDMETHOD.

  METHOD zif_brainfuck_inspector~end_of_instruction.
    RETURN.
  ENDMETHOD.

  METHOD constructor.
    output = ir_output.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_abap_breakpoint_inspector DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_brainfuck_inspector.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_abap_breakpoint_inspector IMPLEMENTATION.

  METHOD zif_brainfuck_inspector~on_debug_instruction.
    BREAK-POINT.
  ENDMETHOD.

  METHOD zif_brainfuck_inspector~end_of_instruction.
    BREAK-POINT.
  ENDMETHOD.

  METHOD zif_brainfuck_inspector~start_of_instruction.
    BREAK-POINT.
  ENDMETHOD.

ENDCLASS.

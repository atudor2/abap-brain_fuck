"! <p class="shorttext synchronized" lang="en">Brainfuck Inspector Factory</p>
CLASS zcl_brainfuck_inspectors DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Gets an instance of the ECHO inspector</p>
    "! This inspector will dump the current state of the Brainfuck application to the output stream
    "! when a DEBUG instruction is hit
    "! @parameter ir_output | <p class="shorttext synchronized" lang="en">Brainfuck output stream to echo values to</p>
    "! @parameter r_result | <p class="shorttext synchronized" lang="en">Inspector instance</p>
    CLASS-METHODS get_echo_inspector
      IMPORTING
        ir_output       TYPE REF TO zif_brainfuck_output_stream
      RETURNING
        VALUE(r_result) TYPE REF TO zif_brainfuck_inspector.

    "! <p class="shorttext synchronized" lang="en">Gets an instance of the ABAP-BREAKPOINT inspector</p>
    "! This inspector will force an ABAP Debugger BREAK-POINT on the first instruction.
    "! This breakpoint will only be fired on the 1st instruction and can then be manually set in the debugger
    "! @parameter r_result | <p class="shorttext synchronized" lang="en">Inspector instance</p>
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

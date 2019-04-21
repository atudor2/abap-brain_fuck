*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_out DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_brainfuck_output_stream.
    DATA out TYPE REF TO if_oo_adt_intrnl_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_out IMPLEMENTATION.

  METHOD zif_brainfuck_output_stream~write_character.
    out->write_text( text = i_character ).
  ENDMETHOD.

ENDCLASS.

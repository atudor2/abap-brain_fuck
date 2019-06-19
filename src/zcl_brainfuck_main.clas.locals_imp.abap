*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_out DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_brainfuck_output_stream.
    DATA out TYPE REF TO if_oo_adt_intrnl_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA string TYPE string.
ENDCLASS.

CLASS lcl_out IMPLEMENTATION.
  METHOD zif_brainfuck_output_stream~write_character.
    CONCATENATE me->string i_character INTO me->string RESPECTING BLANKS. " Got to love automatic string trims... ;)
  ENDMETHOD.

  METHOD zif_brainfuck_output_stream~flush.
    out->write_text( text = me->string ).

    CLEAR me->string.
  ENDMETHOD.

  METHOD zif_brainfuck_output_stream~write_string.
    me->string = me->string && i_string.
  ENDMETHOD.
ENDCLASS.

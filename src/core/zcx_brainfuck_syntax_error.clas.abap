CLASS zcx_brainfuck_syntax_error DEFINITION
  PUBLIC
  INHERITING FROM zcx_brainfuck_error
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        message     TYPE string
        location    TYPE i
        source_code TYPE string
        textid      LIKE if_t100_message=>t100key OPTIONAL
        previous    LIKE previous OPTIONAL .

    METHODS get_error_message
      RETURNING VALUE(r_result) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA source_code TYPE string.
    DATA location TYPE i.
    DATA message TYPE string.
ENDCLASS.



CLASS zcx_brainfuck_syntax_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    me->message = message.
    me->location = location.
    me->source_code = source_code.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD get_error_message.
    " Print message in form of:
    " [Text]
    " At character [location]
    " [Source Code]
    " -----^ (pointer to error)

    r_result =  |Syntax error:\n{ me->message }\nAt character offset: { location }\n|.
    IF location <= 0.
      RETURN.
    ENDIF.

    " If the source code is multiple lines, the offset should be adjusted and indicator
    " drawn under the specific line
    DATA(stream) = NEW cl_abap_string_c_writer( ).
    stream->write( r_result ).

    DATA(code_len) = strlen( me->source_code ).
    DATA(err_location) = COND #( WHEN location > code_len THEN code_len ELSE location ).
    DATA(offset) = 0.
    DATA(error_line_drawn) = abap_false.

    SPLIT me->source_code AT |\n| INTO TABLE DATA(code_lines).

    LOOP AT code_lines ASSIGNING FIELD-SYMBOL(<line>).
      stream->write( <line> ).
      stream->write( |\n| ).

      IF error_line_drawn = abap_true.
        CONTINUE. " Line already drawn
      ENDIF.

      DATA(line_len) = strlen( <line> ) + 1. " Add the new line again
      offset = offset + line_len.

      IF offset < err_location.
        CONTINUE.
      ENDIF.

      " Write the error marker:
      DATA(adj_err_location) = err_location - ( offset - line_len ).

      DATA(error_ind) = |{ '^' ALIGN = RIGHT WIDTH = adj_err_location PAD = '-' }\n|.
      stream->write( error_ind ).
      error_line_drawn = abap_true.
    ENDLOOP.

    r_result = stream->get_result_string( ).
  ENDMETHOD.
ENDCLASS.

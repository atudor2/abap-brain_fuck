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



CLASS ZCX_BRAINFUCK_SYNTAX_ERROR IMPLEMENTATION.


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

    DATA lf TYPE c VALUE cl_abap_char_utilities=>cr_lf.

    r_result = |Syntax error:{ lf }{ me->message }{ lf }At character offset: { location }{ lf }{ me->source_code }|.

    " Create the error indicator line
    DATA(error_ind) = REDUCE #( INIT s = || FOR i = 1 UNTIL i > me->location NEXT s = s && '-' ) && '^'.

    r_result = r_result && lf && error_ind.
  ENDMETHOD.
ENDCLASS.

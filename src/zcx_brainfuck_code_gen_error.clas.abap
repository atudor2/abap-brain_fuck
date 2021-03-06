CLASS zcx_brainfuck_code_gen_error DEFINITION
  PUBLIC
  INHERITING FROM zcx_brainfuck_error
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_syntax_error,
        line    TYPE string,
        word    TYPE string,
        message TYPE string,
        offset  TYPE i,
      END OF t_syntax_error,
      tt_program_source TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DATA generated_source_code TYPE tt_program_source READ-ONLY.
    DATA syntax_error_details TYPE t_syntax_error READ-ONLY.

    METHODS constructor
      IMPORTING
        textid            LIKE if_t100_message=>t100key OPTIONAL
        previous          LIKE previous OPTIONAL
        it_program_source TYPE tt_program_source
        i_syntax_error    TYPE t_syntax_error.

    METHODS get_text REDEFINITION.

    METHODS get_longtext REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_brainfuck_code_gen_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    generated_source_code = it_program_source.
    syntax_error_details  = i_syntax_error.
  ENDMETHOD.


  METHOD get_longtext.
    DATA lines TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA formatted_code LIKE me->generated_source_code.

    " Dump full program and highlight the code line
    INSERT |Generated program code:| INTO TABLE lines.

    CALL FUNCTION 'PRETTY_PRINTER'
      EXPORTING
        inctoo = abap_false " Ignore includes
      TABLES
        ntext  = formatted_code
        otext  = me->generated_source_code
      EXCEPTIONS
        OTHERS = 5.

    IF sy-subrc <> 0.
      " Pretty print fail, use the original code
      formatted_code = me->generated_source_code.
    ENDIF.

    DATA(i) = 0.
    LOOP AT formatted_code ASSIGNING FIELD-SYMBOL(<code>).
      i = i + 1.

      DATA(line_prefix) = |{ i ALIGN = RIGHT WIDTH = 3 PAD = '0' }: |.
      DATA(line) = |{ line_prefix }{ <code> }|.

      " Syntax error line?
      IF i = syntax_error_details-line.
        " Print message and indicator:
        INSERT |>>>> { syntax_error_details-message }| INTO TABLE lines.
      ENDIF.

      INSERT line INTO TABLE lines.
    ENDLOOP.

    result = REDUCE #( INIT x = || FOR <wa> IN lines NEXT x = |{ x }{ <wa> }{ cl_abap_char_utilities=>cr_lf }| ).
  ENDMETHOD.


  METHOD get_text.
    result = |Syntax error in generated program: { syntax_error_details-message } (at line { syntax_error_details-line })|.
  ENDMETHOD.
ENDCLASS.

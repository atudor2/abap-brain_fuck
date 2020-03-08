"! <p class="shorttext synchronized" lang="en">Brainfuck Compiler</p>
CLASS zcl_brainfuck_compiler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_brainfuck_compiler.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_added_instruction,
        instruction  TYPE REF TO zif_brainfuck_instruction,
        insert_index TYPE i,
      END OF t_added_instruction.

    METHODS char_to_instruction
      IMPORTING
        i_char         TYPE string
      RETURNING
        VALUE(r_token) TYPE zif_brainfuck_instruction=>t_instruction_type.

    METHODS add_or_fold_instruction
      IMPORTING
        i_token         TYPE zif_brainfuck_instruction=>t_instruction_type
        i_location      TYPE i
        i_optimised     TYPE abap_bool
      CHANGING
        ct_instructions TYPE zif_brainfuck_instruction=>tt_instructions
      RETURNING
        VALUE(r_result) TYPE t_added_instruction
      RAISING
        zcx_brainfuck_error.
    METHODS can_token_fold
      IMPORTING
        i_token         TYPE zif_brainfuck_instruction=>t_instruction_type
      RETURNING
        VALUE(r_result) TYPE abap_bool.

ENDCLASS.

CLASS zcl_brainfuck_compiler IMPLEMENTATION.

  METHOD zif_brainfuck_compiler~compile.
    DATA:
      loop_stack TYPE STANDARD TABLE OF i,
      last_token LIKE LINE OF et_instructions.

    CLEAR: et_instructions[].

    DATA(code_len) = strlen( i_code ).

    IF code_len < 1.
      RAISE EXCEPTION TYPE zcx_brainfuck_syntax_error
        EXPORTING
          location    = 0
          source_code = i_code
          message     = 'Empty source code'.
    ENDIF.

    DATA(location) = 0.
    DO code_len TIMES.
      DATA(offset) = sy-index - 1. " 0-index
      location     = offset + 1.   " 1-index

      DATA(char) = i_code+offset(1).

      DATA(token) = char_to_instruction( char ).

      " There could be multiple optimisation levels, but for us its an ON/OFF switch
      DATA(is_optimised) = xsdbool( i_optimisation_level <> zif_brainfuck_compiler=>optimisation_levels-none ).

      " Allow debugger calls?
      IF token = zif_brainfuck_instruction=>instruction_type-debugger AND i_allow_debugger <> abap_true.
        CONTINUE.
      ENDIF.

      " If same as last token, then bump the REPEAT value...
      DATA(insertion) = add_or_fold_instruction( EXPORTING i_token     = token
                                                           i_location  = location
                                                           i_optimised = is_optimised
                                                 CHANGING  ct_instructions = et_instructions ).

      " Handle loops to set the open/close index
      CASE token.
        WHEN zif_brainfuck_instruction=>instruction_type-jmp_if_zero.
          " Push the 'stack'
          APPEND insertion-insert_index TO loop_stack.
        WHEN zif_brainfuck_instruction=>instruction_type-jmp_if_not_zero.
          TRY.
              " Pop the 'stack' for the opening of loop location
              DATA(open_loop) = loop_stack[ lines( loop_stack ) ].
              DELETE loop_stack INDEX lines( loop_stack ).

              " Set the opening loop instruction index as the end loop argument
              insertion-instruction->argument = open_loop.

              " Set the closing loop instruction index as the opening loop argument
              et_instructions[ open_loop ]->argument = insertion-insert_index.
            CATCH cx_sy_itab_line_not_found.
              " Syntax error -> closing loop without corresponding opening
              RAISE EXCEPTION TYPE zcx_brainfuck_syntax_error
                EXPORTING
                  location    = location
                  source_code = i_code
                  message     = |No opening '[' for loop|.
          ENDTRY.
      ENDCASE.
    ENDDO.

    " If loop stack is not empty, then a loop was not closed...
    IF loop_stack[] IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_brainfuck_syntax_error
        EXPORTING
          location    = location
          source_code = i_code
          message     = |No closing ']' for loop|.
    ENDIF.
  ENDMETHOD.

  METHOD char_to_instruction.
    CASE i_char.
      WHEN '['.
        r_token = zif_brainfuck_instruction=>instruction_type-jmp_if_zero.
      WHEN ']'.
        r_token = zif_brainfuck_instruction=>instruction_type-jmp_if_not_zero.
      WHEN '+'.
        r_token = zif_brainfuck_instruction=>instruction_type-plus.
      WHEN '-'.
        r_token = zif_brainfuck_instruction=>instruction_type-minus.
      WHEN '>'.
        r_token = zif_brainfuck_instruction=>instruction_type-right.
      WHEN '<'.
        r_token = zif_brainfuck_instruction=>instruction_type-left.
      WHEN '.'.
        r_token = zif_brainfuck_instruction=>instruction_type-put_char.
      WHEN ','.
        r_token = zif_brainfuck_instruction=>instruction_type-read_char.
      WHEN '#'.
        r_token = zif_brainfuck_instruction=>instruction_type-debugger.
      WHEN OTHERS.
        " Anything else is a comment
        r_token = zif_brainfuck_instruction=>instruction_type-comment.
    ENDCASE.
  ENDMETHOD.


  METHOD add_or_fold_instruction.
    DATA last_instruction TYPE REF TO zif_brainfuck_instruction.

    " If the last instruction is the same, just bump the REPEAT if optmisations are on
    " and the token can be folded
    DATA(count) = lines( ct_instructions ).
    IF count > 0.
      last_instruction = ct_instructions[ count ].
    ENDIF.

    IF i_optimised = abap_true AND can_token_fold( i_token ) AND last_instruction IS BOUND AND last_instruction->type = i_token.
      " Nothing to add, just bump the REPEAT
      last_instruction->repeated = last_instruction->repeated + 1.

      r_result = VALUE #( instruction  = last_instruction
                          insert_index = count ).
      RETURN.
    ENDIF.

    " New instruction:
    " @TODO decouple...
    DATA(instruction) = zcl_brainfuck_instruction=>zif_brainfuck_instruction~create(
                                            i_instruction = i_token
                                            i_location    = i_location
                                            i_repeated    = 1 ).
    APPEND instruction TO ct_instructions.

    r_result = VALUE #( instruction  = instruction
                        insert_index = sy-tabix ).
  ENDMETHOD.

  METHOD can_token_fold.
    r_result = SWITCH #( i_token
                  " The open and close loop instructions can never be optimised as this will
                  " change the semantics of the program
                  WHEN zif_brainfuck_instruction=>instruction_type-jmp_if_zero     THEN abap_false
                  WHEN zif_brainfuck_instruction=>instruction_type-jmp_if_not_zero THEN abap_false
                  ELSE abap_true ).
  ENDMETHOD.
ENDCLASS.

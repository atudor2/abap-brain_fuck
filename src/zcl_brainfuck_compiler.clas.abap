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
      CHANGING
        ct_instructions TYPE zif_brainfuck_instruction=>tt_instructions
      RETURNING
        VALUE(r_result) TYPE t_added_instruction
      RAISING
        zcx_brainfuck_error.

ENDCLASS.

CLASS zcl_brainfuck_compiler IMPLEMENTATION.

  METHOD zif_brainfuck_compiler~compile.
    DATA:
      loop_stack TYPE STANDARD TABLE OF i,
      last_token LIKE LINE OF et_instructions.

    CLEAR: et_instructions[].

    DATA(code_len) = strlen( i_code ).

    IF code_len < 1.
      " todo better error...
      RAISE EXCEPTION TYPE zcx_brainfuck_error.
    ENDIF.

    DO code_len TIMES.
      DATA(i) = sy-index - 1.
      DATA(char) = i_code+i(1).

      DATA(token) = char_to_instruction( char ).

      " If same as last token, then bump the REPEAT value...
      DATA(insertion) = add_or_fold_instruction( EXPORTING i_token = token
                                                           i_location = i
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
              ASSERT 1 = 0. " todo
          ENDTRY.
      ENDCASE.
    ENDDO.

    " If loop stack is not empty, then a loop was not closed...
    IF loop_stack[] IS NOT INITIAL.
      " Syntax error
      ASSERT 1 = 2.
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
      WHEN OTHERS.
        " Anything else is a comment
        r_token = zif_brainfuck_instruction=>instruction_type-comment.
    ENDCASE.
  ENDMETHOD.


  METHOD add_or_fold_instruction.
    DATA last_instruction TYPE REF TO zif_brainfuck_instruction.

    " If the last instruction is the same, just bump the REPEAT...
    DATA(count) = lines( ct_instructions ).
    IF count > 0.
      last_instruction = ct_instructions[ count ].
    ENDIF.

    IF last_instruction IS BOUND AND last_instruction->type = i_token.
      " Nothing to add, just bump the REPEAT
      last_instruction->repeated = last_instruction->repeated + 1.

      r_result = VALUE #( instruction  = last_instruction
                          insert_index = count ).
      RETURN.
    ENDIF.

    " New instruction:
    " ...todo decouple...
    DATA(instruction) = zcl_brainfuck_instruction=>zif_brainfuck_instruction~create(
                                            i_instruction = i_token
                                            i_location    = i_location
                                            i_repeated    = 1 ).
    APPEND instruction TO ct_instructions.

    r_result = VALUE #( instruction  = instruction
                        insert_index = sy-tabix ).
  ENDMETHOD.
ENDCLASS.

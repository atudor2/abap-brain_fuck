CLASS zcl_brainfuck_executor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_brainfuck_executor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS write_char
      IMPORTING
        i_value   TYPE i
        ir_output TYPE REF TO zif_brainfuck_output_stream.
    METHODS read_char
      IMPORTING
        ir_input        TYPE REF TO zif_brainfuck_input_stream
      RETURNING
        VALUE(r_result) TYPE i.
ENDCLASS.

CLASS zcl_brainfuck_executor IMPLEMENTATION.
  METHOD zif_brainfuck_executor~execute.
    DATA memory TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    " Setup the memory cells
    DO i_memory_cells TIMES.
      INSERT INITIAL LINE INTO TABLE memory.
    ENDDO.

    DATA(ip) = 1. " Instruction pointer
    DATA(dp) = 1. " Data pointer
    DATA(instructions_count) = lines( it_instructions ).

    WHILE ip <= instructions_count.
      ASSIGN it_instructions[ ip ] TO FIELD-SYMBOL(<instruction>).
      DATA(ins_type) = <instruction>->type.

      CASE ins_type.
        WHEN zif_brainfuck_instruction=>instruction_type-plus.
          memory[ dp ] = memory[ dp ] + <instruction>->repeated.
        WHEN zif_brainfuck_instruction=>instruction_type-minus.
          memory[ dp ] = memory[ dp ] - <instruction>->repeated.
        WHEN zif_brainfuck_instruction=>instruction_type-right.
          dp = dp + <instruction>->repeated.
          IF dp > i_memory_cells.
            " Overrun of DP -> runtime error
            " todo
            ASSERT 1 = 2.
          ENDIF.
        WHEN zif_brainfuck_instruction=>instruction_type-left.
          dp = dp - <instruction>->repeated.
          IF dp < 1.
            " Under run of DP -> runtime error
            " todo
            ASSERT 1 = 2.
          ENDIF.
        WHEN zif_brainfuck_instruction=>instruction_type-put_char.
          DO <instruction>->repeated TIMES.
            me->write_char( i_value   = memory[ dp ]
                            ir_output = ir_output ).
          ENDDO.
        WHEN zif_brainfuck_instruction=>instruction_type-read_char.
          DO <instruction>->repeated TIMES.
            memory[ dp ] = me->read_char( ir_input = ir_input ).
          ENDDO.
        WHEN zif_brainfuck_instruction=>instruction_type-jmp_if_zero.
          IF memory[ dp ] = 0.
            ip = <instruction>->argument.
            CONTINUE.
          ENDIF.
        WHEN zif_brainfuck_instruction=>instruction_type-jmp_if_not_zero.
          IF memory[ dp ] <> 0.
            ip = <instruction>->argument.
            CONTINUE.
          ENDIF.
      ENDCASE.

      ip = ip + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD write_char.
    DATA(char) = CONV zif_brainfuck_output_stream=>t_character( cl_abap_conv_in_ce=>uccpi( i_value ) ).
    ir_output->write_character( char ).
  ENDMETHOD.

  METHOD read_char.
    r_result = cl_abap_conv_out_ce=>uccpi( char = ir_input->read_character( ) ).
  ENDMETHOD.
ENDCLASS.

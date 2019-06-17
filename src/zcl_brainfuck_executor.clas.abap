CLASS zcl_brainfuck_executor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_brainfuck_executor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_exec_state,
        instruction_pointer TYPE i,
        data_pointer        TYPE i,
        memory_cells        TYPE i,
        instruction         TYPE REF TO zif_brainfuck_instruction,
        memory              TYPE zif_brainfuck_instruction=>tt_memory_cells,
      END OF t_exec_state.

    METHODS write_char
      IMPORTING
        i_value   TYPE zif_brainfuck_instruction=>t_memory_cell
        ir_output TYPE REF TO zif_brainfuck_output_stream.
    METHODS read_char
      IMPORTING
        ir_input        TYPE REF TO zif_brainfuck_input_stream
      RETURNING
        VALUE(r_result) TYPE zif_brainfuck_instruction=>t_memory_cell.

    METHODS dump_execution_state
      IMPORTING
        ir_exec_state TYPE ref to lcl_execution_state
        ir_output    TYPE REF TO zif_brainfuck_output_stream.
ENDCLASS.

CLASS zcl_brainfuck_executor IMPLEMENTATION.

  METHOD dump_execution_state.
    ir_output->flush( ).

    DATA(dbg_msg) = |IP = { ir_exec_state->instruction_pointer }, DP = { ir_exec_state->data_pointer }, Instruction = { CONV string( ir_exec_state->current_instruction_type ) }|.

    ir_output->write_string( i_string = dbg_msg ).
    ir_output->flush( ).
  ENDMETHOD.

  METHOD read_char.
    r_result = cl_abap_conv_out_ce=>uccpi( char = ir_input->read_character( ) ).
  ENDMETHOD.


  METHOD write_char.
    DATA(char) = CONV zif_brainfuck_output_stream=>t_character( cl_abap_conv_in_ce=>uccpi( CONV #( i_value ) ) ).
    ir_output->write_character( char ).
  ENDMETHOD.


  METHOD zif_brainfuck_executor~execute.
    DATA(exec_state) = NEW lcl_execution_state(
        it_instructions = it_instructions
        i_memory_cells  = i_memory_cells  ).

    WHILE exec_state->has_instructions( ).
      ASSIGN exec_state->current_instruction TO FIELD-SYMBOL(<instruction>).

      CASE exec_state->current_instruction_type.
        WHEN zif_brainfuck_instruction=>instruction_type-plus.
          exec_state->increment_value( <instruction>->repeated ).

        WHEN zif_brainfuck_instruction=>instruction_type-minus.
          exec_state->decrement_value( <instruction>->repeated ).

        WHEN zif_brainfuck_instruction=>instruction_type-right.
          exec_state->shift_data_pointer( i_shift = <instruction>->repeated ).

        WHEN zif_brainfuck_instruction=>instruction_type-left.
          exec_state->shift_data_pointer( i_shift = - <instruction>->repeated ).

        WHEN zif_brainfuck_instruction=>instruction_type-put_char.
          DO <instruction>->repeated TIMES.
            me->write_char( i_value   = exec_state->get_value( )
                            ir_output = ir_output ).
          ENDDO.

        WHEN zif_brainfuck_instruction=>instruction_type-read_char.
          DO <instruction>->repeated TIMES.
            exec_state->set_value( me->read_char( ir_input = ir_input ) ).
          ENDDO.
        WHEN zif_brainfuck_instruction=>instruction_type-jmp_if_zero.
          IF exec_state->get_value( ) = 0.
            exec_state->jump_instruction( <instruction>->argument ).
            CONTINUE.
          ENDIF.

        WHEN zif_brainfuck_instruction=>instruction_type-jmp_if_not_zero.
          IF exec_state->get_value( ) <> 0.
            exec_state->jump_instruction( <instruction>->argument ).
            CONTINUE.
          ENDIF.

        WHEN zif_brainfuck_instruction=>instruction_type-debugger.
          " Print state of executor
          me->dump_execution_state( ir_exec_state = exec_state
                                    ir_output     = ir_output ).
        WHEN zif_brainfuck_instruction=>instruction_type-comment.
          " NOP
      ENDCASE.

      exec_state->next_instruction( ).
    ENDWHILE.

    " Flush output stream
    ir_output->flush( ).
  ENDMETHOD.
ENDCLASS.

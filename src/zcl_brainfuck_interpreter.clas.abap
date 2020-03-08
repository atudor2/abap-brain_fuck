"! <p class="shorttext synchronized" lang="en">Brainfuck Executor - Interpreter</p>
CLASS zcl_brainfuck_interpreter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_brainfuck_executor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    "! <p class="shorttext synchronized" lang="en">Writes a memory cell to the output stream</p>
    "! @parameter i_value | <p class="shorttext synchronized" lang="en">Memory Cell </p>
    "! @parameter ir_output | <p class="shorttext synchronized" lang="en">Output Stream</p>
    METHODS write_char
      IMPORTING
        i_value   TYPE zif_brainfuck_instruction=>t_memory_cell
        ir_output TYPE REF TO zif_brainfuck_output_stream.

    "! <p class="shorttext synchronized" lang="en">Reads a input value (from user, file etc) into a memory cell</p>
    "! @parameter ir_input | <p class="shorttext synchronized" lang="en">Input Stream</p>
    "! @parameter r_result | <p class="shorttext synchronized" lang="en">Memory Cell value</p>
    METHODS read_char
      IMPORTING
        ir_input        TYPE REF TO zif_brainfuck_input_stream
      RETURNING
        VALUE(r_result) TYPE zif_brainfuck_instruction=>t_memory_cell.

    "! <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ir_exec_state | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ir_output | <p class="shorttext synchronized" lang="en"></p>
    METHODS dump_execution_state
      IMPORTING
        ir_exec_state TYPE REF TO lcl_execution_state
        ir_output     TYPE REF TO zif_brainfuck_output_stream.

    METHODS handle_instruction
      IMPORTING
        ir_state  TYPE REF TO lcl_execution_state
        ir_input  TYPE REF TO zif_brainfuck_input_stream
        ir_output TYPE REF TO zif_brainfuck_output_stream
      RAISING
        zcx_brainfuck_error.
    METHODS call_inspector_start
      IMPORTING
        ir_inspector TYPE REF TO zif_brainfuck_inspector
        ir_state     TYPE REF TO lcl_execution_state.
    METHODS call_inspector_end
      IMPORTING
        ir_inspector TYPE REF TO zif_brainfuck_inspector
        ir_state     TYPE REF TO lcl_execution_state.

    METHODS get_state_for_inspector
      IMPORTING
        ir_state        TYPE REF TO lcl_execution_state
      RETURNING
        VALUE(r_result) TYPE zif_brainfuck_inspector=>t_execution_state.

    METHODS write_string_to_output
      IMPORTING
        i_string  TYPE string
        ir_output TYPE REF TO zif_brainfuck_output_stream.
ENDCLASS.



CLASS ZCL_BRAINFUCK_INTERPRETER IMPLEMENTATION.


  METHOD call_inspector_end.
    CHECK ir_inspector IS BOUND.

    ir_inspector->end_of_instruction( i_state = get_state_for_inspector( ir_state ) ).
  ENDMETHOD.


  METHOD call_inspector_start.
    CHECK ir_inspector IS BOUND.

    ir_inspector->start_of_instruction( i_state = get_state_for_inspector( ir_state ) ).
  ENDMETHOD.


  METHOD dump_execution_state.
    ir_output->flush( ).

    DATA(dbg_msg) = |IP = { ir_exec_state->instruction_pointer }, DP = { ir_exec_state->data_pointer }, Instruction = { CONV string( ir_exec_state->current_instruction_type ) }|.

    write_string_to_output( i_string = dbg_msg ir_output = ir_output ).
  ENDMETHOD.


  METHOD get_state_for_inspector.
    r_result = VALUE #(
        data_pointer        = ir_state->data_pointer
        instruction         = ir_state->current_instruction
        instruction_pointer = ir_state->instruction_pointer
        memory_cells        = REF #( ir_state->memory_cells )
    ).
  ENDMETHOD.


  METHOD handle_instruction.
    DATA(exec_state) = ir_state.
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
          RETURN.
        ENDIF.

      WHEN zif_brainfuck_instruction=>instruction_type-jmp_if_not_zero.
        IF exec_state->get_value( ) <> 0.
          exec_state->jump_instruction( <instruction>->argument ).
          RETURN.
        ENDIF.

      WHEN zif_brainfuck_instruction=>instruction_type-debugger.
        " Print state of executor
        me->dump_execution_state( ir_exec_state = exec_state
                                  ir_output     = ir_output ).
      WHEN zif_brainfuck_instruction=>instruction_type-comment.
        " NOP
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_brainfuck_error.
    ENDCASE.

    exec_state->next_instruction( ).
  ENDMETHOD.


  METHOD read_char.
    r_result = ir_input->read_character( ).
  ENDMETHOD.


  METHOD write_char.
    ir_output->write_character( i_value ).
  ENDMETHOD.


  METHOD write_string_to_output.
    DO strlen( i_string ) TIMES.
      DATA(i) = sy-index.

      DATA(c) = i_string+i(1).
      ir_output->write_character( cl_abap_conv_out_ce=>uccpi( char = c ) ).
      ir_output->flush( ).
    ENDDO.
  ENDMETHOD.


  METHOD zif_brainfuck_executor~execute.
    DATA(exec_state) = NEW lcl_execution_state(
        it_instructions = it_instructions
        i_memory_cells  = i_memory_cells  ).

    " Execution Loop
    WHILE exec_state->has_instructions( ).

      call_inspector_start( ir_inspector = ir_inspector ir_state = exec_state ).

      me->handle_instruction(
        ir_state  = exec_state
        ir_input  = ir_input
        ir_output = ir_output
      ).

      call_inspector_end( ir_inspector = ir_inspector ir_state = exec_state ).
    ENDWHILE.

    " Flush output stream
    ir_output->flush( ).
  ENDMETHOD.
ENDCLASS.

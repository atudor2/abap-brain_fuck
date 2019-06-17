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

    METHODS init_memory_cells
      IMPORTING
        VALUE(ir_state) TYPE REF TO t_exec_state.

    METHODS dump_execution_state
      IMPORTING
        i_exec_state TYPE zcl_brainfuck_executor=>t_exec_state
        ir_output    TYPE REF TO zif_brainfuck_output_stream.
ENDCLASS.



CLASS ZCL_BRAINFUCK_EXECUTOR IMPLEMENTATION.


  METHOD dump_execution_state.
    ir_output->flush( ).

    DATA(dbg_msg) = |IP = { i_exec_state-instruction_pointer }, DP = { i_exec_state-data_pointer }, Instruction = { CONV string( i_exec_state-instruction->type ) }|.

    ir_output->write_string( i_string = dbg_msg ).
    ir_output->flush( ).
  ENDMETHOD.


  METHOD init_memory_cells.
    DO ir_state->memory_cells TIMES.
      INSERT INITIAL LINE INTO TABLE ir_state->memory.
    ENDDO.
  ENDMETHOD.


  METHOD read_char.
    r_result = cl_abap_conv_out_ce=>uccpi( char = ir_input->read_character( ) ).
  ENDMETHOD.


  METHOD write_char.
    DATA(char) = CONV zif_brainfuck_output_stream=>t_character( cl_abap_conv_in_ce=>uccpi( CONV #( i_value ) ) ).
    ir_output->write_character( char ).
  ENDMETHOD.


  METHOD zif_brainfuck_executor~execute.
    DATA exec_state TYPE t_exec_state.

    exec_state = VALUE #(
        data_pointer        = 1
        instruction_pointer = 1
        memory              = VALUE #( )
        instruction         = VALUE #( )
        memory_cells        = i_memory_cells
    ).

    " Setup the memory cells
    me->init_memory_cells( ir_state = REF #( exec_state ) ).

    DATA(instructions_count) = lines( it_instructions ).

    " Setup some <FS>s for ease of access
    ASSIGN exec_state-data_pointer        TO FIELD-SYMBOL(<dp>).
    ASSIGN exec_state-instruction_pointer TO FIELD-SYMBOL(<ip>).
    ASSIGN exec_state-memory              TO FIELD-SYMBOL(<memory>).
    ASSIGN exec_state-instruction         TO FIELD-SYMBOL(<instruction>).

    WHILE <ip> <= instructions_count.
      exec_state-instruction = it_instructions[ <ip> ].
      DATA(ins_type) = <instruction>->type.

      CASE ins_type.
        WHEN zif_brainfuck_instruction=>instruction_type-plus.
          <memory>[ <dp> ] = <memory>[ <dp> ] + <instruction>->repeated.
        WHEN zif_brainfuck_instruction=>instruction_type-minus.
          <memory>[ <dp> ] = <memory>[ <dp> ] - <instruction>->repeated.
        WHEN zif_brainfuck_instruction=>instruction_type-right.
          <dp> = <dp> + <instruction>->repeated.
          IF <dp> > i_memory_cells.
            " Overrun of DP -> wrap back to 1
            <dp> = 1.
          ENDIF.
        WHEN zif_brainfuck_instruction=>instruction_type-left.
          <dp> = <dp> - <instruction>->repeated.
          IF <dp> < 1.
            " Under run of DP -> wrap around
            <dp> = i_memory_cells.
          ENDIF.
        WHEN zif_brainfuck_instruction=>instruction_type-put_char.
          DO <instruction>->repeated TIMES.
            me->write_char( i_value   = <memory>[ <dp> ]
                            ir_output = ir_output ).
          ENDDO.
        WHEN zif_brainfuck_instruction=>instruction_type-read_char.
          DO <instruction>->repeated TIMES.
            <memory>[ <dp> ] = me->read_char( ir_input = ir_input ).
          ENDDO.
        WHEN zif_brainfuck_instruction=>instruction_type-jmp_if_zero.
          IF <memory>[ <dp> ] = 0.
            <ip> = <instruction>->argument.
            CONTINUE.
          ENDIF.
        WHEN zif_brainfuck_instruction=>instruction_type-jmp_if_not_zero.
          IF <memory>[ <dp> ] <> 0.
            <ip> = <instruction>->argument.
            CONTINUE.
          ENDIF.
        WHEN zif_brainfuck_instruction=>instruction_type-debugger.
          " Print state of executor
          me->dump_execution_state( i_exec_state = exec_state
                                    ir_output    = ir_output ).
        WHEN zif_brainfuck_instruction=>instruction_type-comment.
          " NOP
      ENDCASE.

      <ip> = <ip> + 1.
    ENDWHILE.

    " Flush output stream
    ir_output->flush( ).
  ENDMETHOD.
ENDCLASS.

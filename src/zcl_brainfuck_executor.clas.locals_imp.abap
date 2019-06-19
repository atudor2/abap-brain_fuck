*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_execution_state IMPLEMENTATION.
  METHOD constructor.
    ASSERT i_memory_cells > 0.

    " Init state
    CLEAR: me->current_instruction, me->current_instruction_type.

    me->total_memory_cells  = i_memory_cells.
    me->instructions        = it_instructions.
    me->data_pointer        = 1.
    me->total_instructions  = lines( it_instructions ).

    me->set_instruction_pointer( 1 ).

    DO me->total_memory_cells TIMES.
      INSERT INITIAL LINE INTO TABLE me->memory_cells.
    ENDDO.
  ENDMETHOD.

  METHOD has_instructions.
    r_result = xsdbool( me->instruction_pointer <= me->total_instructions ).
  ENDMETHOD.

  METHOD jump_instruction.
    me->set_instruction_pointer( i_ip = i_index ).
  ENDMETHOD.

  METHOD next_instruction.
    me->set_instruction_pointer( i_ip = ( me->instruction_pointer + 1 ) ).
  ENDMETHOD.

  METHOD shift_data_pointer.
    me->data_pointer = me->data_pointer + i_shift.

    " Wrap around?
    IF me->data_pointer < 1.
      me->data_pointer = me->total_memory_cells. " Under run of DP -> wrap around
    ELSEIF me->data_pointer > me->total_memory_cells.
      me->data_pointer = 1. " Overrun of DP -> wrap back to 1
    ENDIF.
  ENDMETHOD.

  METHOD get_value.
    r_result = me->memory_cells[ me->data_pointer ].
  ENDMETHOD.

  METHOD set_value.
    me->memory_cells[ me->data_pointer ] = i_value.
  ENDMETHOD.

  METHOD decrement_value.
    DATA(value) = me->safe_value_wrap(
                        i_value    = me->get_value( )
                        i_adjument = - i_shift ).

    me->set_value( value ).
  ENDMETHOD.

  METHOD increment_value.
    DATA(value) = me->safe_value_wrap(
                        i_value    = me->get_value( )
                        i_adjument = i_shift ).

    me->set_value( value ).
  ENDMETHOD.

  METHOD safe_value_wrap.
    " Must do integer wrap around - by default ABAP will raise exception on overflow, so handle the situation manually
    DATA int_result TYPE i.

    int_result = i_value + i_adjument.
    IF int_result < 0.
      r_result = abs( int_result + 255 ).
    ELSEIF int_result > 255.
      r_result = abs( 255 - int_result ).
    ELSE.
      r_result = int_result.
    ENDIF.
  ENDMETHOD.

  METHOD set_instruction_pointer.
    ASSERT i_ip > 0.

    me->instruction_pointer = i_ip.

    " Any more?
    IF me->has_instructions( ).
      me->current_instruction = me->instructions[ me->instruction_pointer ].
      me->current_instruction_type = me->current_instruction->type.
    ELSE.
      CLEAR:me->current_instruction, me->current_instruction_type.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

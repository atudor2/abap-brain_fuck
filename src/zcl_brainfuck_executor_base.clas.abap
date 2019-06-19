"! <p class="shorttext synchronized" lang="en">Base Brainfuck Executor Class</p>
CLASS zcl_brainfuck_executor_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS adj_cell_value_with_wrap
      IMPORTING
        i_value         TYPE zif_brainfuck_instruction=>t_memory_cell
        i_adjustment    TYPE i
      RETURNING
        VALUE(r_result) TYPE zif_brainfuck_instruction=>t_memory_cell.

    METHODS move_data_pointer
      IMPORTING
        i_dp            TYPE i
        i_shift         TYPE i
        i_max_cells     TYPE i
      RETURNING
        VALUE(r_result) TYPE i.

    METHODS initialise_memory_cells
      IMPORTING
        i_max_cells     TYPE i
      CHANGING
        ct_memory_cells TYPE zif_brainfuck_instruction=>tt_memory_cells.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_cell_buffer,
        size            TYPE i,
        buffer_template TYPE zif_brainfuck_instruction=>tt_memory_cells,
      END OF t_cell_buffer.

    CLASS-DATA memory_cell_buffer TYPE SORTED TABLE OF t_cell_buffer WITH UNIQUE KEY size.
ENDCLASS.

CLASS zcl_brainfuck_executor_base IMPLEMENTATION.
  METHOD adj_cell_value_with_wrap.
    " Must do integer wrap around - by default ABAP will raise exception on overflow, so handle the situation manually
    DATA int_result TYPE i.

    int_result = i_value + i_adjustment.
    IF int_result < 0.
      r_result = abs( int_result + 255 ).
    ELSEIF int_result > 255.
      r_result = abs( 255 - int_result ).
    ELSE.
      r_result = int_result.
    ENDIF.
  ENDMETHOD.

  METHOD move_data_pointer.
    r_result = i_dp + i_shift.

    " Wrap around?
    IF r_result < 1.
      r_result = i_max_cells. " Under run of DP -> wrap around
    ELSEIF r_result > i_max_cells.
      r_result = 1. " Overrun of DP -> wrap back to 1
    ENDIF.
  ENDMETHOD.

  METHOD initialise_memory_cells.
    FIELD-SYMBOLS <buffer> LIKE LINE OF me->memory_cell_buffer.

    CLEAR ct_memory_cells[].

    READ TABLE me->memory_cell_buffer WITH TABLE KEY size = i_max_cells ASSIGNING <buffer>.
    IF sy-subrc <> 0.
      INSERT VALUE #( size = i_max_cells ) INTO TABLE me->memory_cell_buffer ASSIGNING <buffer>.
      DO i_max_cells TIMES.
        INSERT INITIAL LINE INTO TABLE <buffer>-buffer_template.
      ENDDO.
    ENDIF.

    ct_memory_cells[] = <buffer>-buffer_template[].
  ENDMETHOD.
ENDCLASS.

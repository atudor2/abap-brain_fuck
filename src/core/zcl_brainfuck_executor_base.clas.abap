"! <p class="shorttext synchronized" lang="en">Base Brainfuck Executor Class</p>
CLASS zcl_brainfuck_executor_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_large_cell_values TYPE abap_bool DEFAULT abap_false.

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
      END OF t_cell_buffer,

      BEGIN OF t_cell_value_range,
        min   TYPE zif_brainfuck_instruction=>t_memory_cell,
        max   TYPE zif_brainfuck_instruction=>t_memory_cell,
        range TYPE i,
      END OF t_cell_value_range.

    DATA:
      cell_range TYPE t_cell_value_range.

    CLASS-DATA memory_cell_buffer TYPE SORTED TABLE OF t_cell_buffer WITH UNIQUE KEY size.
ENDCLASS.



CLASS ZCL_BRAINFUCK_EXECUTOR_BASE IMPLEMENTATION.


  METHOD adj_cell_value_with_wrap.
    DATA int_result TYPE i.

    int_result = i_value + i_adjustment.
    DATA(min)   = cell_range-min.
    DATA(max)   = cell_range-max.
    DATA(range) = cell_range-range.

    " Handle the wrap around behaviour for int depending on the cell size
    " with thanks to "CB Bailey" -> https://stackoverflow.com/a/707426

    " Early exit
    IF int_result <= max AND int_result >= min.
      r_result = int_result.
      RETURN.
    ENDIF.

    IF int_result < min.
      int_result = int_result + range * ( ( min - int_result ) / range + 1 ).
    ENDIF.

    r_result = min + ( int_result - min ) MOD range.
  ENDMETHOD.


  METHOD constructor.
    cell_range = VALUE #(
        min   = -128
        max   = 127
    ).
    IF i_large_cell_values = abap_true.
      cell_range-min = cl_abap_math=>min_int4.
      cell_range-max = cl_abap_math=>max_int4.
    ENDIF.

    cell_range-range = cell_range-max - cell_range-min + 1.
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


  METHOD move_data_pointer.
    r_result = i_dp + i_shift.

    " Wrap around?
    IF r_result < 1.
      r_result = i_max_cells. " Under run of DP -> wrap around
    ELSEIF r_result > i_max_cells.
      r_result = 1. " Overrun of DP -> wrap back to 1
    ENDIF.
  ENDMETHOD.
ENDCLASS.

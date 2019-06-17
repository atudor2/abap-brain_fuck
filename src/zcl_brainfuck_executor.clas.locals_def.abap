*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_execution_state DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_instructions TYPE zif_brainfuck_executor=>tt_instructions
        i_memory_cells  TYPE i.

    METHODS has_instructions
      RETURNING VALUE(r_result) TYPE abap_bool.

    METHODS next_instruction.

    METHODS jump_instruction
      IMPORTING i_index TYPE i.

    METHODS shift_data_pointer
      IMPORTING
        i_shift TYPE i.

    METHODS get_value
      RETURNING VALUE(r_result) TYPE zif_brainfuck_instruction=>t_memory_cell.

    METHODS increment_value
      IMPORTING
        i_shift TYPE i.

    METHODS decrement_value
      IMPORTING
        i_shift TYPE i.

    METHODS set_value
      IMPORTING
        i_value TYPE zif_brainfuck_instruction=>t_memory_cell.

    DATA:
      instruction_pointer      TYPE i READ-ONLY,
      instructions             TYPE zif_brainfuck_executor=>tt_instructions READ-ONLY,
      data_pointer             TYPE i READ-ONLY,
      total_memory_cells       TYPE i READ-ONLY,
      current_instruction      TYPE REF TO zif_brainfuck_instruction READ-ONLY,
      current_instruction_type TYPE zif_brainfuck_instruction=>t_instruction_type READ-ONLY,
      memory_cells             TYPE zif_brainfuck_instruction=>tt_memory_cells READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      total_instructions TYPE i.

    METHODS safe_value_wrap
      IMPORTING
        i_value         TYPE zif_brainfuck_instruction=>t_memory_cell
        i_adjument      TYPE i
      RETURNING
        VALUE(r_result) TYPE zif_brainfuck_instruction=>t_memory_cell.
    METHODS set_instruction_pointer
      IMPORTING
        i_ip TYPE i.
ENDCLASS.

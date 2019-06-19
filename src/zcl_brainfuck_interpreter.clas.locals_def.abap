*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

"! Execution state for Brainfuck engine
CLASS lcl_execution_state DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    "! @parameter it_instructions | Instructions for program
    "! @parameter i_memory_cells | Max number of memory cells
    METHODS constructor
      IMPORTING
        it_instructions TYPE zif_brainfuck_executor=>tt_instructions
        i_memory_cells  TYPE i.

    "! Are there any further instructions to process?
    "! @parameter r_result | ABAP_TRUE if there are further instructions
    METHODS has_instructions
      RETURNING VALUE(r_result) TYPE abap_bool.

    "! Increments the instruction pointer
    METHODS next_instruction.

    "! Performs a "jump" to an instruction
    "! @parameter i_index | Instruction index
    METHODS jump_instruction
      IMPORTING i_index TYPE i.

    "! Shifts the data cell pointer. Wraps around if GT max number of memory cells
    "! @parameter i_shift | Shift right (+) or left (-)
    METHODS shift_data_pointer
      IMPORTING
        i_shift TYPE i.

    "! Gets the current memory cell value
    "! @parameter r_result | Cell Value
    METHODS get_value
      RETURNING VALUE(r_result) TYPE zif_brainfuck_instruction=>t_memory_cell.

    "! Increments the current memory cell value
    "! @parameter i_shift | Increment value
    METHODS increment_value
      IMPORTING
        i_shift TYPE i.

    "! Decrements the current memory cell value
    "! @parameter i_shift | Decrement value
    METHODS decrement_value
      IMPORTING
        i_shift TYPE i.

    "! Sets the current memory cell
    "! @parameter i_value | Value
    METHODS set_value
      IMPORTING
        i_value TYPE zif_brainfuck_instruction=>t_memory_cell.

    DATA:
      "! Current Instruction Pointer
      instruction_pointer      TYPE i READ-ONLY,
      "! All instructions
      instructions             TYPE zif_brainfuck_executor=>tt_instructions READ-ONLY,
      "! Data Cell Pointer
      data_pointer             TYPE i READ-ONLY,
      "! Maximum number of memory cells
      total_memory_cells       TYPE i READ-ONLY,
      "! Current instruction
      current_instruction      TYPE REF TO zif_brainfuck_instruction READ-ONLY,
      "! Current instruction type
      current_instruction_type TYPE zif_brainfuck_instruction=>t_instruction_type READ-ONLY,
      "! Memory Cells
      memory_cells             TYPE zif_brainfuck_instruction=>tt_memory_cells READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      "! Total number of instructions
      total_instructions TYPE i.

    METHODS safe_value_wrap
      IMPORTING
        i_value         TYPE zif_brainfuck_instruction=>t_memory_cell
        i_adjument      TYPE i
      RETURNING
        VALUE(r_result) TYPE zif_brainfuck_instruction=>t_memory_cell.

    "! Sets the instruction pointer
    "! @parameter i_ip | Instruction count
    METHODS set_instruction_pointer
      IMPORTING
        i_ip TYPE i.
ENDCLASS.

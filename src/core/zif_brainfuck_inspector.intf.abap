"! <p class="shorttext synchronized" lang="en">Brainfuck Runtime Inspector</p>
INTERFACE zif_brainfuck_inspector
  PUBLIC .

  TYPES:
    BEGIN OF t_execution_state,
      "! Current Instruction Pointer
      instruction_pointer TYPE i,
      "! Data Cell Pointer
      data_pointer        TYPE i,
      "! Instruction
      instruction         TYPE REF TO zif_brainfuck_instruction,
      "! Memory Cells
      memory_cells        TYPE REF TO zif_brainfuck_instruction=>tt_memory_cells,
    END OF t_execution_state.

  METHODS start_of_instruction
    IMPORTING
      i_state TYPE t_execution_state.

  METHODS end_of_instruction
    IMPORTING
      i_state TYPE t_execution_state.
ENDINTERFACE.

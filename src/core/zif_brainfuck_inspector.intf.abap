"! <p class="shorttext synchronized" lang="en">Brainfuck Runtime Inspector</p>
INTERFACE zif_brainfuck_inspector
  PUBLIC .

  TYPES:
    "! Execution state of application
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

  "! <p class="shorttext synchronized" lang="en">Start of instruction callback</p>
  "! @parameter i_state | <p class="shorttext synchronized" lang="en">Execution state</p>
  METHODS start_of_instruction
    IMPORTING
      i_state TYPE t_execution_state.

  "! <p class="shorttext synchronized" lang="en">End of instruction callback</p>
  "! @parameter i_state | <p class="shorttext synchronized" lang="en">Execution state</p>
  METHODS end_of_instruction
    IMPORTING
      i_state TYPE t_execution_state.

  "! <p class="shorttext synchronized" lang="en">Debug instruction callback</p>
  "! Triggered when instruction of type DEBUG is hit
  "! @parameter i_state | <p class="shorttext synchronized" lang="en">Execution state</p>
  METHODS on_debug_instruction DEFAULT IGNORE
    IMPORTING
      i_state TYPE t_execution_state.
ENDINTERFACE.

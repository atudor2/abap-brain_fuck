"! <p class="shorttext synchronized" lang="en">Brainfuck Executor</p>
INTERFACE zif_brainfuck_executor
  PUBLIC .

  "! Type Alias for set of Brainfuck instructions
  TYPES tt_instructions TYPE zif_brainfuck_instruction=>tt_instructions.

  "! Default Memory Cell Table Size
  CONSTANTS c_default_memory_cells TYPE i VALUE 30000.

  "! <p class="shorttext synchronized" lang="en">Execute the given Brainfuck program</p>
  "! @parameter it_instructions | <p class="shorttext synchronized" lang="en">Set of Brainfuck instructions</p>
  "! @parameter ir_input | <p class="shorttext synchronized" lang="en">Input Stream</p>
  "! @parameter ir_output | <p class="shorttext synchronized" lang="en">Output Stream</p>
  "! @parameter i_memory_cells | <p class="shorttext synchronized" lang="en">Number of Memory Cells</p>
  "! @raising zcx_brainfuck_error | <p class="shorttext synchronized" lang="en"></p>
  METHODS execute
    IMPORTING
      it_instructions TYPE tt_instructions
      ir_input        TYPE REF TO zif_brainfuck_input_stream
      ir_output       TYPE REF TO zif_brainfuck_output_stream
      i_memory_cells  TYPE i DEFAULT c_default_memory_cells
      ir_inspector    TYPE REF TO zif_brainfuck_inspector OPTIONAL
    RAISING
      zcx_brainfuck_error.
ENDINTERFACE.

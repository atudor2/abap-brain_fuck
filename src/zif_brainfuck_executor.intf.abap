"! <p class="shorttext synchronized" lang="en">Brainfuck Executor</p>
interface ZIF_BRAINFUCK_EXECUTOR
  public .

  TYPES tt_instructions TYPE zif_brainfuck_instruction=>tt_instructions.

  CONSTANTS c_default_memory_cells TYPE i VALUE 30000.

  METHODS execute
    IMPORTING
      it_instructions TYPE tt_instructions
      ir_input        TYPE REF TO zif_brainfuck_input_stream
      ir_output       TYPE REF TO zif_brainfuck_output_stream
      i_memory_cells  TYPE i DEFAULT c_default_memory_cells
    RAISING
      zcx_brainfuck_error.
endinterface.

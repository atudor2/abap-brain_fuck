"! <p class="shorttext synchronized" lang="en">Brainfuck Input Stream</p>
INTERFACE zif_brainfuck_input_stream
  PUBLIC .

  TYPES t_character TYPE c LENGTH 1.

  METHODS read_character
    RETURNING VALUE(r_result) TYPE t_character.
ENDINTERFACE.

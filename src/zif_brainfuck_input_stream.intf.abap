"! <p class="shorttext synchronized" lang="en">Brainfuck Input Stream</p>
INTERFACE zif_brainfuck_input_stream
  PUBLIC .

  "! Single length character
  TYPES t_character TYPE c LENGTH 1.

  "! <p class="shorttext synchronized" lang="en">Reads a character from the Input Stream</p>
  "! @parameter r_result | <p class="shorttext synchronized" lang="en"></p>
  METHODS read_character
    RETURNING VALUE(r_result) TYPE t_character.
ENDINTERFACE.

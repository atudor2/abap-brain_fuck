"! <p class="shorttext synchronized" lang="en">Brainfuck Output Stream</p>
INTERFACE zif_brainfuck_output_stream
  PUBLIC .

  TYPES t_character TYPE c LENGTH 1.

  METHODS write_character
    IMPORTING i_character TYPE t_character.
ENDINTERFACE.

"! <p class="shorttext synchronized" lang="en">Brainfuck Input Stream</p>
INTERFACE zif_brainfuck_input_stream
  PUBLIC .

  "! Character byte
  TYPES t_character_byte TYPE int1.

  "! <p class="shorttext synchronized" lang="en">Reads a character from the Input Stream</p>
  "! @parameter r_result | <p class="shorttext synchronized" lang="en">Character byte or 0 for EOF</p>
  METHODS read_character
    RETURNING VALUE(r_result) TYPE t_character_byte.
ENDINTERFACE.

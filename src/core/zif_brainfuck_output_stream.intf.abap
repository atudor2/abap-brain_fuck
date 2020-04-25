"! <p class="shorttext synchronized" lang="en">Brainfuck Output Stream</p>
INTERFACE zif_brainfuck_output_stream
  PUBLIC .

  "! Character byte
  TYPES t_character_byte TYPE zif_brainfuck_instruction=>t_memory_cell..

  "! <p class="shorttext synchronized" lang="en">Writes a single character to the output stream</p>
  "! @parameter i_character | <p class="shorttext synchronized" lang="en">Character byte to write</p>
  METHODS write_character
    IMPORTING i_character TYPE t_character_byte.

  "! <p class="shorttext synchronized" lang="en">Flush the output buffer to the underlying stream</p>
  METHODS flush.
ENDINTERFACE.

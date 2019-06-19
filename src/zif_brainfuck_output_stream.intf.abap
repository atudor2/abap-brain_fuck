"! <p class="shorttext synchronized" lang="en">Brainfuck Output Stream</p>
INTERFACE zif_brainfuck_output_stream
  PUBLIC .

  "! Single length character
  TYPES t_character TYPE c LENGTH 1.

  "! <p class="shorttext synchronized" lang="en">Writes a single character to the output stream</p>
  "! @parameter i_character | <p class="shorttext synchronized" lang="en">Character to write</p>
  METHODS write_character
    IMPORTING i_character TYPE t_character.

  "! <p class="shorttext synchronized" lang="en">Writes a string to the output stream</p>
  "! @parameter i_string | <p class="shorttext synchronized" lang="en">String to write</p>
  METHODS write_string
    IMPORTING i_string TYPE string.

  "! <p class="shorttext synchronized" lang="en">Flush the output buffer to the stream</p>
  METHODS flush.
ENDINTERFACE.

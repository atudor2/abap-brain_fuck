"! <p class="shorttext synchronized" lang="en">Brainfuck Compiler</p>
INTERFACE zif_brainfuck_compiler
  PUBLIC .

  "! Type alias for set of Brainfuck Instructions
  TYPES tt_instructions TYPE zif_brainfuck_instruction=>tt_instructions.

  TYPES:
    "! Optimisation level
    BEGIN OF ENUM enum_optimisation STRUCTURE optimisation_levels,
      none,
      full,
    END OF ENUM enum_optimisation STRUCTURE optimisation_levels.

  "! <p class="shorttext synchronized" lang="en">Compile Brainfuck source code into a set of instructions</p>
  "! @parameter i_code | <p class="shorttext synchronized" lang="en">Brainfuck source code</p>
  "! @parameter i_allow_debugger | <p class="shorttext synchronized" lang="en">Allow debugger instructions (ABAP_TRUE)?</p>
  "! @parameter i_optimisation_level | <p class="shorttext synchronized" lang="en">Optimisation level to apply during compilation</p>
  "! @parameter et_instructions | <p class="shorttext synchronized" lang="en">Table of result instructions</p>
  "! @raising zcx_brainfuck_error | <p class="shorttext synchronized" lang="en"></p>
  METHODS compile
    IMPORTING
      i_code               TYPE string
      i_allow_debugger     TYPE abap_bool DEFAULT abap_true
      i_optimisation_level TYPE enum_optimisation DEFAULT optimisation_levels-full
    EXPORTING
      et_instructions      TYPE tt_instructions
    RAISING
      zcx_brainfuck_error.
ENDINTERFACE.

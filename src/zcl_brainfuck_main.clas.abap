"! <p class="shorttext synchronized" lang="en">Brainfuck Main Class</p>
CLASS zcl_brainfuck_main DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_brainfuck_main IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA(compiler) = CAST zif_brainfuck_compiler( NEW zcl_brainfuck_compiler( ) ).
    data(output) = new lcl_out( ).
    output->out = out.
    TRY.
        compiler->compile(
          EXPORTING
            i_code              = '++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.'
          IMPORTING
            et_instructions     = DATA(instructions) ).

        DATA(exe) = NEW zcl_brainfuck_executor( ).

        exe->zif_brainfuck_executor~execute(
          EXPORTING
            it_instructions     = instructions
            ir_input            = VALUE #( )
            ir_output           = output ).
      CATCH zcx_brainfuck_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
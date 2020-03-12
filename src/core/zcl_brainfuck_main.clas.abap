"! <p class="shorttext synchronized" lang="en">Brainfuck Main Class</p>
CLASS zcl_brainfuck_main DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS execute_tests
      IMPORTING
        ir_executor          TYPE REF TO zif_brainfuck_executor
        i_iterations         TYPE i
        it_instructions      TYPE zif_brainfuck_instruction=>tt_instructions
        ir_input             TYPE REF TO zif_brainfuck_input_stream
        ir_output            TYPE REF TO zif_brainfuck_output_stream
        ir_background_input  TYPE REF TO zif_brainfuck_input_stream
        ir_background_output TYPE REF TO zif_brainfuck_output_stream
      RETURNING
        VALUE(r_result)      TYPE i.
ENDCLASS.

CLASS zcl_brainfuck_main IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA:
      compiler TYPE REF TO zif_brainfuck_compiler,
      runtime  TYPE i.

    compiler = NEW zcl_brainfuck_compiler( ).

    DATA(output) = NEW lcl_out( ).
    output->out = out.
    DATA(null_output) = NEW lcl_null_out( ).

    TRY.
        GET RUN TIME FIELD DATA(compile_time).

        compiler->compile(
          EXPORTING
            "i_code              = '*++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.'
            i_code              = '>++[<+++++++++++++>-]<[[>+>+<<-]>[<+>-]++++++++[>++++++++<-]>.[-]<<>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[-]<-]<-]<-]<-]<-]<-]<-]++++++++++.'
          IMPORTING
            et_instructions     = DATA(instructions) ).

        GET RUN TIME FIELD compile_time.
        out->write_text( text = |Compile Time { compile_time } microseconds| ).

        " Test interpreter and then code gen
        DATA(iterations) = 2.

        DATA(int_exe) = NEW zcl_brainfuck_interpreter( ).
        runtime = me->execute_tests(
                    EXPORTING
                        ir_executor          = int_exe
                        i_iterations         = iterations
                        it_instructions      = instructions
                        ir_input             = VALUE #( )
                        ir_output            = output
                        ir_background_input  = VALUE #( )
                        ir_background_output = null_output ).

        out->write_text( text = |Interpret: Runtime { runtime } microseconds over { iterations } iterations ({ runtime / iterations } microseconds)| ).

        DATA(gen_exe) = zcl_brainfuck_code_gen=>generate_program( EXPORTING it_instructions = instructions ).
        runtime = me->execute_tests(
                    EXPORTING
                        ir_executor          = gen_exe
                        i_iterations         = iterations
                        it_instructions      = instructions
                        ir_input             = VALUE #( )
                        ir_output            = output
                        ir_background_input  = VALUE #( )
                        ir_background_output = null_output ).

        out->write_text( text = |Code Gen: Runtime { runtime } microseconds over { iterations } iterations ({ runtime / iterations } microseconds)| ).
      CATCH zcx_brainfuck_syntax_error INTO DATA(ex).
        out->write_text( text = ex->get_error_message( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD execute_tests.
    " Execute once on normal output:
    ir_executor->execute(
      EXPORTING
        it_instructions     = it_instructions
        ir_input            = ir_input
        ir_output           = ir_output ).

    GET RUN TIME FIELD DATA(runtime).

    DO i_iterations TIMES.
      ir_executor->execute(
        EXPORTING
          it_instructions     = it_instructions
          ir_input            = ir_background_input
          ir_output           = ir_background_output ).
    ENDDO.

    GET RUN TIME FIELD runtime.

    r_result = runtime.
  ENDMETHOD.
ENDCLASS.

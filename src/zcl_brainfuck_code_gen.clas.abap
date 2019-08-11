"! <p class="shorttext synchronized" lang="en">Brainfuck Executor - Code Generator</p>
CLASS zcl_brainfuck_code_gen DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPES t_prog_src TYPE c LENGTH 255.
    TYPES tt_prog_src TYPE STANDARD TABLE OF t_prog_src WITH EMPTY KEY.

    CLASS-METHODS generate_program
      IMPORTING
        it_instructions TYPE zif_brainfuck_executor=>tt_instructions
        i_memory_cells  TYPE i DEFAULT zif_brainfuck_executor=>c_default_memory_cells
      EXPORTING
        et_source_code  TYPE tt_prog_src
      RETURNING
        VALUE(r_result) TYPE REF TO zif_brainfuck_executor
      RAISING
        zcx_brainfuck_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS get_dyn_program_header
      RETURNING
        VALUE(r_result) TYPE tt_prog_src.

    CLASS-METHODS get_dyn_program_footer
      RETURNING
        VALUE(r_result) TYPE tt_prog_src.
ENDCLASS.

CLASS zcl_brainfuck_code_gen IMPLEMENTATION.
  METHOD get_dyn_program_header.
    r_result = VALUE #(
        ( 'program.                                                      ' )
        ( 'class main definition                                         ' )
        ( '  inheriting from zcl_brainfuck_executor_base                 ' )
        ( '  create public.                                              ' )
        ( '                                                              ' )
        ( '  public section.                                             ' )
        ( '    interfaces zif_brainfuck_executor.                        ' )
        ( 'endclass.                                                     ' )
        ( '                                                              ' )
        ( 'class main implementation.'                                     )
        ( ' method zif_brainfuck_executor~execute.                       ' )
        ( '  data dp type i.                                             ' )
        ( '  data memory type zif_brainfuck_instruction=>tt_memory_cells.' )
        ( '  data cell   type zif_brainfuck_instruction=>t_memory_cell.  ' )
        ( '                                                              ' )
        ( '      me->initialise_memory_cells(                            ' )
        ( '           EXPORTING                                          ' )
        ( '             i_max_cells     =  i_memory_cells                ' )
        ( '           CHANGING                                           ' )
        ( '             ct_memory_cells = memory ).                      ' )
        ( '     dp = 1.                                                  ' )
        ( '     cell = memory[ dp ].                                     ' )
        ( '                                                              ' )

    ).
  ENDMETHOD.

  METHOD get_dyn_program_footer.
    r_result = VALUE #(
        ( ' ir_output->flush( ).' )
        ( ' endmethod.          ' )
        ( 'endclass.            ' )
    ).
  ENDMETHOD.

  METHOD generate_program.
    DATA:
          prog_src TYPE tt_prog_src.

    CLEAR: et_source_code.

    " Program header:
    prog_src = get_dyn_program_header( ).

    LOOP AT it_instructions ASSIGNING FIELD-SYMBOL(<instruction>).
      DATA(type) = <instruction>->type.
      CASE type.
        WHEN zif_brainfuck_instruction=>instruction_type-plus.
          APPEND |cell = me->adj_cell_value_with_wrap( i_value = cell i_adjustment = { <instruction>->repeated } ). | TO prog_src.
          APPEND |memory[ dp ] = cell.| TO prog_src.

        WHEN zif_brainfuck_instruction=>instruction_type-minus.
          APPEND |cell = me->adj_cell_value_with_wrap( i_value = cell i_adjustment = { -1 * <instruction>->repeated } ). | TO prog_src.
          APPEND |memory[ dp ] = cell.| TO prog_src.

        WHEN zif_brainfuck_instruction=>instruction_type-right.
          APPEND |dp = move_data_pointer( i_dp = dp i_shift = { <instruction>->repeated } i_max_cells = i_memory_cells ). | TO prog_src.
          APPEND |cell = memory[ dp ].| TO prog_src.

        WHEN zif_brainfuck_instruction=>instruction_type-left.
          APPEND |dp = move_data_pointer( i_dp = dp i_shift = { -1 * <instruction>->repeated } i_max_cells = i_memory_cells ). | TO prog_src.
          APPEND |cell = memory[ dp ].| TO prog_src.

        WHEN zif_brainfuck_instruction=>instruction_type-put_char.
          DO <instruction>->repeated TIMES.
            APPEND |ir_output->write_character( |                     TO prog_src.
            APPEND |CONV zif_brainfuck_output_stream=>t_character( |  TO prog_src.
            APPEND |cl_abap_conv_in_ce=>uccpi( CONV #( cell ) ) ) ).| TO prog_src.
          ENDDO.

        WHEN zif_brainfuck_instruction=>instruction_type-read_char.
          DO <instruction>->repeated TIMES.
            APPEND |cell = cl_abap_conv_out_ce=>uccpi( char = ir_input->read_character( ) ).| TO prog_src.
          ENDDO.

        WHEN zif_brainfuck_instruction=>instruction_type-jmp_if_zero.
          APPEND |WHILE cell <> 0.| TO prog_src.

        WHEN zif_brainfuck_instruction=>instruction_type-jmp_if_not_zero.
          APPEND |ENDWHILE.| TO prog_src.

        WHEN zif_brainfuck_instruction=>instruction_type-debugger.
          " Just a BREAK-POINT for the # command
          APPEND |BREAK-POINT.| TO prog_src.

        WHEN zif_brainfuck_instruction=>instruction_type-comment.
          " NOP
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_brainfuck_error.
      ENDCASE.
    ENDLOOP.

    " Program footer:
    APPEND LINES OF get_dyn_program_footer( ) TO prog_src.

    GENERATE SUBROUTINE POOL prog_src NAME DATA(dyn_prog).
    DATA(dyn_class) = `\PROGRAM=` && dyn_prog && `\CLASS=MAIN`.

    et_source_code[] = prog_src[].
    CREATE OBJECT r_result TYPE (dyn_class).
  ENDMETHOD.
ENDCLASS.

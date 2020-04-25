"! <p class="shorttext synchronized" lang="en">Brainfuck Executor - Code Generation</p>
CLASS zcl_brainfuck_code_gen DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_brainfuck_executor.

    "! Line of generated ABAP code
    TYPES t_prog_src TYPE c LENGTH 255.
    "! Table of generated ABAP code
    TYPES tt_prog_src TYPE STANDARD TABLE OF t_prog_src WITH EMPTY KEY.

    "! <p class="shorttext synchronized" lang="en">Generate ABAP program for the set of Brainfuck instructions</p>
    "! @parameter it_instructions | <p class="shorttext synchronized" lang="en">Brainfuck instructions</p>
    "! @parameter i_memory_cells | <p class="shorttext synchronized" lang="en">Size of memory cells buffer</p>
    "! @parameter et_source_code | <p class="shorttext synchronized" lang="en">Table of generated ABAP code</p>
    "! @parameter r_result | <p class="shorttext synchronized" lang="en">Instance of generated class</p>
    "! @raising zcx_brainfuck_error | <p class="shorttext synchronized" lang="en">Code generation error</p>
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

    CLASS-METHODS generate_subroutine
      IMPORTING
        it_prog_src     TYPE zcl_brainfuck_code_gen=>tt_prog_src
      RETURNING
        VALUE(r_result) TYPE string
      RAISING
        zcx_brainfuck_code_gen_error.

    CLASS-METHODS get_generated_by
      RETURNING
        VALUE(r_result) TYPE t_prog_src.

    CLASS-METHODS get_generated_at
      RETURNING
        VALUE(r_result) TYPE t_prog_src.
ENDCLASS.

CLASS zcl_brainfuck_code_gen IMPLEMENTATION.
  METHOD get_dyn_program_header.
    r_result = VALUE #(
        ( 'program.                                                      ' )
        ( '*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""*' )
        ( '* Brainf*ck code generation                                   ' )
        ( '* Template Version: 1.1                                       ' )
        ( get_generated_by( )                                           )
        ( get_generated_at( )                                           )
        ( '*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""*' )
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
            APPEND |ir_output->write_character( cell ).| TO prog_src.
          ENDDO.

        WHEN zif_brainfuck_instruction=>instruction_type-read_char.
          DO <instruction>->repeated TIMES.
            APPEND |cell = ir_input->read_character( ).| TO prog_src.
          ENDDO.

        WHEN zif_brainfuck_instruction=>instruction_type-jmp_if_zero.
          DO <instruction>->repeated TIMES.
            APPEND |WHILE cell <> 0.| TO prog_src.
          ENDDO.

        WHEN zif_brainfuck_instruction=>instruction_type-jmp_if_not_zero.
          DO <instruction>->repeated TIMES.
            APPEND |ENDWHILE.| TO prog_src.
          ENDDO.

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

    DATA(dyn_class) = generate_subroutine( prog_src ).

    et_source_code[] = prog_src[].
    CREATE OBJECT r_result TYPE (dyn_class).
  ENDMETHOD.

  METHOD zif_brainfuck_executor~execute.
    " Generate and immediately call the dyn object
    DATA(dyn_exec) = generate_program(
                           EXPORTING
                             it_instructions     = it_instructions
                             i_memory_cells      = i_memory_cells ).

    dyn_exec->execute(
      EXPORTING
        it_instructions     = it_instructions
        ir_input            = ir_input
        ir_output           = ir_output
        i_memory_cells      = i_memory_cells ).
  ENDMETHOD.

  METHOD generate_subroutine.
    DATA syntax_error_info TYPE zcx_brainfuck_code_gen_error=>t_syntax_error.

    GENERATE SUBROUTINE POOL it_prog_src NAME    DATA(dyn_prog)
                                         MESSAGE syntax_error_info-message
                                         LINE    syntax_error_info-line
                                         OFFSET  syntax_error_info-offset
                                         WORD    syntax_error_info-word.

    IF sy-subrc <> 0.
      " Generation error
      RAISE EXCEPTION TYPE zcx_brainfuck_code_gen_error
        EXPORTING
          it_program_source = CONV #( it_prog_src )
          i_syntax_error    = syntax_error_info.
    ENDIF.

    r_result = `\PROGRAM=` && dyn_prog && `\CLASS=MAIN`.
  ENDMETHOD.

  METHOD get_generated_by.
    r_result = |* Generated By: { cl_abap_syst=>get_user_name( ) }|.
  ENDMETHOD.

  METHOD get_generated_at.
    r_result = |* Generated At: { sy-datum } { sy-uzeit }|.
  ENDMETHOD.
ENDCLASS.

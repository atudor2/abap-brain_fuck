*&---------------------------------------------------------------------*
*& Report zbrainfuck_execute
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbrainfuck_execute.

*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*" Application Class (Definition)
*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_application DEFINITION
  CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES zif_brainfuck_input_stream.
    INTERFACES zif_brainfuck_output_stream.

    TYPES:
      BEGIN OF t_class_pair,
        execution_class TYPE string,
        compiler_class  TYPE string,
      END OF t_class_pair,

      BEGIN OF ENUM instruction_dump_meth,
        no_dump,
        basic_dump,
        full_dump,
      END OF ENUM instruction_dump_meth.

    METHODS constructor
      IMPORTING
        i_parse_debug_instr TYPE abap_bool
        i_class_pair        TYPE t_class_pair
        i_dump_instr_method TYPE instruction_dump_meth
        i_code              TYPE string.

    CLASS-METHODS main.
    CLASS-METHODS pbo.
    CLASS-METHODS initialise.
    CLASS-METHODS pai.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA output_buffer TYPE string.
    DATA class_pairs TYPE lcl_application=>t_class_pair.
    DATA enable_debug_instructions TYPE abap_bool.
    DATA instruction_dump_method TYPE lcl_application=>instruction_dump_meth.
    DATA code TYPE string.
    CLASS-DATA text_editor TYPE REF TO cl_gui_textedit.
    CLASS-DATA c_dock_ratio TYPE i VALUE 75.

    CLASS-METHODS init_class_dropdown
      IMPORTING
        i_class TYPE string
        i_field TYPE string.

    CLASS-METHODS get_dropdown_values
      IMPORTING
        it_class_list   TYPE seor_implementing_keys
      RETURNING
        VALUE(r_result) TYPE vrm_values.

    METHODS run.

    METHODS print
      IMPORTING
        i_value TYPE string.

    METHODS dump_instructions
      IMPORTING
        it_instructions TYPE zif_brainfuck_instruction=>tt_instructions.

    METHODS get_compiler
      RETURNING
        VALUE(r_result) TYPE REF TO zif_brainfuck_compiler.

    METHODS get_executor
      RETURNING
        VALUE(r_result) TYPE REF TO zif_brainfuck_executor.

    CLASS-METHODS get_code
      RETURNING
        VALUE(r_result) TYPE string.
    CLASS-METHODS get_instruction_dump_method
      RETURNING
        VALUE(r_result) TYPE instruction_dump_meth.
ENDCLASS.

*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*" Selection Screen Definition
*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
PARAMETERS p_cmplr TYPE seoclskey AS LISTBOX VISIBLE LENGTH 30 OBLIGATORY. " Compiler Class
PARAMETERS p_exec  TYPE seoclskey AS LISTBOX VISIBLE LENGTH 30 OBLIGATORY. " Execution Class

PARAMETERS p_instr1 TYPE abap_bool RADIOBUTTON GROUP inst DEFAULT 'X'. " Do not dump instructions
PARAMETERS p_instr2 TYPE abap_bool RADIOBUTTON GROUP inst.             " Dump instructions
PARAMETERS p_instr3 TYPE abap_bool RADIOBUTTON GROUP inst.             " Dump instructions (with comments)

PARAMETERS p_debug TYPE abap_bool AS CHECKBOX. " Enable debug instruction?

*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*" Selection Screen Events
*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
INITIALIZATION.
  lcl_application=>initialise( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_application=>pbo( ).

AT SELECTION-SCREEN.
  lcl_application=>pai( ).

START-OF-SELECTION.
  lcl_application=>main( ).

*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*" Application Class (Implementation)
*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_application IMPLEMENTATION.
  METHOD main.
    DATA(instance) = NEW lcl_application(
        i_parse_debug_instr = p_debug
        i_class_pair        = VALUE #( compiler_class  = p_cmplr
                                       execution_class = p_exec )
        i_dump_instr_method = get_instruction_dump_method( )
        i_code              = get_code( ) ).
    instance->run( ).
  ENDMETHOD.

  METHOD zif_brainfuck_output_stream~flush.
    print( me->output_buffer ).

    CLEAR me->output_buffer.
  ENDMETHOD.

  METHOD zif_brainfuck_input_stream~read_character.
    cl_demo_input=>request(
      EXPORTING
        text        = 'Input:'
      CHANGING
        field       = r_result ).
  ENDMETHOD.

  METHOD zif_brainfuck_output_stream~write_character.
    CONCATENATE me->output_buffer i_character INTO me->output_buffer RESPECTING BLANKS. " Got to love automatic string trims... ;)
  ENDMETHOD.

  METHOD zif_brainfuck_output_stream~write_string.
    me->output_buffer = me->output_buffer && i_string.
  ENDMETHOD.

  METHOD print.
    WRITE: / i_value.
  ENDMETHOD.

  METHOD run.
    DATA(code) = me->code.

    DATA(compiler) = me->get_compiler( ).
    DATA(executor) = me->get_executor( ).

    TRY.
        GET RUN TIME FIELD DATA(compile_time).

        compiler->compile(
          EXPORTING
            i_code              = code
            i_allow_debugger    = me->enable_debug_instructions
          IMPORTING
            et_instructions     = DATA(instructions) ).

        GET RUN TIME FIELD compile_time.
        print( |Compile Time: { compile_time } microseconds| ).

        me->dump_instructions( it_instructions = instructions ).

        GET RUN TIME FIELD DATA(runtime).
        executor->execute(
            EXPORTING
                it_instructions     = instructions
                ir_input            = me
                ir_output           = me ).

        GET RUN TIME FIELD runtime.
        print( |Runtime: { runtime } microseconds| ).
      CATCH zcx_brainfuck_syntax_error INTO DATA(syntax_ex).
        print( syntax_ex->get_error_message( ) ).
      CATCH zcx_brainfuck_error INTO DATA(ex).
        print( ex->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD dump_instructions.
    CHECK me->instruction_dump_method <> no_dump.

    DATA(i) = 0.
    LOOP AT it_instructions ASSIGNING FIELD-SYMBOL(<ins>).
      " Skip comments and debug commands if basic dump
      IF me->instruction_dump_method = basic_dump AND
         ( <ins>->type = <ins>->instruction_type-comment OR
           <ins>->type = <ins>->instruction_type-debugger ).
        CONTINUE.
      ENDIF.

      i = i + 1.
      print( |[{ i }] Source: { <ins>->source_code_location } -> { <ins>->type }(x{ <ins>->repeated })[Argument = { <ins>->argument }]| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD pbo.
    RETURN.
  ENDMETHOD.

  METHOD initialise.
    " Class selection drop downs
    init_class_dropdown( i_class = |ZIF_BRAINFUCK_COMPILER| i_field = 'P_CMPLR' ).
    init_class_dropdown( i_class = |ZIF_BRAINFUCK_EXECUTOR| i_field = 'P_EXEC' ).

    DATA(dock) = NEW cl_gui_docking_container( repid = sy-cprog
                                               dynnr = sy-dynnr
                                               side  = cl_gui_docking_container=>dock_at_bottom
                                               ratio = c_dock_ratio ).

    text_editor = NEW cl_gui_textedit( parent = dock ).
    text_editor->set_navigate_on_dblclick( 0 ).
    text_editor->set_adjust_design( 0 ).
    text_editor->set_grid_handle( 0 ).
    text_editor->set_statusbar_mode( 0 ).
    text_editor->set_toolbar_mode( 0 ).
  ENDMETHOD.

  METHOD init_class_dropdown.
    DATA implementations TYPE seor_implementing_keys.

    " Get Implementations of interface:
    CALL FUNCTION 'SEO_INTERFACE_IMPLEM_GET_ALL'
      EXPORTING
        intkey       = CONV seoclskey( i_class )
      IMPORTING
        impkeys      = implementations
      EXCEPTIONS
        not_existing = 1
        OTHERS       = 2.
    ASSERT sy-subrc = 0.

    " Pull texts and and add to the list box values
    DATA(vrm_values) = get_dropdown_values( it_class_list = implementations ).
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = CONV vrm_id( i_field )
        values          = vrm_values
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    ASSERT sy-subrc = 0.
  ENDMETHOD.


  METHOD get_dropdown_values.
    LOOP AT it_class_list ASSIGNING FIELD-SYMBOL(<class>).
      DATA class TYPE seoc_class_r.

      CALL FUNCTION 'SEO_CLASS_READ'
        EXPORTING
          clskey  = CONV seoclskey( <class>-clsname )
          version = seoc_version_active
        IMPORTING
          class   = class.

      INSERT VALUE #( key = CONV #( <class>-clsname ) text = CONV #( class-descript ) ) INTO TABLE r_result.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_compiler.
    CREATE OBJECT r_result TYPE (me->class_pairs-compiler_class).
  ENDMETHOD.

  METHOD get_executor.
    CREATE OBJECT r_result TYPE (me->class_pairs-execution_class).
  ENDMETHOD.

  METHOD pai.
    RETURN.
  ENDMETHOD.

  METHOD get_code.
    text_editor->get_textstream(
      IMPORTING
        text                   = r_result
      EXCEPTIONS
        error_cntl_call_method = 1
        not_supported_by_gui   = 2
        OTHERS                 = 3 ).

    ASSERT sy-subrc = 0.

    cl_gui_cfw=>flush( ). " Required to get R_RESULT filled
  ENDMETHOD.

  METHOD constructor.
    me->class_pairs               = i_class_pair.
    me->enable_debug_instructions = i_parse_debug_instr.
    me->instruction_dump_method   = i_dump_instr_method.
    me->code                      = i_code.
  ENDMETHOD.

  METHOD get_instruction_dump_method.
    r_result = COND #( WHEN p_instr1 = abap_true THEN no_dump
                       WHEN p_instr2 = abap_true THEN basic_dump
                       WHEN p_instr3 = abap_true THEN full_dump ).
  ENDMETHOD.

ENDCLASS.

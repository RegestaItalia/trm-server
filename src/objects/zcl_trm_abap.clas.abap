CLASS zcl_trm_abap DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_object
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA: object TYPE trobjtype READ-ONLY.

    METHODS constructor
      IMPORTING object TYPE trobjtype
                key    TYPE ztrm_object.

    METHODS zif_trm_object~get_dependencies REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_param,
             name  TYPE string,
             value TYPE string,
           END OF ty_param,
           tyt_param TYPE STANDARD TABLE OF ty_param WITH DEFAULT KEY,
           BEGIN OF ty_function_module,
             funcname  TYPE rs38l_fnam,
             exporting TYPE tyt_param,
             importing TYPE tyt_param,
             tables    TYPE tyt_param,
             changing  TYPE tyt_param,
           END OF ty_function_module,
           tyt_function_module TYPE STANDARD TABLE OF ty_function_module WITH DEFAULT KEY.
    TYPES: ty_program     TYPE c LENGTH 180,
           tyt_programs   TYPE STANDARD TABLE OF ty_program WITH DEFAULT KEY,
           tyt_tokens     TYPE TABLE OF stoken,
           tyt_statements TYPE TABLE OF sstmnt.

    DATA: function_modules TYPE tyt_function_module.

    METHODS get_clas_programs
      RETURNING VALUE(rt_programs) TYPE tyt_programs.
    METHODS get_fugr_programs
      RETURNING VALUE(rt_programs) TYPE tyt_programs.
    METHODS get_prog_programs
      RETURNING VALUE(rt_programs) TYPE tyt_programs.
    METHODS extract_function_modules
      IMPORTING it_tokens     TYPE tyt_tokens
                it_statements TYPE tyt_statements.

    METHODS add_nrob
      CHANGING ct_dependencies TYPE ztrm_object_dependency_t.
ENDCLASS.



CLASS zcl_trm_abap IMPLEMENTATION.

  METHOD constructor.
    super->constructor( key = key ).
    me->object = object.
  ENDMETHOD.

  METHOD zif_trm_object~get_dependencies.
    DATA: lt_programs       TYPE tyt_programs,
          lv_program        LIKE LINE OF lt_programs,
          lt_program_src    TYPE STANDARD TABLE OF string,
          lt_src_tokens     TYPE TABLE OF stoken,
          lt_src_statements TYPE TABLE OF sstmnt.

    super->zif_trm_object~get_dependencies(
      IMPORTING
        et_dependencies = et_dependencies
    ).

    CASE object.
      WHEN 'CLAS'.
        lt_programs = get_clas_programs( ).
      WHEN 'FUGR'.
        lt_programs = get_fugr_programs( ).
      WHEN 'PROG'.
        lt_programs = get_prog_programs( ).
    ENDCASE.

    CHECK lt_programs[] IS NOT INITIAL.

    LOOP AT lt_programs INTO lv_program.
      CLEAR lt_program_src[].
      CLEAR lt_src_tokens[].
      CLEAR lt_src_statements[].
      READ REPORT lv_program INTO lt_program_src.
      SCAN ABAP-SOURCE lt_program_src
        TOKENS INTO    lt_src_tokens
        STATEMENTS INTO lt_src_statements.
      extract_function_modules(
        it_tokens     = lt_src_tokens
        it_statements = lt_src_statements
      ).
    ENDLOOP.

    add_nrob( CHANGING ct_dependencies = et_dependencies ).
  ENDMETHOD.

  METHOD get_clas_programs.
    DATA: lv_classname TYPE seoclsname,
          lt_includes  TYPE seop_methods_w_include,
          ls_include   LIKE LINE OF lt_includes.
    lv_classname = key-obj_name.
    TRY.
        lt_includes = cl_oo_classname_service=>get_all_method_includes(
          EXPORTING
            clsname           = lv_classname
            with_enhancements = 'X'
        ).
      CATCH cx_class_not_existent.
    ENDTRY.
    LOOP AT lt_includes INTO ls_include.
      APPEND ls_include-incname TO rt_programs.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_fugr_programs.
    DATA ls_senvi TYPE senvi.
    LOOP AT senvi INTO ls_senvi WHERE type EQ 'INCL'.
      APPEND ls_senvi-object TO rt_programs.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_prog_programs.
    DATA ls_senvi TYPE senvi.
    READ TABLE senvi TRANSPORTING NO FIELDS WITH KEY type = 'INCL' object = key-obj_name.
    IF sy-subrc <> 0.
      APPEND key-obj_name TO rt_programs.
    ENDIF.
    LOOP AT senvi INTO ls_senvi WHERE type EQ 'INCL'.
      APPEND ls_senvi-object TO rt_programs.
    ENDLOOP.
  ENDMETHOD.

  METHOD extract_function_modules.
    DATA: lv_param     TYPE string,
          lv_call      TYPE flag,
          lv_function  TYPE flag,
          lt_params    TYPE tyt_param,
          ls_statement TYPE sstmnt,
          ls_token     TYPE stoken.

    FIELD-SYMBOLS: <fs_func>   TYPE ty_function_module,
                   <fs_param>  TYPE ty_param,
                   <fs_tparam> TYPE tyt_param.
    LOOP AT it_statements INTO ls_statement.
      UNASSIGN <fs_func>.
      CLEAR lv_param.
      CLEAR lt_params.
      CLEAR ls_token.
      LOOP AT it_tokens INTO ls_token FROM ls_statement-from TO ls_statement-to.
        IF <fs_func> IS NOT ASSIGNED.
          IF ls_token-str EQ 'CALL' AND ls_token-type EQ 'I'.
            lv_call = 'X'.
          ELSEIF lv_call EQ 'X' AND ls_token-str EQ 'FUNCTION' AND ls_token-type EQ 'I'.
            lv_function = 'X'.
          ELSEIF lv_call EQ 'X' AND lv_function EQ 'X' AND ls_token-type EQ 'S'.
            APPEND INITIAL LINE TO function_modules ASSIGNING <fs_func>.
            <fs_func>-funcname = ls_token-str.
            REPLACE ALL OCCURRENCES OF '''' IN <fs_func>-funcname WITH ''.
          ENDIF.
        ELSE.
          IF ls_token-str EQ 'IMPORTING'.
            IF lv_param IS NOT INITIAL.
              ASSIGN COMPONENT lv_param OF STRUCTURE <fs_func> TO <fs_tparam>.
              APPEND LINES OF lt_params TO <fs_tparam>.
            ENDIF.
            lv_param = 'IMPORTING'.
            CLEAR lt_params.
            CONTINUE.
          ELSEIF ls_token-str EQ 'EXPORTING'.
            IF lv_param IS NOT INITIAL.
              ASSIGN COMPONENT lv_param OF STRUCTURE <fs_func> TO <fs_tparam>.
              APPEND LINES OF lt_params TO <fs_tparam>.
            ENDIF.
            lv_param = 'EXPORTING'.
            CONTINUE.
          ELSEIF ls_token-str EQ 'CHANGING'.
            IF lv_param IS NOT INITIAL.
              ASSIGN COMPONENT lv_param OF STRUCTURE <fs_func> TO <fs_tparam>.
              APPEND LINES OF lt_params TO <fs_tparam>.
            ENDIF.
            lv_param = 'CHANGING'.
            CLEAR lt_params.
            CONTINUE.
          ELSEIF ls_token-str EQ 'TABLES'.
            IF lv_param IS NOT INITIAL.
              ASSIGN COMPONENT lv_param OF STRUCTURE <fs_func> TO <fs_tparam>.
              APPEND LINES OF lt_params TO <fs_tparam>.
            ENDIF.
            lv_param = 'TABLES'.
            CLEAR lt_params.
            CONTINUE.
          ENDIF.
          CHECK lv_param IS NOT INITIAL.
          CHECK ls_token-str <> '='.
          IF <fs_param> IS NOT ASSIGNED.
            APPEND INITIAL LINE TO lt_params ASSIGNING <fs_param>.
          ENDIF.
          IF <fs_param>-name IS INITIAL.
            <fs_param>-name = ls_token-str.
            CONDENSE <fs_param>-name.
          ELSEIF <fs_param>-value IS INITIAL.
            <fs_param>-value = ls_token-str.
            IF <fs_param>-value(1) EQ ''''.
              <fs_param>-value = <fs_param>-value+1.
              SHIFT <fs_param>-value RIGHT DELETING TRAILING ''''.
            ENDIF.
            CONDENSE <fs_param>-value.
            UNASSIGN <fs_param>.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF lv_param IS NOT INITIAL.
        ASSIGN COMPONENT lv_param OF STRUCTURE <fs_func> TO <fs_tparam>.
        APPEND LINES OF lt_params TO <fs_tparam>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_nrob.
    DATA: ls_function_module TYPE ty_function_module,
          ls_exporting       TYPE ty_param,
          lt_nrob_check      TYPE STANDARD TABLE OF sobj_name,
          lt_nrob            TYPE STANDARD TABLE OF tadir,
          ls_nrob            TYPE tadir.
    FIELD-SYMBOLS <fs_dep> TYPE ztrm_object_dependency.

    LOOP AT function_modules INTO ls_function_module WHERE funcname = 'NUMBER_GET_NEXT'.
      READ TABLE ls_function_module-exporting INTO ls_exporting WITH KEY name = 'OBJECT'.
      CHECK sy-subrc EQ 0.
      IF strlen( ls_exporting-value ) LE 40.
        APPEND ls_exporting-value TO lt_nrob_check.
      ENDIF.
    ENDLOOP.

    CHECK lt_nrob_check[] IS NOT INITIAL.

    SELECT pgmid object obj_name
    INTO CORRESPONDING FIELDS OF TABLE lt_nrob
    FROM tadir
    FOR ALL ENTRIES IN lt_nrob_check
    WHERE pgmid EQ 'R3TR' AND object EQ 'NROB' AND obj_name EQ lt_nrob_check-table_line.

    LOOP AT lt_nrob INTO ls_nrob.
      READ TABLE ct_dependencies TRANSPORTING NO FIELDS WITH KEY tabname = 'TADIR' tabkey = ls_nrob.
      CHECK sy-subrc <> 0.
      TRY.
          APPEND get_tadir_dependency(
            EXPORTING
              object     = ls_nrob-object
              obj_name   = ls_nrob-obj_name
          ) TO ct_dependencies.
        CATCH zcx_trm_exception.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

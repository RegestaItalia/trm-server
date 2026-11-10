CLASS /atrm/cl_abap DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: ty_program        TYPE c LENGTH 180,
           ty_program_src    TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
           ty_src_tokens     TYPE TABLE OF stoken WITH DEFAULT KEY,
           ty_src_statements TYPE TABLE OF sstmnt WITH DEFAULT KEY.
    TYPES: BEGIN OF ty_source_code,
             program     TYPE ty_program,
             source_code TYPE ty_program_src,
             tokens      TYPE ty_src_tokens,
             statements  TYPE ty_src_statements,
           END OF ty_source_code,
           tyt_source_code TYPE STANDARD TABLE OF ty_source_code WITH DEFAULT KEY.

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

    DATA: object           TYPE trobjtype READ-ONLY,
          source_code      TYPE tyt_source_code READ-ONLY,
          function_modules TYPE tyt_function_module READ-ONLY.

    METHODS constructor
      IMPORTING object TYPE trobjtype
                key    TYPE /atrm/object.

    METHODS /atrm/if_object~get_dependencies REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: tyt_programs   TYPE STANDARD TABLE OF ty_program WITH DEFAULT KEY,
           tyt_tokens     TYPE TABLE OF stoken,
           tyt_statements TYPE TABLE OF sstmnt.
    DATA: programs TYPE tyt_programs.
    DATA: includes TYPE tyt_programs.
    METHODS get_clas_programs
      RETURNING VALUE(rt_programs) TYPE tyt_programs.
    METHODS get_fugr_programs
      RETURNING VALUE(rt_programs) TYPE tyt_programs.
    METHODS get_prog_programs
      RETURNING VALUE(rt_programs) TYPE tyt_programs.
    METHODS extract_function_modules
      IMPORTING it_tokens     TYPE tyt_tokens
                it_statements TYPE tyt_statements.
    METHODS extract_includes
      IMPORTING it_tokens     TYPE tyt_tokens
                it_statements TYPE tyt_statements.

    METHODS add_nrob
      CHANGING ct_dependencies TYPE /atrm/object_dependency_t.
    METHODS add_docv
      CHANGING ct_dependencies TYPE /atrm/object_dependency_t.
    METHODS add_includes
      CHANGING ct_dependencies TYPE /atrm/object_dependency_t.
METHODS get_main_program
      RETURNING VALUE(rv_program) TYPE program.
METHODS add_safety_dependencies
      CHANGING ct_dependencies TYPE /atrm/object_dependency_t.
METHODS append_safety_dependency
      IMPORTING is_dependency  TYPE /atrm/object_dependency
      CHANGING  ct_dependencies TYPE /atrm/object_dependency_t.
ENDCLASS.



CLASS /atrm/cl_abap IMPLEMENTATION.

  METHOD constructor.
    DATA: lv_program        LIKE LINE OF me->programs,
          lt_program_src    TYPE ty_program_src,
          lt_src_tokens     TYPE ty_src_tokens,
          lt_src_statements TYPE ty_src_statements.
    FIELD-SYMBOLS <fs_source_code> TYPE ty_source_code.
    super->constructor( key = key ).
    me->object = object.

    IF object = 'FUGR' OR object = 'PROG'.
      " get_fugr_programs / get_prog_programs need the repository environment, but
      " the base class only fills it later, inside get_dependencies; read it here
      " so the program/include list below is not empty. read_senvi is idempotent,
      " so the later call from get_dependencies (via super) does not refetch it.
      read_senvi( ).
    ENDIF.

    CASE object.
      WHEN 'CLAS'.
        me->programs = get_clas_programs( ).
      WHEN 'FUGR'.
        me->programs = get_fugr_programs( ).
      WHEN 'PROG'.
        me->programs = get_prog_programs( ).
    ENDCASE.
    LOOP AT me->programs INTO lv_program.
      CLEAR lt_program_src[].
      CLEAR lt_src_tokens[].
      CLEAR lt_src_statements[].
      UNASSIGN <fs_source_code>.
      APPEND INITIAL LINE TO me->source_code ASSIGNING <fs_source_code>.
      READ REPORT lv_program INTO lt_program_src.
      SCAN ABAP-SOURCE lt_program_src
        TOKENS INTO    lt_src_tokens
        STATEMENTS INTO lt_src_statements.
      <fs_source_code>-program = lv_program.
      <fs_source_code>-source_code = lt_program_src.
      <fs_source_code>-statements = lt_src_statements.
      <fs_source_code>-tokens = lt_src_tokens.
      extract_function_modules(
        it_tokens     = lt_src_tokens
        it_statements = lt_src_statements
      ).
      extract_includes(
        it_tokens     = lt_src_tokens
        it_statements = lt_src_statements
      ).
    ENDLOOP.
  ENDMETHOD.

  METHOD /atrm/if_object~get_dependencies.
    super->/atrm/if_object~get_dependencies(
      IMPORTING
        dependencies = dependencies
    ).
    add_nrob( CHANGING ct_dependencies = dependencies ).
    add_docv( CHANGING ct_dependencies = dependencies ).
    add_includes( CHANGING ct_dependencies = dependencies ).
    add_safety_dependencies( CHANGING ct_dependencies = dependencies ).
  ENDMETHOD.

  METHOD get_clas_programs.
    DATA: lv_classname TYPE seoclsname,
          lt_includes  TYPE seop_methods_w_include,
          ls_include   LIKE LINE OF lt_includes.
    lv_classname = key-obj_name.
    TRY.
        CALL METHOD ('CL_OO_CLASSNAME_SERVICE')=>('GET_ALL_METHOD_INCLUDES')
          EXPORTING
            clsname           = lv_classname
            with_enhancements = 'X'
          RECEIVING
            result = lt_includes
        .
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
    APPEND key-obj_name TO rt_programs.
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

  METHOD extract_includes.
    DATA: lv_keyword   TYPE flag,
          lv_include   TYPE ty_program,
          ls_statement TYPE sstmnt,
          ls_token     TYPE stoken.

    LOOP AT it_statements INTO ls_statement.
      CLEAR lv_keyword.
      CLEAR lv_include.
      CLEAR ls_token.
      LOOP AT it_tokens INTO ls_token FROM ls_statement-from TO ls_statement-to.
        IF lv_keyword IS INITIAL.
          IF ls_token-str EQ 'INCLUDE' AND ls_token-type EQ 'I'.
            lv_keyword = 'X'.
          ENDIF.
        ELSEIF lv_include IS INITIAL.
          " skip TYPES/DATA "INCLUDE STRUCTURE ..." / "INCLUDE TYPE ...", only the
          " plain "INCLUDE <program>." program-include statement is a dependency
          IF ls_token-str EQ 'STRUCTURE' OR ls_token-str EQ 'TYPE'.
            EXIT.
          ENDIF.
          lv_include = ls_token-str.
        ENDIF.
      ENDLOOP.
      CHECK lv_include IS NOT INITIAL.
      TRANSLATE lv_include TO UPPER CASE.
      APPEND lv_include TO includes.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_nrob.
    DATA: ls_function_module TYPE ty_function_module,
          ls_exporting       TYPE ty_param,
          lt_nrob_check      TYPE STANDARD TABLE OF sobj_name,
          lt_nrob            TYPE STANDARD TABLE OF tadir,
          ls_nrob            TYPE tadir,
          ls_dependency      LIKE LINE OF ct_dependencies.

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
      CLEAR ls_dependency.
      TRY.
          get_tadir_dependency(
            EXPORTING
              object = ls_nrob-object
              obj_name = ls_nrob-obj_name
            RECEIVING
              dependency = ls_dependency
          ).
          READ TABLE ct_dependencies
            WITH KEY tabname = ls_dependency-tabname tabkey = ls_dependency-tabkey
            TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND ls_dependency TO ct_dependencies.
          ENDIF.
        CATCH /atrm/cx_exception.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_docv.
    DATA: ls_function_module TYPE ty_function_module,
          ls_exporting       TYPE ty_param,
          lv_objname_full    TYPE sobj_name,
          lv_namespace       TYPE string,
          lv_objname         TYPE string,
          lt_docv_check      TYPE STANDARD TABLE OF sobj_name,
          lt_docv            TYPE STANDARD TABLE OF tadir,
          ls_docv            TYPE tadir,
          ls_dependency      LIKE LINE OF ct_dependencies.

    LOOP AT function_modules INTO ls_function_module WHERE funcname = 'POPUP_DISPLAY_TEXT'.
      READ TABLE ls_function_module-exporting INTO ls_exporting WITH KEY name = 'TEXT_OBJECT'.
      CHECK sy-subrc EQ 0.
      IF strlen( ls_exporting-value ) LE 38.
        CLEAR lv_objname_full.
        IF ls_exporting-value CP '/*'.
          CLEAR lv_namespace.
          CLEAR lv_objname.
          FIND REGEX '^(/[^/]+/)(.+)$' IN ls_exporting-value SUBMATCHES lv_namespace lv_objname.
          CONCATENATE lv_namespace 'DT' lv_objname INTO lv_objname_full.
        ELSE.
          CONCATENATE 'DT' ls_exporting-value INTO lv_objname_full.
        ENDIF.
        APPEND lv_objname_full TO lt_docv_check.
      ENDIF.
    ENDLOOP.

    CHECK lt_docv_check[] IS NOT INITIAL.

    SELECT pgmid object obj_name
    INTO CORRESPONDING FIELDS OF TABLE lt_docv
    FROM tadir
    FOR ALL ENTRIES IN lt_docv_check
    WHERE pgmid EQ 'R3TR' AND object EQ 'DOCV' AND obj_name EQ lt_docv_check-table_line.

    LOOP AT lt_docv INTO ls_docv.
      CLEAR ls_dependency.
      TRY.
          get_tadir_dependency(
            EXPORTING
              object = ls_docv-object
              obj_name = ls_docv-obj_name
            RECEIVING
              dependency = ls_dependency
          ).
          READ TABLE ct_dependencies
            WITH KEY tabname = ls_dependency-tabname tabkey = ls_dependency-tabkey
            TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND ls_dependency TO ct_dependencies.
          ENDIF.
        CATCH /atrm/cx_exception.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_includes.
    DATA: lv_include    TYPE ty_program,
          lv_obj_name   TYPE sobj_name,
          ls_dependency TYPE /atrm/object_dependency.

    LOOP AT includes INTO lv_include.
      lv_obj_name = lv_include.
      TRY.
          CLEAR ls_dependency.
          get_tadir_dependency(
            EXPORTING
              object     = 'PROG'
              obj_name   = lv_obj_name
            RECEIVING
              dependency = ls_dependency
          ).
          READ TABLE ct_dependencies
            WITH KEY tabname = ls_dependency-tabname tabkey = ls_dependency-tabkey
            TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND ls_dependency TO ct_dependencies.
          ENDIF.
        CATCH /atrm/cx_exception.
          " referenced include may no longer exist
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_main_program.
    DATA: lv_clsname   TYPE seoclsname,
          lv_name      TYPE string,
          lv_namespace TYPE string,
          lv_group     TYPE string.
    lv_name = key-obj_name.
    TRY.
        CASE object.
          WHEN 'PROG'.
            rv_program = key-obj_name.
          WHEN 'CLAS'.
            lv_clsname = key-obj_name.
            CALL METHOD ('CL_OO_CLASSNAME_SERVICE')=>('GET_CLASSPOOL_NAME')
              EXPORTING
                clsname = lv_clsname
              RECEIVING
                result  = rv_program.
          WHEN 'INTF'.
            lv_clsname = key-obj_name.
            CALL METHOD ('CL_OO_CLASSNAME_SERVICE')=>('GET_INTERFACEPOOL_NAME')
              EXPORTING
                clsname = lv_clsname
              RECEIVING
                result  = rv_program.
          WHEN 'FUGR'.
            IF lv_name CP '/*'.
              FIND REGEX '^(/[^/]+/)(.+)$' IN lv_name SUBMATCHES lv_namespace lv_group.
              CONCATENATE lv_namespace 'SAPL' lv_group INTO rv_program.
            ELSE.
              CONCATENATE 'SAPL' lv_name INTO rv_program.
            ENDIF.
        ENDCASE.
      CATCH cx_root.
        " class name service not available in this release
        CLEAR rv_program.
    ENDTRY.
  ENDMETHOD.

  METHOD add_safety_dependencies.
    " safety handler: Repository Environment reads the where-used index, which
    " can be missing (e.g. objects imported without an index update). Ask the
    " compiler for the external objects used by the main program instead, so the
    " dependencies are found even when the index is empty.
    TYPES: BEGIN OF ty_used_object,
             trobjtype TYPE trobjtype,
             sobj_name TYPE sobj_name,
           END OF ty_used_object.
    DATA: lv_program     TYPE program,
          lv_true        TYPE abap_bool VALUE abap_true,
          lv_error       TYPE c LENGTH 1,
          lo_compiler    TYPE REF TO object,
          lr_usages      TYPE REF TO data,
          lt_ptab        TYPE abap_parmbind_tab,
          ls_ptab        TYPE abap_parmbind,
          lt_used        TYPE SORTED TABLE OF ty_used_object WITH UNIQUE KEY trobjtype sobj_name,
          ls_used        TYPE ty_used_object,
          lv_table       TYPE tabname,
          lv_where       TYPE string,
          lv_funcname    TYPE string,
          lv_area        TYPE sobj_name,
          lv_enqu        TYPE string,
          ls_dependency  TYPE /atrm/object_dependency.
    FIELD-SYMBOLS: <lt_usages> TYPE STANDARD TABLE,
                   <ls_usage>  TYPE any,
                   <lv_type>   TYPE any,
                   <lv_name>   TYPE any.

    lv_program = get_main_program( ).
    CHECK lv_program IS NOT INITIAL.

    TRY.
        CREATE DATA lr_usages TYPE ('SCR_OBJ_USAGES').
        ASSIGN lr_usages->* TO <lt_usages>.
        ls_ptab-name = 'P_NAME'.
        ls_ptab-kind = cl_abap_objectdescr=>exporting.
        GET REFERENCE OF lv_program INTO ls_ptab-value.
        INSERT ls_ptab INTO TABLE lt_ptab.
        ls_ptab-name = 'P_EXT_OBJECT_USAGE'.
        ls_ptab-kind = cl_abap_objectdescr=>exporting.
        GET REFERENCE OF lv_true INTO ls_ptab-value.
        INSERT ls_ptab INTO TABLE lt_ptab.
        CREATE OBJECT lo_compiler TYPE ('CL_ABAP_COMPILER')
          PARAMETER-TABLE lt_ptab.
        CALL METHOD lo_compiler->('GET_EXT_OBJ_USAGE')
          IMPORTING
            p_obj_usages = <lt_usages>
            p_error      = lv_error.
      CATCH cx_root.
        " compiler object usage API not available in this release
        RETURN.
    ENDTRY.

    LOOP AT <lt_usages> ASSIGNING <ls_usage>.
      ASSIGN COMPONENT 'TROBJTYPE' OF STRUCTURE <ls_usage> TO <lv_type>.
      CHECK sy-subrc = 0.
      ASSIGN COMPONENT 'SOBJ_NAME' OF STRUCTURE <ls_usage> TO <lv_name>.
      CHECK sy-subrc = 0.
      CHECK <lv_type> IS NOT INITIAL AND <lv_name> IS NOT INITIAL.
      CLEAR ls_used.
      ls_used-trobjtype = <lv_type>.
      ls_used-sobj_name = <lv_name>.
      CHECK NOT ( ls_used-trobjtype = object AND ls_used-sobj_name = key-obj_name ).
      INSERT ls_used INTO TABLE lt_used.
    ENDLOOP.

    LOOP AT lt_used INTO ls_used.
      CASE ls_used-trobjtype.
        WHEN 'FUNC'.
          lv_funcname = ls_used-sobj_name.
          " same mapping as /ATRM/CL_SENVI_FUNC: lock modules belong to their lock object
          IF lv_funcname CP 'ENQUEUE_*' OR lv_funcname CP 'DEQUEUE_*'.
            lv_enqu = lv_funcname+8.
            TRY.
                ls_dependency = get_tadir_dependency( object = 'ENQU' obj_name = lv_enqu ).
                append_safety_dependency( EXPORTING is_dependency = ls_dependency CHANGING ct_dependencies = ct_dependencies ).
              CATCH /atrm/cx_exception.
                " lock function module without lock object
            ENDTRY.
            CONTINUE.
          ENDIF.
          CLEAR lv_area.
          TRY.
              lv_table = 'ENLFDIR'.
              REPLACE ALL OCCURRENCES OF '''' IN lv_funcname WITH ''''''.
              CONCATENATE 'FUNCNAME = ''' lv_funcname '''' INTO lv_where.
              SELECT SINGLE area FROM (lv_table) INTO lv_area WHERE (lv_where).
            CATCH cx_root.
              CLEAR lv_area.
          ENDTRY.
          CHECK lv_area IS NOT INITIAL.
          CHECK NOT ( object = 'FUGR' AND lv_area = key-obj_name ).
          TRY.
              ls_dependency = get_tadir_dependency( object = 'FUGR' obj_name = lv_area ).
              append_safety_dependency( EXPORTING is_dependency = ls_dependency CHANGING ct_dependencies = ct_dependencies ).
              ls_dependency = get_tfdir_dependency( funcname = ls_used-sobj_name ).
              append_safety_dependency( EXPORTING is_dependency = ls_dependency CHANGING ct_dependencies = ct_dependencies ).
            CATCH /atrm/cx_exception.
              " function group not in TADIR
          ENDTRY.
        WHEN OTHERS.
          TRY.
              ls_dependency = get_tadir_dependency( object = ls_used-trobjtype obj_name = ls_used-sobj_name ).
              append_safety_dependency( EXPORTING is_dependency = ls_dependency CHANGING ct_dependencies = ct_dependencies ).
            CATCH /atrm/cx_exception.
              " not a TADIR object (e.g. a sub-object of the program)
          ENDTRY.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD append_safety_dependency.
    CHECK is_dependency-tabname IS NOT INITIAL.
    READ TABLE ct_dependencies
      WITH KEY tabname = is_dependency-tabname tabkey = is_dependency-tabkey
      TRANSPORTING NO FIELDS.
    CHECK sy-subrc <> 0.
    APPEND is_dependency TO ct_dependencies.
  ENDMETHOD.
ENDCLASS.


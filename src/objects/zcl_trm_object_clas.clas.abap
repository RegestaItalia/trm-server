CLASS zcl_trm_object_clas DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_object
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
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
    TYPES: tyt_tokens     TYPE TABLE OF stoken,
           tyt_statements TYPE TABLE OF sstmnt.
    METHODS extract_function_modules
      IMPORTING it_tokens           TYPE tyt_tokens
                it_statements       TYPE tyt_statements
      RETURNING VALUE(rt_functions) TYPE tyt_function_module.
    METHODS add_snro
      CHANGING ct_dependencies TYPE ztrm_object_dependency_t.
ENDCLASS.



CLASS zcl_trm_object_clas IMPLEMENTATION.

  METHOD zif_trm_object~get_dependencies.
    super->zif_trm_object~get_dependencies(
      IMPORTING
        et_dependencies = et_dependencies
    ).
    " proof of concept
    add_snro( CHANGING ct_dependencies = et_dependencies ).
  ENDMETHOD.

  METHOD add_snro.
    READ TABLE ct_dependencies TRANSPORTING NO FIELDS WITH KEY tabname = 'TFDIR' tabkey = 'NUMBER_GET_NEXT'.
    CHECK sy-subrc EQ 0.
    DATA lt_src TYPE STANDARD TABLE OF string.
    DATA lt_snro_check TYPE STANDARD TABLE OF sobj_name.
    DATA lv_classname TYPE seoclsname.
    lv_classname = me->key-obj_name.
    DATA(lt_includes) = cl_oo_classname_service=>get_all_method_includes( lv_classname ).
    LOOP AT lt_includes INTO DATA(ls_include).
      CLEAR lt_src.
      READ REPORT ls_include-incname INTO lt_src.
      DATA lt_tokens     TYPE TABLE OF stoken.
      DATA lt_statements TYPE TABLE OF sstmnt.

      SCAN ABAP-SOURCE lt_src
        TOKENS INTO    lt_tokens
        STATEMENTS INTO lt_statements.
      DATA(lt_functions) = extract_function_modules(
        EXPORTING
          it_tokens     = lt_tokens
          it_statements = lt_statements
      ).
      READ TABLE lt_functions INTO DATA(ls_func) WITH KEY funcname = 'NUMBER_GET_NEXT'.
      CHECK sy-subrc EQ 0.
      READ TABLE ls_func-exporting INTO DATA(ls_exp) WITH KEY name = 'OBJECT'.
      CHECK sy-subrc EQ 0.
      IF strlen( ls_exp-value ) LE 40.
        APPEND ls_exp-value TO lt_snro_check.
      ENDIF.
    ENDLOOP.
    CHECK lt_snro_check[] IS NOT INITIAL.
    SELECT pgmid, object, obj_name FROM tadir
    FOR ALL ENTRIES IN @lt_snro_check
    WHERE pgmid EQ 'R3TR' AND object EQ 'NROB' AND obj_name EQ @lt_snro_check-table_line
    INTO TABLE @DATA(lt_nrob).
    LOOP AT lt_nrob INTO DATA(ls_nrob).
      READ TABLE ct_dependencies TRANSPORTING NO FIELDS WITH KEY tabname = 'TADIR' tabkey = ls_nrob.
      CHECK sy-subrc <> 0.
      APPEND INITIAL LINE TO ct_dependencies ASSIGNING FIELD-SYMBOL(<fs_dep>).
      <fs_dep>-tabname = 'TADIR'.
      <fs_dep>-tabkey = ls_nrob.
    ENDLOOP.
    CHECK 1 EQ 1.
  ENDMETHOD.

  METHOD extract_function_modules.
    DATA lv_param TYPE string.
    DATA(lv_call) = ' '.
    DATA(lv_function) = ' '.
    DATA lt_params TYPE tyt_param.
    FIELD-SYMBOLS <fs_func> TYPE ty_function_module.
    FIELD-SYMBOLS <fs_param> TYPE ty_param.
    FIELD-SYMBOLS <fs_tparam> TYPE tyt_param.
    LOOP AT it_statements INTO DATA(ls_statement).
      UNASSIGN <fs_func>.
      CLEAR lv_param.
      CLEAR lt_params.
      LOOP AT it_tokens INTO DATA(ls_token) FROM ls_statement-from TO ls_statement-to.
        IF <fs_func> IS NOT ASSIGNED.
          IF ls_token-str EQ 'CALL' AND ls_token-type EQ 'I'.
            lv_call = 'X'.
          ELSEIF lv_call EQ 'X' AND ls_token-str EQ 'FUNCTION' AND ls_token-type EQ 'I'.
            lv_function = 'X'.
          ELSEIF lv_call EQ 'X' AND lv_function EQ 'X' AND ls_token-type EQ 'S'.
            APPEND INITIAL LINE TO rt_functions ASSIGNING <fs_func>.
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

ENDCLASS.

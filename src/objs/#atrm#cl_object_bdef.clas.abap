CLASS /atrm/cl_object_bdef DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /atrm/if_object~get_dependencies REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS read_source_tokens
      RETURNING VALUE(rt_tokens) TYPE string_table.
    METHODS append_dependency
      IMPORTING is_dependency  TYPE /atrm/object_dependency
      CHANGING  ct_dependencies TYPE /atrm/object_dependency_t.
ENDCLASS.



CLASS /atrm/cl_object_bdef IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lt_tokens     TYPE string_table,
          lv_token      TYPE string,
          lv_prev1      TYPE string,
          lv_prev2      TYPE string,
          lv_next       TYPE string,
          lv_index      TYPE i,
          ls_dependency TYPE /atrm/object_dependency.

    super->/atrm/if_object~get_dependencies(
      IMPORTING
        dependencies = dependencies
    ).

    " safety handler: Repository Environment reads the where-used index, which
    " can be missing (e.g. objects imported without an index update). Read the
    " behavior definition source instead, so the dependencies are found anyway.
    lt_tokens = read_source_tokens( ).
    LOOP AT lt_tokens INTO lv_token.
      lv_index = sy-tabix.
      CLEAR: lv_prev1, lv_prev2, lv_next, ls_dependency.
      IF lv_index > 1.
        READ TABLE lt_tokens INTO lv_prev1 INDEX lv_index - 1.
      ENDIF.
      IF lv_index > 2.
        READ TABLE lt_tokens INTO lv_prev2 INDEX lv_index - 2.
      ENDIF.
      READ TABLE lt_tokens INTO lv_next INDEX lv_index + 1.
      CHECK lv_next IS NOT INITIAL.
      TRY.
          IF lv_token = 'CLASS' AND lv_prev1 = 'IN' AND lv_prev2 = 'IMPLEMENTATION'.
            " [managed|unmanaged|...] implementation in class <behavior pool>
            ls_dependency = get_tadir_dependency( object = 'CLAS' obj_name = lv_next ).
          ELSEIF lv_token = 'FOR' AND lv_prev1 = 'BEHAVIOR' AND lv_prev2 = 'DEFINE'.
            " define behavior for <CDS entity>
            get_cds_dependency(
              EXPORTING
                entity     = lv_next
              IMPORTING
                dependency = ls_dependency
            ).
          ELSEIF lv_token = 'TABLE' AND ( lv_prev1 = 'PERSISTENT' OR lv_prev1 = 'DRAFT' ).
            " persistent table <table> / draft table <table>
            ls_dependency = get_tadir_dependency( object = 'TABL' obj_name = lv_next ).
          ENDIF.
          append_dependency( EXPORTING is_dependency = ls_dependency CHANGING ct_dependencies = dependencies ).
        CATCH /atrm/cx_exception.
          " referenced object not in TADIR
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD read_source_tokens.
    DATA: lv_name    TYPE string,
          lv_include TYPE program,
          lt_source  TYPE STANDARD TABLE OF string,
          lv_line    TYPE string,
          lv_text    TYPE string,
          lv_start   TYPE i,
          lv_end     TYPE i.

    lv_name = key-obj_name.
    TRY.
        " the behavior definition source is stored in its BD include
        CALL METHOD ('CL_BDEF_NAME_SERVICE')=>('GET_BD_INCLUDE')
          EXPORTING
            i_name    = lv_name
          RECEIVING
            r_include = lv_include.
      CATCH cx_root.
        " behavior definitions not available in this release
        RETURN.
    ENDTRY.
    CHECK lv_include IS NOT INITIAL.
    READ REPORT lv_include INTO lt_source.
    CHECK sy-subrc = 0.

    LOOP AT lt_source INTO lv_line.
      FIND '//' IN lv_line MATCH OFFSET lv_start.
      IF sy-subrc = 0.
        lv_line = lv_line(lv_start).
      ENDIF.
      CONCATENATE lv_text lv_line INTO lv_text SEPARATED BY space.
    ENDLOOP.
    DO.
      FIND '/*' IN lv_text MATCH OFFSET lv_start.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      FIND '*/' IN SECTION OFFSET lv_start OF lv_text MATCH OFFSET lv_end.
      IF sy-subrc <> 0.
        lv_text = lv_text(lv_start).
        EXIT.
      ENDIF.
      lv_end = lv_end + 2.
      CONCATENATE lv_text(lv_start) lv_text+lv_end INTO lv_text SEPARATED BY space.
    ENDDO.

    TRANSLATE lv_text TO UPPER CASE.
    REPLACE ALL OCCURRENCES OF '{' IN lv_text WITH ` `.
    REPLACE ALL OCCURRENCES OF '}' IN lv_text WITH ` `.
    REPLACE ALL OCCURRENCES OF ';' IN lv_text WITH ` `.
    REPLACE ALL OCCURRENCES OF ',' IN lv_text WITH ` `.
    REPLACE ALL OCCURRENCES OF '(' IN lv_text WITH ` `.
    REPLACE ALL OCCURRENCES OF ')' IN lv_text WITH ` `.
    SPLIT lv_text AT space INTO TABLE rt_tokens.
    DELETE rt_tokens WHERE table_line IS INITIAL.
  ENDMETHOD.

  METHOD append_dependency.
    CHECK is_dependency-tabname IS NOT INITIAL.
    READ TABLE ct_dependencies
      WITH KEY tabname = is_dependency-tabname tabkey = is_dependency-tabkey
      TRANSPORTING NO FIELDS.
    CHECK sy-subrc <> 0.
    APPEND is_dependency TO ct_dependencies.
  ENDMETHOD.

ENDCLASS.


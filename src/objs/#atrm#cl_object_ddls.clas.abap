CLASS /atrm/cl_object_ddls DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS /atrm/cl_object_ddls IMPLEMENTATION.
  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name       TYPE string,
          lv_where      TYPE string,
          lv_table      TYPE tabname,
          lv_source     TYPE string,
          lv_target     TYPE string,
          lv_target_esc TYPE string,
          lv_resolved   TYPE abap_bool,
          lt_results    TYPE match_result_tab,
          ls_result     TYPE match_result,
          ls_submatch   TYPE submatch_result,
          lt_targets    TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          lr_rows       TYPE REF TO data,
          ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS: <lt_rows>  TYPE STANDARD TABLE,
                   <ls_row>   TYPE any,
                   <lv_value> TYPE any.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.

    " Active DDL source
    TRY.
        lv_table = 'DDDDLSRC'.
        CONCATENATE `DDLNAME = '` lv_name `' AND AS4LOCAL = 'A'` INTO lv_where.
        CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_rows->* TO <lt_rows>.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).
        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <ls_row> TO <lv_value>.
          IF sy-subrc = 0.
            lv_source = <lv_value>.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        " optional repository table may not exist in the target system
    ENDTRY.
    CHECK lv_source IS NOT INITIAL.

    " Remove comments, normalize case
    REPLACE ALL OCCURRENCES OF PCRE `/\*[\s\S]*?\*/` IN lv_source WITH ` `.
    REPLACE ALL OCCURRENCES OF PCRE `//[^\n]*` IN lv_source WITH ` `.
    TRANSLATE lv_source TO UPPER CASE.

    " Data sources, joins, association and composition targets
    FIND ALL OCCURRENCES OF PCRE `\b(?:FROM|JOIN|TO|OF)\s+(?:PARENT\s+)?([/A-Z0-9_]+)`
      IN lv_source RESULTS lt_results.
    LOOP AT lt_results INTO ls_result.
      READ TABLE ls_result-submatches INTO ls_submatch INDEX 1.
      CHECK sy-subrc = 0 AND ls_submatch-length > 0.
      lv_target = lv_source+ls_submatch-offset(ls_submatch-length).
      CHECK lv_target <> me->key-obj_name.
      COLLECT lv_target INTO lt_targets.
    ENDLOOP.

    LOOP AT lt_targets INTO lv_target.
      lv_target_esc = lv_target.
      REPLACE ALL OCCURRENCES OF `'` IN lv_target_esc WITH `''`.
      lv_resolved = abap_false.

      " CDS entity -> owning DDL source
      TRY.
          lv_table = 'DDLDEPENDENCY'.
          CONCATENATE `OBJECTNAME = '` lv_target_esc `' AND STATE = 'A'` INTO lv_where.
          CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_table).
          ASSIGN lr_rows->* TO <lt_rows>.
          SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).
          LOOP AT <lt_rows> ASSIGNING <ls_row>.
            ASSIGN COMPONENT 'DDLNAME' OF STRUCTURE <ls_row> TO <lv_value>.
            CHECK sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'DDLS' obj_name = <lv_value>
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
                lv_resolved = abap_true.
              CATCH cx_root.
            ENDTRY.
          ENDLOOP.
        CATCH cx_root.
      ENDTRY.
      CHECK lv_resolved = abap_false.

      " Dictionary table or view
      TRY.
          CLEAR ls_dependency.
          CALL METHOD get_tadir_dependency
            EXPORTING object = 'TABL' obj_name = lv_target
            RECEIVING dependency = ls_dependency.
          APPEND ls_dependency TO dependencies.
        CATCH cx_root.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'VIEW' obj_name = lv_target
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " not a repository data source (e.g. alias or keyword match)
          ENDTRY.
      ENDTRY.
    ENDLOOP.

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.
ENDCLASS.

CLASS /atrm/cl_object_tobj DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_tobj IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_table_name TYPE tabname,
      lv_full       TYPE string,
      lv_len        TYPE i,
      lv_off        TYPE i,
      lv_type       TYPE c LENGTH 1,
      lv_objname    TYPE string,
      lv_name       TYPE string,
      lv_where      TYPE string,
      lv_func_where TYPE string,
      lv_table      TYPE tabname,
      lv_target     TYPE trobjtype,
      lv_docuid     TYPE string,
      lv_dsys       TYPE string,
      lv_func       TYPE string,
      lr_rows       TYPE REF TO data,
      ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_rows>  TYPE STANDARD TABLE,
      <ls_row>   TYPE any,
      <lv_pgmid> TYPE any,
      <lv_tobj>  TYPE any,
      <lv_tname> TYPE any,
      <lv_value> TYPE any.

    " Legacy behavior: object name prefix as table
    TRY.
        lv_table_name = me->key-obj_name(10).
        CONDENSE lv_table_name.
        IF lv_table_name IS NOT INITIAL.
          CALL METHOD get_tadir_dependency
            EXPORTING object = 'TABL' obj_name = lv_table_name
            RECEIVING dependency = ls_dependency.
          APPEND ls_dependency TO dependencies.
        ENDIF.
      CATCH cx_root.
        " optional dependency may not exist in the target system
    ENDTRY.

    " TADIR name = OBJH-OBJECTNAME (padded to 10) + OBJH-OBJECTTYPE
    lv_full = me->key-obj_name.
    lv_len = strlen( lv_full ).
    IF lv_len > 1.
      lv_len = lv_len - 1.
      lv_type = lv_full+lv_len(1).
      lv_objname = lv_full(lv_len).
      CONDENSE lv_objname.
      lv_name = lv_objname.
      REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.
      CONCATENATE `OBJECTNAME = '` lv_name `' AND OBJECTTYPE = '` lv_type `'` INTO lv_where.

      " Object itself by type
      CASE lv_type.
        WHEN 'S'. lv_target = 'TABL'.
        WHEN 'V'. lv_target = 'VIEW'.
        WHEN 'C'. lv_target = 'VCLS'.
        WHEN 'T'. lv_target = 'TRAN'.
        WHEN OTHERS. CLEAR lv_target.
      ENDCASE.
      IF lv_target IS NOT INITIAL.
        TRY.
            CLEAR ls_dependency.
            CALL METHOD get_tadir_dependency
              EXPORTING object = lv_target obj_name = lv_objname
              RECEIVING dependency = ls_dependency.
            APPEND ls_dependency TO dependencies.
          CATCH cx_root.
            " optional dependency may not exist in the target system
        ENDTRY.
      ENDIF.

      " Member tables
      append_table_dependencies(
        EXPORTING table_name   = 'OBJS'
                  where_clause = lv_where
                  object_field = 'TABNAME'
                  object_type  = 'TABL'
        CHANGING  dependencies = dependencies ).

      " Generated maintenance dialog (tables and views)
      IF lv_type = 'S' OR lv_type = 'V'.
        CONCATENATE `TABNAME = '` lv_name `'` INTO lv_func_where.
        append_table_dependencies(
          EXPORTING table_name   = 'TVDIR'
                    where_clause = lv_func_where
                    object_field = 'AREA'
                    object_type  = 'FUGR'
          CHANGING  dependencies = dependencies ).
      ENDIF.

      " Object methods: function module and its function group
      append_table_dependencies(
        EXPORTING table_name   = 'OBJM'
                  where_clause = lv_where
                  object_field = 'METHODNAME'
                  object_type  = 'FUNC'
        CHANGING  dependencies = dependencies ).
      TRY.
          lv_table = 'OBJM'.
          CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_table).
          ASSIGN lr_rows->* TO <lt_rows>.
          SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).
          LOOP AT <lt_rows> ASSIGNING <ls_row>.
            ASSIGN COMPONENT 'METHODNAME' OF STRUCTURE <ls_row> TO <lv_value>.
            CHECK sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
            lv_func = <lv_value>.
            REPLACE ALL OCCURRENCES OF `'` IN lv_func WITH `''`.
            CONCATENATE `FUNCNAME = '` lv_func `'` INTO lv_func_where.
            append_table_dependencies(
              EXPORTING table_name   = 'ENLFDIR'
                        where_clause = lv_func_where
                        object_field = 'AREA'
                        object_type  = 'FUGR'
              CHANGING  dependencies = dependencies ).
          ENDLOOP.
        CATCH cx_root.
          " optional repository table may not exist in the target system
      ENDTRY.

      " Piece list entries
      TRY.
          lv_table = 'OBJSL'.
          CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_table).
          ASSIGN lr_rows->* TO <lt_rows>.
          SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).
          LOOP AT <lt_rows> ASSIGNING <ls_row>.
            ASSIGN COMPONENT 'TPGMID' OF STRUCTURE <ls_row> TO <lv_pgmid>.
            CHECK sy-subrc = 0 AND <lv_pgmid> = 'R3TR'.
            ASSIGN COMPONENT 'TOBJECT' OF STRUCTURE <ls_row> TO <lv_tobj>.
            CHECK sy-subrc = 0 AND <lv_tobj> IS NOT INITIAL.
            ASSIGN COMPONENT 'TOBJ_NAME' OF STRUCTURE <ls_row> TO <lv_tname>.
            CHECK sy-subrc = 0 AND <lv_tname> IS NOT INITIAL.
            CASE <lv_tobj>.
              WHEN 'TABU' OR 'TDAT'. lv_target = 'TABL'.
              WHEN 'VDAT'. lv_target = 'VIEW'.
              WHEN 'CDAT'. lv_target = 'VCLS'.
              WHEN OTHERS. lv_target = <lv_tobj>.
            ENDCASE.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = lv_target obj_name = <lv_tname>
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDLOOP.
        CATCH cx_root.
          " optional repository table may not exist in the target system
      ENDTRY.

      " Multiclient-compliance document (class MCLI, transported as DSYS)
      TRY.
          lv_table = 'OBJH'.
          CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_table).
          ASSIGN lr_rows->* TO <lt_rows>.
          SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).
          LOOP AT <lt_rows> ASSIGNING <ls_row>.
            ASSIGN COMPONENT 'MCDOCUID' OF STRUCTURE <ls_row> TO <lv_value>.
            CHECK sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
            lv_docuid = <lv_value>.
            CLEAR lv_off.
            IF lv_docuid(1) = '/'.
              FIND FIRST OCCURRENCE OF '/' IN SECTION OFFSET 1 OF lv_docuid
                MATCH OFFSET lv_off.
            ENDIF.
            IF lv_off > 0.
              lv_off = lv_off + 1.
              CONCATENATE lv_docuid(lv_off) 'MCLI' lv_docuid+lv_off INTO lv_dsys.
            ELSE.
              CONCATENATE 'MCLI' lv_docuid INTO lv_dsys.
            ENDIF.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'DSYS' obj_name = lv_dsys
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDLOOP.
        CATCH cx_root.
          " optional repository table may not exist in the target system
      ENDTRY.
    ENDIF.

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.

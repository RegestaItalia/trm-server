CLASS /atrm/cl_object_vcls DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS /atrm/cl_object_vcls IMPLEMENTATION.
  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name       TYPE string,
          lv_where      TYPE string,
          lv_tobj       TYPE c LENGTH 40,
          lv_len        TYPE i,
          ls_dependency TYPE /atrm/object_dependency.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.
    CONCATENATE `VCLNAME = '` lv_name `'` INTO lv_where.

    " Cluster members (tables or maintenance views) and member switches
    append_table_dependencies(
      EXPORTING table_name   = 'VCLSTRUC'
                where_clause = lv_where
                object_field = 'OBJECT'
                object_type  = 'TABL'
      CHANGING  dependencies = dependencies ).
    append_table_dependencies(
      EXPORTING table_name   = 'VCLSTRUC'
                where_clause = lv_where
                object_field = 'OBJECT'
                object_type  = 'VIEW'
      CHANGING  dependencies = dependencies ).
    append_table_dependencies(
      EXPORTING table_name   = 'VCLSTRUC'
                where_clause = lv_where
                object_field = 'SWITCH_ID'
                object_type  = 'SFSW'
      CHANGING  dependencies = dependencies ).

    " Event FORM routine program and base view cluster
    append_table_dependencies(
      EXPORTING table_name   = 'VCLDIR'
                where_clause = lv_where
                object_field = 'EXITPROG'
                object_type  = 'PROG'
      CHANGING  dependencies = dependencies ).
    append_table_dependencies(
      EXPORTING table_name   = 'VCLDIR'
                where_clause = lv_where
                object_field = 'BASEVCL'
                object_type  = 'VCLS'
      CHANGING  dependencies = dependencies ).

    " Generated transport object: name padded to 10 characters plus type C
    lv_tobj = me->key-obj_name.
    lv_len = strlen( lv_tobj ).
    IF lv_len < 10.
      lv_len = 10.
    ENDIF.
    IF lv_len < 40.
      lv_tobj+lv_len(1) = 'C'.
      TRY.
          CALL METHOD get_tadir_dependency
            EXPORTING object = 'TOBJ' obj_name = lv_tobj
            RECEIVING dependency = ls_dependency.
          APPEND ls_dependency TO dependencies.
        CATCH cx_root.
          " transport object may not exist
      ENDTRY.
    ENDIF.

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.
ENDCLASS.

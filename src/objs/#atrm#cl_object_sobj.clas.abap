CLASS /atrm/cl_object_sobj DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_sobj IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name    TYPE string,
      lv_where   TYPE string,
      lv_table   TYPE tabname,
      lt_fnames  TYPE STANDARD TABLE OF rs38l_fnam WITH DEFAULT KEY,
      lv_fname   TYPE rs38l_fnam,
      lv_fescape TYPE string.

    super->/atrm/if_object~get_dependencies(
      IMPORTING dependencies = dependencies ).

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.

    " Key fields and attributes: DDIC reference table and referenced object type
    CONCATENATE `OBJTYPE = '` lv_name `' AND REFSTRUCT <> ' '` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'SWOTDV'
                where_clause = lv_where
                object_field = 'REFSTRUCT'
                object_type  = 'TABL'
      CHANGING  dependencies = dependencies ).
    CONCATENATE `OBJTYPE = '` lv_name `' AND REFOBJTYPE <> ' '` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'SWOTDV'
                where_clause = lv_where
                object_field = 'REFOBJTYPE'
                object_type  = 'SOBJ'
      CHANGING  dependencies = dependencies ).

    " Method parameters/exceptions: reference table, object type, message class
    CONCATENATE `OBJTYPE = '` lv_name `' AND REFSTRUCT <> ' '` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'SWOTDQ'
                where_clause = lv_where
                object_field = 'REFSTRUCT'
                object_type  = 'TABL'
      CHANGING  dependencies = dependencies ).
    CONCATENATE `OBJTYPE = '` lv_name `' AND REFOBJTYPE <> ' '` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'SWOTDQ'
                where_clause = lv_where
                object_field = 'REFOBJTYPE'
                object_type  = 'SOBJ'
      CHANGING  dependencies = dependencies ).
    CONCATENATE `OBJTYPE = '` lv_name `' AND ARBGB <> ' '` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'SWOTDQ'
                where_clause = lv_where
                object_field = 'ARBGB'
                object_type  = 'MSAG'
      CHANGING  dependencies = dependencies ).

    " Implemented interface types
    CONCATENATE `OBJTYPE = '` lv_name `'` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'SWOTDI'
                where_clause = lv_where
                object_field = 'INTERFACE'
                object_type  = 'SOBJ'
      CHANGING  dependencies = dependencies ).

    " Methods implemented by a function module: module and function group
    TRY.
        lv_table = 'SWOTDV'.
        CONCATENATE `OBJTYPE = '` lv_name `' AND ABAPTYPE = 'F' AND ABAPNAME <> ' '` INTO lv_where.
        SELECT ('ABAPNAME') FROM (lv_table) INTO TABLE lt_fnames WHERE (lv_where).
      CATCH cx_root.
        CLEAR lt_fnames.
    ENDTRY.
    LOOP AT lt_fnames INTO lv_fname.
      lv_fescape = lv_fname.
      REPLACE ALL OCCURRENCES OF `'` IN lv_fescape WITH `''`.
      CONCATENATE `FUNCNAME = '` lv_fescape `'` INTO lv_where.
      append_table_dependencies(
        EXPORTING table_name   = 'V_FDIR'
                  where_clause = lv_where
                  object_field = 'AREA'
                  object_type  = 'FUGR'
        CHANGING  dependencies = dependencies ).
      append_table_dependencies(
        EXPORTING table_name   = 'V_FDIR'
                  where_clause = lv_where
                  object_field = 'FUNCNAME'
                  object_type  = 'FUNC'
        CHANGING  dependencies = dependencies ).
    ENDLOOP.

    DELETE dependencies WHERE tabname = 'TADIR' AND tabkey = 'R3TRSOBJ' && me->key-obj_name.
    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.

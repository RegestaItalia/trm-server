CLASS /atrm/cl_object_webi DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_webi IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name    TYPE string,
      lv_where   TYPE string,
      lv_table   TYPE tabname,
      lt_fnames  TYPE STANDARD TABLE OF rs38l_fnam WITH DEFAULT KEY,
      lv_fname   TYPE rs38l_fnam,
      lv_fescape TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.

    " Endpoint implemented by a function group
    CONCATENATE `VEPNAME = '` lv_name `' AND VERSION = 'A' AND ENDPOINTTYPE = 'FUGR'` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'VEPENDPOINT'
                where_clause = lv_where
                object_field = 'ENDPOINTNAME'
                object_type  = 'FUGR'
      CHANGING  dependencies = dependencies ).

    " Endpoint implemented by a function module: function group
    TRY.
        lv_table = 'VEPENDPOINT'.
        CONCATENATE `VEPNAME = '` lv_name `' AND VERSION = 'A' AND ENDPOINTTYPE = 'FUNC'` INTO lv_where.
        SELECT ('ENDPOINTNAME') FROM (lv_table) INTO TABLE lt_fnames WHERE (lv_where).
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
    ENDLOOP.

    " Cross references: function groups and BOR object types (BAPI)
    CONCATENATE `VEPNAME = '` lv_name `' AND OBJ_TYPE = 'FUGR'` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'VEPCROSSREF'
                where_clause = lv_where
                object_field = 'OBJ_NAME'
                object_type  = 'FUGR'
      CHANGING  dependencies = dependencies ).
    CONCATENATE `VEPNAME = '` lv_name `' AND OBJ_TYPE = 'BAPI'` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'VEPCROSSREF'
                where_clause = lv_where
                object_field = 'OBJ_NAME'
                object_type  = 'SOBJ'
      CHANGING  dependencies = dependencies ).

    " Interface types: data elements, structures, table types
    CONCATENATE `VEPNAME = '` lv_name `' AND VERSION = 'A' AND TYPE = 'E'` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'VEPTYPE'
                where_clause = lv_where
                object_field = 'TYPENAME'
                object_type  = 'DTEL'
      CHANGING  dependencies = dependencies ).
    CONCATENATE `VEPNAME = '` lv_name `' AND VERSION = 'A' AND TYPE = 'S'` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'VEPTYPE'
                where_clause = lv_where
                object_field = 'TYPENAME'
                object_type  = 'TABL'
      CHANGING  dependencies = dependencies ).
    CONCATENATE `VEPNAME = '` lv_name `' AND VERSION = 'A' AND TYPE = 'T'` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'VEPTYPE'
                where_clause = lv_where
                object_field = 'TYPENAME'
                object_type  = 'TTYP'
      CHANGING  dependencies = dependencies ).

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.

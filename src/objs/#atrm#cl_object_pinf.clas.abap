CLASS /atrm/cl_object_pinf DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_pinf IMPLEMENTATION.

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

    " Exposed function modules: function group
    TRY.
        lv_table = 'IFOBJSHORT'.
        CONCATENATE `INTF_NAME = '` lv_name `' AND ELEM_TYPE = 'FUNC'` INTO lv_where.
        SELECT ('ELEM_KEY') FROM (lv_table) INTO TABLE lt_fnames WHERE (lv_where).
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

    " Replacement objects of deprecated elements
    CONCATENATE `INTF_NAME = '` lv_name `'` INTO lv_where.
    append_typed_dependencies(
      EXPORTING table_name        = 'IFOBJSHORT'
                where_clause      = lv_where
                object_field      = 'REPL_OBJECT_NAME'
                object_type_field = 'REPL_OBJECT_TYPE'
      CHANGING  dependencies      = dependencies ).

    " Use access: permitted client packages
    append_table_dependencies(
      EXPORTING table_name   = 'ACLPERMIS'
                where_clause = lv_where
                object_field = 'CLIENT_PAK'
                object_type  = 'DEVC'
      CHANGING  dependencies = dependencies ).

    DELETE dependencies WHERE tabname = 'TADIR' AND tabkey = 'R3TRPINF' && me->key-obj_name.
    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.

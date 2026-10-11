CLASS /atrm/cl_object_ecsp DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_ecsp IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name  TYPE sobj_name,
      lv_where TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'ECSP_PAR'
        where_clause = lv_where
        object_field = 'PDOM'
        object_type  = 'DOMA'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'ECSP_PAR'
        where_clause = lv_where
        object_field = 'PREF_NAME'
        object_type  = 'ENTY'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'ECSP_PAR'
        where_clause = lv_where
        object_field = 'PREF_NAME'
        object_type  = 'DTEL'
      CHANGING
        dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.

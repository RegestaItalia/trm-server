CLASS /atrm/cl_object_drul DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_drul IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name  TYPE sobj_name,
      lv_where TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    CONCATENATE 'NAME = ''' lv_name ''''
      ' AND AS4LOCAL = ''A''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'DDDRUL_PREDICATE'
        where_clause = lv_where
        object_field = 'BASEOBJ_NAME'
        object_type  = 'ENTY'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name ''''
      ' AND AS4LOCAL = ''A''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'DDDRUL_PREDICATE'
        where_clause = lv_where
        object_field = 'ROLLNAME'
        object_type  = 'DTEL'
      CHANGING
        dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.

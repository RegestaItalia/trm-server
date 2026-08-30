CLASS /atrm/cl_object_iwom DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_iwom IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name  TYPE sobj_name,
      lv_where TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'MODEL_IDENTIFIER = ''' lv_name '''' INTO lv_where.

    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = '/IWFND/I_MED_REF'
        where_clause = lv_where
        object_field = 'TARGET_MDL_IDENT'
        object_type  = 'IWOM'
      CHANGING
        dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.

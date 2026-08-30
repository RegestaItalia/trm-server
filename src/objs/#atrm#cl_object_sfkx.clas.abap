CLASS /atrm/cl_object_sfkx DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_sfkx IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name TYPE sobj_name, lv_where TYPE string.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'EXCEPTION_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCMI_FK_EXCEPT' where_clause = lv_where object_field = 'TABNAME' object_type = 'TABL' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCMI_FK_EXCEPT' where_clause = lv_where object_field = 'FORTAB' object_type = 'TABL' CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.

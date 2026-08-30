CLASS /atrm/cl_object_fdt0 DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_fdt0 IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lr_row          TYPE REF TO data,
      lr_objects      TYPE REF TO data,
      lv_table        TYPE tabname,
      lv_name         TYPE sobj_name,
      lv_application  TYPE string,
      lv_object_id    TYPE string,
      lv_where        TYPE string.

    FIELD-SYMBOLS:
      <ls_row>        TYPE any,
      <lt_objects>    TYPE STANDARD TABLE,
      <ls_object>     TYPE any,
      <lv_value>      TYPE any.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    TRY.
        lv_table = 'FDT_APPL_TADIR'.
        CREATE DATA lr_row TYPE (lv_table).
        ASSIGN lr_row->* TO <ls_row>.
        CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
        SELECT SINGLE * FROM (lv_table) INTO <ls_row> WHERE (lv_where).
        CHECK sy-subrc = 0.

        ASSIGN COMPONENT 'ID' OF STRUCTURE <ls_row> TO <lv_value>.
        CHECK sy-subrc = 0.
        lv_application = <lv_value>.
        REPLACE ALL OCCURRENCES OF '''' IN lv_application WITH ''''''.
        CONCATENATE 'APPLICATION_ID = ''' lv_application ''''
          INTO lv_where.

        CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_ADMN_0010' where_clause = lv_where object_field = 'TEXT_EXIT_CL' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_ADMN_0010' where_clause = lv_where object_field = 'DOCU_EXIT_CL' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_ADMN_0010' where_clause = lv_where object_field = 'TXTSY_PROGNAME' object_type = 'PROG' CHANGING dependencies = dependencies.

        CLEAR lv_where.
        CONCATENATE 'ID = ''' lv_application '''' INTO lv_where.
        CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_APPL_0000' where_clause = lv_where object_field = 'PROB_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_APPL_0000' where_clause = lv_where object_field = 'SETTINGS_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.

        lv_table = 'FDT_ADMN_0000'.
        CREATE DATA lr_objects TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_objects->* TO <lt_objects>.
        CONCATENATE 'APPLICATION_ID = ''' lv_application ''''
          INTO lv_where.
        SELECT * FROM (lv_table)
          INTO TABLE <lt_objects>
          WHERE (lv_where).

        LOOP AT <lt_objects> ASSIGNING <ls_object>.
          ASSIGN COMPONENT 'ID' OF STRUCTURE <ls_object> TO <lv_value>.
          CHECK sy-subrc = 0.
          CHECK <lv_value> IS NOT INITIAL.
          lv_object_id = <lv_value>.
          REPLACE ALL OCCURRENCES OF '''' IN lv_object_id WITH ''''''.
          CONCATENATE 'ID = ''' lv_object_id '''' INTO lv_where.

          CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_ACTN_1100' where_clause = lv_where object_field = 'CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
          CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_EXPR_1401' where_clause = lv_where object_field = 'CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
          CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_EXPR_1500' where_clause = lv_where object_field = 'CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
          CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_EXTY_0000' where_clause = lv_where object_field = 'CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
          CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_EXTY_0000' where_clause = lv_where object_field = 'QUERY_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
          CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_EXTY_0000' where_clause = lv_where object_field = 'UI_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
          CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_EXTY_0000' where_clause = lv_where object_field = 'DATA_XCHNG_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
          CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_CTLG_0000' where_clause = lv_where object_field = 'EXIT_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
          CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_EXPR_1921' where_clause = lv_where object_field = 'BRF_APPLCLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
          CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_EXPR_1971' where_clause = lv_where object_field = 'BRF_APPLCLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
          CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_FNCT_0100' where_clause = lv_where object_field = 'CONTEXT_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
          CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_EXPR_2200' where_clause = lv_where object_field = 'TABLE_NAME' object_type = 'TABL' CHANGING dependencies = dependencies.
          CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_EXTY_0200' where_clause = lv_where object_field = 'TABLE_TYPE' object_type = 'TTYP' CHANGING dependencies = dependencies.
          CALL METHOD append_table_dependencies EXPORTING table_name = 'FDT_ADMN_0203' where_clause = lv_where object_field = 'TABLE_TYPE' object_type = 'TTYP' CHANGING dependencies = dependencies.
        ENDLOOP.
      CATCH cx_root.
        " BRFplus APIs may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

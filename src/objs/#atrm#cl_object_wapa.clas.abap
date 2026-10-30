CLASS /atrm/cl_object_wapa DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_wapa IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_appl       TYPE c LENGTH 30,
      lv_name       TYPE string,
      lv_where      TYPE string,
      lv_table      TYPE tabname,
      lr_rows       TYPE REF TO data,
      ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_rows>  TYPE STANDARD TABLE,
      <ls_row>   TYPE any,
      <lv_value> TYPE any.

    super->/atrm/if_object~get_dependencies(
      IMPORTING dependencies = dependencies
    ).

    lv_appl = me->key-obj_name.
    lv_name = lv_appl.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    " Application class and theme
    CONCATENATE 'APPLNAME = ''' lv_name ''' AND VERSION = ''A''' INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'O2APPL'
                where_clause = lv_where
                object_field = 'APPLCLAS'
                object_type  = 'CLAS'
      CHANGING  dependencies = dependencies ).
    append_table_dependencies(
      EXPORTING table_name   = 'O2APPL'
                where_clause = lv_where
                object_field = 'THEME'
                object_type  = 'WTHM'
      CHANGING  dependencies = dependencies ).

    " Controller classes
    CONCATENATE 'APPLNAME = ''' lv_name ''' AND PAGETYPE = ''C''' INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'O2PAGDIR'
                where_clause = lv_where
                object_field = 'IMPLCLASS'
                object_type  = 'CLAS'
      CHANGING  dependencies = dependencies ).

    " BSP extensions used by pages
    CONCATENATE 'APPLNAME = ''' lv_name '''' INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'O2PAGTLIB'
                where_clause = lv_where
                object_field = 'TLIBID'
                object_type  = 'WTAG'
      CHANGING  dependencies = dependencies ).

    " Page fragments included from other applications
    CONCATENATE 'MASTERAPPL = ''' lv_name ''' AND INCLAPPL <> ''' lv_name '''' INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'O2PAGINC'
                where_clause = lv_where
                object_field = 'INCLAPPL'
                object_type  = 'WAPA'
      CHANGING  dependencies = dependencies ).

    " OTR texts used by pages: the SOTR object is the package of the text
    TRY.
        lv_table = 'SOTR_USE'.
        CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_rows->* TO <lt_rows>.
        CONCATENATE 'PGMID = ''LIMU'' AND OBJECT = ''WAPP'' AND OBJ_NAME LIKE ''' lv_appl '%''' INTO lv_where RESPECTING BLANKS.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).
        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'CONCEPT' OF STRUCTURE <ls_row> TO <lv_value>.
          CHECK sy-subrc = 0.
          CHECK <lv_value> IS NOT INITIAL.
          CONCATENATE 'CONCEPT = ''' <lv_value> '''' INTO lv_where.
          append_table_dependencies(
            EXPORTING table_name   = 'SOTR_HEAD'
                      where_clause = lv_where
                      object_field = 'PAKET'
                      object_type  = 'SOTR'
            CHANGING  dependencies = dependencies ).
        ENDLOOP.
      CATCH cx_root.
        " optional repository table may not exist in the target system
    ENDTRY.

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.

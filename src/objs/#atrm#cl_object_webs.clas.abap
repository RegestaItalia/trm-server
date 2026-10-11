CLASS /atrm/cl_object_webs DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_webs IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lr_uses       TYPE REF TO data,
      lr_head       TYPE REF TO data,
      lv_table      TYPE tabname,
      lv_name       TYPE sobj_name,
      lv_where      TYPE string,
      lv_concept    TYPE string,
      ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_uses>    TYPE STANDARD TABLE,
      <ls_use>     TYPE any,
      <ls_head>    TYPE any,
      <lv_concept> TYPE any,
      <lv_package> TYPE any.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'WSNAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING table_name = 'WSHEADER' where_clause = lv_where
                object_field = 'VINAME' object_type = 'WEBI'
      CHANGING dependencies = dependencies.

    TRY.
        lv_table = 'SOTR_USE'.
        CREATE DATA lr_uses TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_uses->* TO <lt_uses>.
        CONCATENATE 'PGMID = ''R3TR'' AND OBJECT = ''WEBS'' AND OBJ_NAME = ''' lv_name '''' INTO lv_where.
        SELECT * FROM (lv_table) INTO TABLE <lt_uses> WHERE (lv_where).

        LOOP AT <lt_uses> ASSIGNING <ls_use>.
          ASSIGN COMPONENT 'CONCEPT' OF STRUCTURE <ls_use> TO <lv_concept>.
          CHECK sy-subrc = 0.
          CHECK <lv_concept> IS NOT INITIAL.
          lv_concept = <lv_concept>.
          REPLACE ALL OCCURRENCES OF '''' IN lv_concept WITH ''''''.

          lv_table = 'SOTR_HEAD'.
          CREATE DATA lr_head TYPE (lv_table).
          ASSIGN lr_head->* TO <ls_head>.
          CONCATENATE 'CONCEPT = ''' lv_concept '''' INTO lv_where.
          SELECT SINGLE * FROM (lv_table) INTO <ls_head> WHERE (lv_where).
          CHECK sy-subrc = 0.
          ASSIGN COMPONENT 'PAKET' OF STRUCTURE <ls_head> TO <lv_package>.
          CHECK sy-subrc = 0.
          CHECK <lv_package> IS NOT INITIAL.

          CLEAR ls_dependency.
          CALL METHOD get_tadir_dependency
            EXPORTING object = 'SOTR' obj_name = <lv_package>
            RECEIVING dependency = ls_dependency.
          IF ls_dependency IS NOT INITIAL.
            APPEND ls_dependency TO dependencies.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        " optional OTR repository tables may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

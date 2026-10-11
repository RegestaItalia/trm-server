CLASS /atrm/cl_object_ccfg DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_ccfg IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lr_table       TYPE REF TO data,
      lv_table       TYPE tabname,
      lv_where       TYPE string,
      lv_name        TYPE sobj_name,
      lv_dep_name    TYPE sobj_name,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_rows>       TYPE STANDARD TABLE,
      <ls_row>        TYPE any,
      <lv_aspect>     TYPE any,
      <lv_value>      TYPE any,
      <lv_parent>     TYPE any.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    TRY.
        lv_table = 'SCCD_CONFIG'.
        CREATE DATA lr_table TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_table->* TO <lt_rows>.
        CONCATENATE 'CONFIG_ID = ''' lv_name '''' INTO lv_where.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).

        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'PARENT_CONFIG_ID' OF STRUCTURE <ls_row> TO <lv_parent>.
          IF sy-subrc = 0 AND <lv_parent> IS NOT INITIAL.
            TRY.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'CCFG' obj_name = <lv_parent>
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
        ENDLOOP.

        CLEAR <lt_rows>.
        lv_table = 'SCCD_CFG_ELEM'.
        CREATE DATA lr_table TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_table->* TO <lt_rows>.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).

        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'ASPECT_ID' OF STRUCTURE <ls_row> TO <lv_aspect>.
          ASSIGN COMPONENT 'ASPECT_VALUE' OF STRUCTURE <ls_row> TO <lv_value>.
          IF sy-subrc = 0 AND <lv_aspect> IS NOT INITIAL
            AND <lv_value> IS NOT INITIAL.
            CLEAR lv_dep_name.
            lv_dep_name = <lv_aspect>.
            lv_dep_name+30 = <lv_value>.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'CASV' obj_name = lv_dep_name
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        " optional check-configuration API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

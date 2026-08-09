CLASS /atrm/cl_object_bccu DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_bccu IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lr_table       TYPE REF TO data,
      lv_table       TYPE tabname,
      lv_where       TYPE string,
      lv_name        TYPE sobj_name,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_rows>       TYPE STANDARD TABLE,
      <ls_row>        TYPE any,
      <lv_value>      TYPE any.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    TRY.
        lv_table = 'DFIT_CHECK_UNIT'.
        CREATE DATA lr_table TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_table->* TO <lt_rows>.
        CONCATENATE 'CHECK_UNIT_ID = ''' lv_name '''' INTO lv_where.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).

        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'CLASSNAME' OF STRUCTURE <ls_row> TO <lv_value>.
          IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
            TRY.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'CLAS' obj_name = <lv_value>
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.

          UNASSIGN <lv_value>.
          ASSIGN COMPONENT 'DOCUMENTATION' OF STRUCTURE <ls_row> TO <lv_value>.
          IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'DOCV' obj_name = <lv_value>
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
        ENDLOOP.

        CLEAR <lt_rows>.
        lv_table = 'DFIT_UNIT_INCL'.
        CREATE DATA lr_table TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_table->* TO <lt_rows>.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).

        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'PRE_CHECK_ID' OF STRUCTURE <ls_row> TO <lv_value>.
          IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'BCCU' obj_name = <lv_value>
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        " optional business configuration unit API may not exist
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

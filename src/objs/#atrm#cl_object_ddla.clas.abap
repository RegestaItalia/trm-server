CLASS /atrm/cl_object_ddla DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_ddla IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lr_table       TYPE REF TO data,
      lv_table       TYPE tabname,
      lv_where       TYPE string,
      lv_name        TYPE sobj_name,
      lv_definition  TYPE sobj_name,
      lv_dummy       TYPE string,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_rows>       TYPE STANDARD TABLE,
      <ls_row>        TYPE any,
      <lv_key>        TYPE any.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    TRY.
        lv_table = 'DDLA_RT_ANNOS'.
        CREATE DATA lr_table TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_table->* TO <lt_rows>.
        CONCATENATE 'DDLANAME = ''' lv_name '''' INTO lv_where.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).

        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'DEFINITION_KEY' OF STRUCTURE <ls_row> TO <lv_key>.
          CHECK sy-subrc = 0.
          CHECK <lv_key> IS NOT INITIAL.

          CLEAR: lv_definition, lv_dummy.
          SPLIT <lv_key> AT '.' INTO lv_definition lv_dummy.
          SPLIT lv_definition AT '$' INTO lv_definition lv_dummy.
          TRANSLATE lv_definition TO UPPER CASE.
          CHECK lv_definition IS NOT INITIAL.
          CHECK lv_definition <> me->key-obj_name.

          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'DDLA' obj_name = lv_definition
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional annotation definition may not exist
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " optional annotation-definition API may not exist
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

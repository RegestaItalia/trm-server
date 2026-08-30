CLASS /atrm/cl_object_styc DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_styc IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lr_table      TYPE REF TO data,
      lv_table      TYPE tabname,
      lv_name       TYPE sobj_name,
      lv_where      TYPE string,
      ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_rows> TYPE STANDARD TABLE,
      <ls_row>  TYPE any,
      <lv_cds>  TYPE any.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'ID = ''' lv_name '''' INTO lv_where.

    TRY.
        lv_table = '/SSB/SACMOD_S'.
        CREATE DATA lr_table TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_table->* TO <lt_rows>.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).

        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'CDSVIEW' OF STRUCTURE <ls_row> TO <lv_cds>.
          CHECK sy-subrc = 0.
          CHECK <lv_cds> IS NOT INITIAL.
          CLEAR ls_dependency.
          CALL METHOD get_cds_dependency
            EXPORTING entity = <lv_cds>
            IMPORTING dependency = ls_dependency.
          IF ls_dependency IS NOT INITIAL.
            APPEND ls_dependency TO dependencies.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        " optional SAC repository tables may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

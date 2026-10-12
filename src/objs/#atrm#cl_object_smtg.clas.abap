CLASS /atrm/cl_object_smtg DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_smtg IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lr_header      TYPE REF TO data,
      lv_table_name  TYPE tabname,
      lv_where       TYPE string,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <ls_header> TYPE any,
      <lv_value>  TYPE any.

    TRY.
        lv_table_name = 'SMTG_TMPL_HDR'.
        CREATE DATA lr_header TYPE (lv_table_name).
        ASSIGN lr_header->* TO <ls_header>.
        CONCATENATE `ID = '` me->key-obj_name
          `' AND VERSION = 'A'` INTO lv_where.
        SELECT SINGLE * FROM (lv_table_name)
          INTO <ls_header>
          WHERE (lv_where).
        CHECK sy-subrc = 0.

        ASSIGN COMPONENT 'CDS_VIEW' OF STRUCTURE <ls_header> TO <lv_value>.
        IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_cds_dependency
                EXPORTING entity = <lv_value>
                IMPORTING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        UNASSIGN <lv_value>.
        ASSIGN COMPONENT 'PARENT_ID' OF STRUCTURE <ls_header> TO <lv_value>.
        IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'SMTG' obj_name = <lv_value>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        UNASSIGN <lv_value>.
        ASSIGN COMPONENT 'ORIGINAL_ID' OF STRUCTURE <ls_header> TO <lv_value>.
        IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'SMTG' obj_name = <lv_value>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.
      CATCH cx_root.
        " optional email template API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

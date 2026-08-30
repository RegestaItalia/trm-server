CLASS /atrm/cl_object_iaxu DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_iaxu IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lr_name        TYPE REF TO data,
      lr_attr        TYPE REF TO data,
      lr_api         TYPE REF TO data,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <ls_name>       TYPE any,
      <ls_attr>       TYPE any,
      <lo_api>        TYPE any,
      <lv_value>      TYPE any.

    TRY.
        CREATE DATA lr_name TYPE ('IACIKEYT').
        CREATE DATA lr_attr TYPE ('IACXU').
        CREATE DATA lr_api TYPE REF TO ('CL_W3_API_XML3').
        ASSIGN lr_name->* TO <ls_name>.
        ASSIGN lr_attr->* TO <ls_attr>.
        ASSIGN lr_api->* TO <lo_api>.
        <ls_name> = me->key-obj_name.

        CALL METHOD ('CL_W3_API_XML3')=>load
          EXPORTING
            p_xml_name   = <ls_name>
          IMPORTING
            p_attributes = <ls_attr>
            p_xml        = <lo_api>
          EXCEPTIONS
            OTHERS       = 5.
        CHECK sy-subrc = 0.

        ASSIGN COMPONENT 'SERVICE' OF STRUCTURE <ls_attr> TO <lv_value>.
        IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
          TRY.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'IASP' obj_name = <lv_value>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional parent IAC service may not exist
          ENDTRY.
        ENDIF.

        UNASSIGN <lv_value>.
        ASSIGN COMPONENT 'MODULPOOL' OF STRUCTURE <ls_attr> TO <lv_value>.
        IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'PROG' obj_name = <lv_value>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional module pool may not exist
          ENDTRY.
        ENDIF.
      CATCH cx_root.
        " optional ITS XML-template API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

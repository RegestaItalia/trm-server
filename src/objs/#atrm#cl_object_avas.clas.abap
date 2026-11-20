CLASS /atrm/cl_object_avas DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_avas IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    " The classification API and its types (CLS_*, PAK_OBJECT_KEY) are not on
    " every release: everything is typed and called dynamically
    DATA:
      lo_assignment TYPE REF TO object,
      lr_attribute  TYPE REF TO data,
      lr_object     TYPE REF TO data,
      lr_links      TYPE REF TO data,
      ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lv_attribute> TYPE any,
      <ls_object>    TYPE any,
      <lt_links>     TYPE ANY TABLE,
      <ls_link>      TYPE any,
      <lv_object>    TYPE any,
      <lv_obj_name>  TYPE any.

    TRY.
        CREATE DATA lr_attribute TYPE ('CLS_ATTRIBUTE_NAME').
        CREATE DATA lr_object TYPE ('PAK_OBJECT_KEY').
        CREATE DATA lr_links TYPE ('CLS_LINKED_OBJECTS').
        ASSIGN lr_attribute->* TO <lv_attribute>.
        ASSIGN lr_object->* TO <ls_object>.
        ASSIGN lr_links->* TO <lt_links>.

        CREATE OBJECT lo_assignment
          TYPE ('CL_CLS_ATTR_VALUE_ASSIGNMENT')
          EXPORTING
            im_assignment_id = me->key-obj_name.

        CALL METHOD lo_assignment->('IF_CLS_ATTR_VALUE_ASSIGNMENT~GET_ATTRIBUTE')
          RECEIVING
            re_attribute = <lv_attribute>.
        IF <lv_attribute> IS NOT INITIAL.
          TRY.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'CHAR' obj_name = <lv_attribute>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        CALL METHOD lo_assignment->('IF_CLS_ATTR_VALUE_ASSIGNMENT~GET_OBJECT')
          RECEIVING
            re_object = <ls_object>.
        ASSIGN COMPONENT 'TROBJTYPE' OF STRUCTURE <ls_object> TO <lv_object>.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'SOBJ_NAME' OF STRUCTURE <ls_object> TO <lv_obj_name>.
        ENDIF.
        IF sy-subrc = 0 AND <lv_object> IS NOT INITIAL AND <lv_obj_name> IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING
                  object = <lv_object>
                  obj_name = <lv_obj_name>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        CALL METHOD lo_assignment->('IF_CLS_ATTR_VALUE_ASSIGNMENT~GET_LINKS')
          IMPORTING
            ex_links = <lt_links>.
        LOOP AT <lt_links> ASSIGNING <ls_link>.
          ASSIGN COMPONENT 'LINKED_OBJECT' OF STRUCTURE <ls_link> TO <ls_object>.
          CHECK sy-subrc = 0.
          ASSIGN COMPONENT 'TROBJTYPE' OF STRUCTURE <ls_object> TO <lv_object>.
          CHECK sy-subrc = 0.
          ASSIGN COMPONENT 'SOBJ_NAME' OF STRUCTURE <ls_object> TO <lv_obj_name>.
          CHECK sy-subrc = 0.
          CHECK <lv_object> IS NOT INITIAL.
          CHECK <lv_obj_name> IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING
                  object = <lv_object>
                  obj_name = <lv_obj_name>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " optional classification API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

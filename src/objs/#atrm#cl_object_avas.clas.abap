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
    DATA:
      lo_assignment TYPE REF TO object,
      lv_attribute  TYPE cls_attribute_name,
      ls_object     TYPE pak_object_key,
      lt_links      TYPE cls_linked_objects,
      ls_link       LIKE LINE OF lt_links,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        CREATE OBJECT lo_assignment
          TYPE ('CL_CLS_ATTR_VALUE_ASSIGNMENT')
          EXPORTING
            im_assignment_id = me->key-obj_name.

        CALL METHOD lo_assignment->('IF_CLS_ATTR_VALUE_ASSIGNMENT~GET_ATTRIBUTE')
          RECEIVING
            re_attribute = lv_attribute.
        IF lv_attribute IS NOT INITIAL.
          TRY.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'CHAR' obj_name = lv_attribute
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        CALL METHOD lo_assignment->('IF_CLS_ATTR_VALUE_ASSIGNMENT~GET_OBJECT')
          RECEIVING
            re_object = ls_object.
        IF ls_object-trobjtype IS NOT INITIAL
          AND ls_object-sobj_name IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING
                  object = ls_object-trobjtype
                  obj_name = ls_object-sobj_name
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        CALL METHOD lo_assignment->('IF_CLS_ATTR_VALUE_ASSIGNMENT~GET_LINKS')
          IMPORTING
            ex_links = lt_links.
        LOOP AT lt_links INTO ls_link.
          CHECK ls_link-linked_object-trobjtype IS NOT INITIAL.
          CHECK ls_link-linked_object-sobj_name IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING
                  object = ls_link-linked_object-trobjtype
                  obj_name = ls_link-linked_object-sobj_name
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

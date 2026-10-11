CLASS /atrm/cl_object_cus0 DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_cus0 IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      ls_header     TYPE cus_imgach,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        CALL FUNCTION 'S_CUS_IMG_ACTIVITY_READ'
          EXPORTING
            img_activity        = me->key-obj_name
          IMPORTING
            img_activity_header = ls_header
          EXCEPTIONS
            OTHERS              = 1.

        CHECK sy-subrc = 0.

        IF ls_header-tcode IS NOT INITIAL.
          TRY.
              CALL METHOD get_tadir_dependency
                EXPORTING
                  object     = 'TRAN'
                  obj_name   = ls_header-tcode
                RECEIVING
                  dependency = ls_dependency.

              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        IF ls_header-c_activity IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING
                  object     = 'CUS0'
                  obj_name   = ls_header-c_activity
                RECEIVING
                  dependency = ls_dependency.

              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.
      CATCH cx_root.
        " optional IMG API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

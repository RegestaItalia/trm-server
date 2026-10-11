CLASS /atrm/cl_object_http DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_http IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    TYPES:
      BEGIN OF lty_handler,
        id             TYPE c LENGTH 30,
        version        TYPE c LENGTH 1,
        serviceorder   TYPE n LENGTH 2,
        servicehandler TYPE c LENGTH 30,
      END OF lty_handler,
      BEGIN OF lty_icf_node,
        icfname    TYPE c LENGTH 15,
        icfparguid TYPE c LENGTH 25,
      END OF lty_icf_node.

    DATA:
      lv_name          TYPE c LENGTH 30,
      lo_service       TYPE REF TO object,
      lt_handlers      TYPE STANDARD TABLE OF lty_handler,
      ls_handler       TYPE lty_handler,
      ls_icf_node      TYPE lty_icf_node,
      lv_sicf_obj_name TYPE sobj_name,
      ls_dependency    TYPE /atrm/object_dependency.

    TRY.
        lv_name = me->key-obj_name.

        CALL METHOD ('CL_UCON_API_FACTORY')=>('GET_HTTP_SERVICE')
          EXPORTING
            name          = lv_name
            no_auth_check = abap_true
          RECEIVING
            http_service  = lo_service.

        CALL METHOD lo_service->('IF_UCON_API_HTTP_SERVICE~GET_HANDLER')
          RECEIVING
            handler = lt_handlers.

        LOOP AT lt_handlers INTO ls_handler.
          CHECK ls_handler-servicehandler IS NOT INITIAL.

          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING
                  object     = 'CLAS'
                  obj_name   = ls_handler-servicehandler
                RECEIVING
                  dependency = ls_dependency.

              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDLOOP.

        TRY.
            CALL METHOD lo_service->('IF_UCON_API_HTTP_SERVICE~GET_ICF_SERVICE')
              IMPORTING
                ev_icfservice = ls_icf_node.

            IF ls_icf_node-icfname IS NOT INITIAL.
              CLEAR lv_sicf_obj_name.
              lv_sicf_obj_name(15) = ls_icf_node-icfname.
              lv_sicf_obj_name+15(25) = ls_icf_node-icfparguid.

              CALL METHOD get_tadir_dependency
                EXPORTING
                  object     = 'SICF'
                  obj_name   = lv_sicf_obj_name
                RECEIVING
                  dependency = ls_dependency.

              APPEND ls_dependency TO dependencies.
            ENDIF.
          CATCH cx_root.
            " ICF linkage is not available in every target release
        ENDTRY.
      CATCH cx_root.
        " optional HTTP service API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

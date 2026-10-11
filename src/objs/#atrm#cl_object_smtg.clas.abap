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
      ls_header      TYPE smtg_tmpl_hdr,
      lv_ddls_name   TYPE sobj_name,
      lv_table_name  TYPE tabname,
      ls_dependency  TYPE /atrm/object_dependency.

    TRY.
        SELECT SINGLE *
          FROM smtg_tmpl_hdr
          INTO ls_header
          WHERE id = me->key-obj_name
            AND version = 'A'.
        CHECK sy-subrc = 0.

        IF ls_header-cds_view IS NOT INITIAL.
          lv_table_name = 'DDLDEPENDENCY'.
          TRY.
              SELECT SINGLE ddlname
                FROM (lv_table_name)
                INTO lv_ddls_name
                WHERE objectname = ls_header-cds_view
                  AND objecttype = 'STOB'
                  AND state = 'A'.
              CHECK lv_ddls_name IS NOT INITIAL.

              CALL METHOD get_tadir_dependency
                EXPORTING object = 'DDLS' obj_name = lv_ddls_name
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        IF ls_header-parent_id IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'SMTG' obj_name = ls_header-parent_id
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        IF ls_header-original_id IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'SMTG' obj_name = ls_header-original_id
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

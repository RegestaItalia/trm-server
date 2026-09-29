CLASS /atrm/cl_object_srvb DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_srvb IMPLEMENTATION.

METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_table_name TYPE tabname,
      lv_srvd_name TYPE sobj_name,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        lv_table_name = 'SRVB_CONTENT_V'.
        SELECT SINGLE srvdname
          FROM (lv_table_name)
          INTO lv_srvd_name
          WHERE srvb_name = me->key-obj_name
            AND version = 'A'.
        IF sy-subrc <> 0 OR lv_srvd_name IS INITIAL.
          RETURN.
        ENDIF.

        CALL METHOD get_tadir_dependency
          EXPORTING object = 'SRVD' obj_name = lv_srvd_name
          RECEIVING dependency = ls_dependency.
        APPEND ls_dependency TO dependencies.
      CATCH cx_root.
        " Service-binding metadata is release-specific
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

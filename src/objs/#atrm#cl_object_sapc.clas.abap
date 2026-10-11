CLASS /atrm/cl_object_sapc DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS append_icf_service
      IMPORTING path         TYPE string
      CHANGING  dependencies TYPE /atrm/object_dependency_t.
ENDCLASS.



CLASS /atrm/cl_object_sapc IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_class_name TYPE apc_appl-class_name,
      lv_path       TYPE apc_appl-path,
      lv_path_str   TYPE string,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        SELECT SINGLE class_name path
          FROM apc_appl
          INTO (lv_class_name, lv_path)
          WHERE application_id = me->key-obj_name
            AND version = 'A'.

        CHECK sy-subrc = 0.

        IF lv_class_name IS NOT INITIAL.
          TRY.
              CALL METHOD get_tadir_dependency
                EXPORTING
                  object     = 'CLAS'
                  obj_name   = lv_class_name
                RECEIVING
                  dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        " Generated ICF service of the application path
        IF lv_path IS NOT INITIAL.
          lv_path_str = lv_path.
          append_icf_service(
            EXPORTING path         = lv_path_str
            CHANGING  dependencies = dependencies ).
        ENDIF.
      CATCH cx_root.
        " optional dependency may not exist in the target system
    ENDTRY.

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

  METHOD append_icf_service.
    DATA:
      lt_segments   TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      lv_segment    TYPE string,
      lv_table      TYPE tabname,
      lv_where      TYPE string,
      lv_parent     TYPE icfparguid VALUE '0000000000000000000000000',
      lv_node       TYPE icfnodguid,
      lv_last_name  TYPE icfname,
      lv_last_par   TYPE icfparguid,
      lv_key        TYPE c LENGTH 40,
      ls_dependency TYPE /atrm/object_dependency.

    SPLIT path AT '/' INTO TABLE lt_segments.
    DELETE lt_segments WHERE table_line IS INITIAL.
    CHECK lt_segments IS NOT INITIAL.

    TRY.
        lv_table = 'ICFSERVICE'.
        LOOP AT lt_segments INTO lv_segment.
          TRANSLATE lv_segment TO UPPER CASE.
          REPLACE ALL OCCURRENCES OF `'` IN lv_segment WITH `''`.
          CONCATENATE `ICF_NAME = '` lv_segment `' AND ICFPARGUID = '` lv_parent `'` INTO lv_where.
          CLEAR lv_node.
          SELECT SINGLE ('ICFNODGUID') FROM (lv_table) INTO lv_node WHERE (lv_where).
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.
          lv_last_name = lv_segment.
          lv_last_par = lv_parent.
          lv_parent = lv_node.
        ENDLOOP.

        lv_key(15) = lv_last_name.
        lv_key+15(25) = lv_last_par.
        CALL METHOD get_tadir_dependency
          EXPORTING object = 'SICF' obj_name = lv_key
          RECEIVING dependency = ls_dependency.
        APPEND ls_dependency TO dependencies.
      CATCH cx_root.
        " optional ICF service may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS /atrm/cl_object_wdya DEFINITION
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



CLASS /atrm/cl_object_wdya IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name       TYPE string,
      lv_ns         TYPE string,
      lv_app        TYPE string,
      lv_path       TYPE string,
      lv_where      TYPE string,
      lv_table      TYPE tabname,
      lt_values     TYPE STANDARD TABLE OF wdy_md_prop_type_string WITH DEFAULT KEY,
      lv_value      TYPE string,
      lr_rows       TYPE REF TO data,
      ls_config_key TYPE wdy_config_key,
      lv_config     TYPE sobj_name,
      ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_rows> TYPE STANDARD TABLE,
      <ls_row>  TYPE any.

    super->/atrm/if_object~get_dependencies(
      IMPORTING dependencies = dependencies ).

    " Generated ICF service /sap/bc/webdynpro/<namespace or sap>/<application>
    lv_name = me->key-obj_name.
    TRANSLATE lv_name TO LOWER CASE.
    IF lv_name(1) = '/'.
      SPLIT lv_name+1 AT '/' INTO lv_ns lv_app.
    ELSE.
      lv_ns = 'sap'.
      lv_app = lv_name.
    ENDIF.
    CONCATENATE '/sap/bc/webdynpro/' lv_ns '/' lv_app INTO lv_path.
    append_icf_service(
      EXPORTING path         = lv_path
      CHANGING  dependencies = dependencies ).

    " Application configuration named by parameter WDCONFIGURATIONID
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.
    TRY.
        lv_table = 'WDY_APP_PROPERTY'.
        CONCATENATE `APPLICATION_NAME = '` lv_name `' AND NAME = 'WDCONFIGURATIONID'` INTO lv_where.
        SELECT ('VALUE') FROM (lv_table) INTO TABLE lt_values WHERE (lv_where).
      CATCH cx_root.
        CLEAR lt_values.
    ENDTRY.
    LOOP AT lt_values INTO lv_value.
      CHECK lv_value IS NOT INITIAL.
      TRANSLATE lv_value TO UPPER CASE.
      REPLACE ALL OCCURRENCES OF `'` IN lv_value WITH `''`.
      TRY.
          lv_table = 'WDY_CONFIG_APPL'.
          CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_table).
          ASSIGN lr_rows->* TO <lt_rows>.
          CONCATENATE `CONFIG_ID = '` lv_value `'` INTO lv_where.
          SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).
          LOOP AT <lt_rows> ASSIGNING <ls_row>.
            MOVE-CORRESPONDING <ls_row> TO ls_config_key.
            lv_config = ls_config_key.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'WDCA' obj_name = lv_config
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDLOOP.
        CATCH cx_root.
          " optional configuration table may not exist in the target system
      ENDTRY.
    ENDLOOP.

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

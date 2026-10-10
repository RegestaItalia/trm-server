CLASS /atrm/cl_object_dcls DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS /atrm/cl_object_dcls IMPLEMENTATION.
METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_table_name TYPE tabname,
      lv_ddl_table TYPE tabname,
      lv_source TYPE string,
      lt_tokens TYPE STANDARD TABLE OF string,
      lv_token TYPE string,
      lv_upper_token TYPE string,
      lv_entity TYPE sobj_name,
      lv_ddls_name TYPE sobj_name,
      lv_dependency TYPE /atrm/object_dependency,
      lv_index TYPE i.

    TRY.
        lv_table_name = 'ACMDCLSRC'.
        SELECT SINGLE source FROM (lv_table_name) INTO lv_source
          WHERE dclname = me->key-obj_name AND as4local = 'A'.
        CHECK sy-subrc = 0 AND lv_source IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
          IN lv_source WITH space.
        CONDENSE lv_source.
        SPLIT lv_source AT space INTO TABLE lt_tokens.

        LOOP AT lt_tokens INTO lv_token.
          lv_upper_token = lv_token.
          TRANSLATE lv_upper_token TO UPPER CASE.
          CHECK lv_upper_token = 'ON'.
          lv_index = sy-tabix + 1.
          READ TABLE lt_tokens INTO lv_entity INDEX lv_index.
          CHECK sy-subrc = 0.
          REPLACE ALL OCCURRENCES OF ';' IN lv_entity WITH ''.
          REPLACE ALL OCCURRENCES OF ',' IN lv_entity WITH ''.
          REPLACE ALL OCCURRENCES OF '{' IN lv_entity WITH ''.
          REPLACE ALL OCCURRENCES OF '}' IN lv_entity WITH ''.
          CHECK lv_entity IS NOT INITIAL.

          TRY.
              lv_ddl_table = 'DDLDEPENDENCY'.
              SELECT SINGLE ddlname FROM (lv_ddl_table) INTO lv_ddls_name
                WHERE objectname = lv_entity
                  AND objecttype = 'STOB' AND state = 'A'.
              IF sy-subrc <> 0 OR lv_ddls_name IS INITIAL.
                lv_ddls_name = lv_entity.
              ENDIF.
              CLEAR lv_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'DDLS' obj_name = lv_ddls_name
                RECEIVING dependency = lv_dependency.
              APPEND lv_dependency TO dependencies.
            CATCH cx_root.
              " Protected CDS entity may not have a repository DDL source
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " DCLS source storage is optional on older SAP releases
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

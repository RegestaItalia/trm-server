CLASS /atrm/cl_object_srvd DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_srvd IMPLEMENTATION.

METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_table_name TYPE tabname,
      lv_ddl_table TYPE tabname,
      lv_source TYPE string,
      lt_tokens TYPE STANDARD TABLE OF string,
      lv_token TYPE string,
      lv_keyword TYPE string,
      lv_entity TYPE string,
      lv_alias TYPE string,
      lv_ddls_name TYPE sobj_name,
      lv_index TYPE i,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        lv_table_name = 'SRVDSRC_SRC'.
        SELECT SINGLE source FROM (lv_table_name) INTO lv_source
          WHERE srvdname = me->key-obj_name AND version = 'A'.
        CHECK sy-subrc = 0 AND lv_source IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
          IN lv_source WITH space.
        CONDENSE lv_source.
        REPLACE ALL OCCURRENCES OF ';' IN lv_source WITH ' ; '.
        SPLIT lv_source AT space INTO TABLE lt_tokens.

        LOOP AT lt_tokens INTO lv_token.
          lv_keyword = lv_token.
          TRANSLATE lv_keyword TO UPPER CASE.
          CHECK lv_keyword = 'EXPOSE'.
          lv_index = sy-tabix + 1.
          CLEAR: lv_entity, lv_alias.
          READ TABLE lt_tokens INTO lv_entity INDEX lv_index.
          CHECK sy-subrc = 0.
          REPLACE ALL OCCURRENCES OF ',' IN lv_entity WITH ''.
          REPLACE ALL OCCURRENCES OF ';' IN lv_entity WITH ''.
          lv_index = lv_index + 1.
          READ TABLE lt_tokens INTO lv_alias INDEX lv_index.
          IF sy-subrc = 0 AND lv_alias = 'AS'.
            lv_index = lv_index + 1.
            READ TABLE lt_tokens INTO lv_alias INDEX lv_index.
          ENDIF.

          TRY.
              CLEAR lv_ddls_name.
              lv_ddl_table = 'DDLDEPENDENCY'.
              SELECT SINGLE ddlname FROM (lv_ddl_table) INTO lv_ddls_name
                WHERE objectname = lv_entity
                  AND objecttype = 'STOB' AND state = 'A'.
              IF sy-subrc <> 0 OR lv_ddls_name IS INITIAL.
                SELECT SINGLE ddlname FROM (lv_ddl_table) INTO lv_ddls_name
                  WHERE ddlname = lv_entity AND state = 'A'.
              ENDIF.
              IF lv_ddls_name IS NOT INITIAL.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'DDLS' obj_name = lv_ddls_name
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              ENDIF.
            CATCH cx_root.
              " Exposed entity may not have a repository DDL source
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " Service-definition persistence is release-specific
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

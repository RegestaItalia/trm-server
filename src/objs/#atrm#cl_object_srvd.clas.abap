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
      lv_source     TYPE string,
      lt_tokens     TYPE STANDARD TABLE OF string,
      lv_token      TYPE string,
      lv_keyword    TYPE string,
      lv_entity     TYPE string,
      lv_ddls_name  TYPE sobj_name,
      lv_index      TYPE i,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        lv_table_name = 'SRVDSRC_SRC'.
        SELECT SINGLE source
          FROM (lv_table_name)
          INTO lv_source
          WHERE srvdname = me->key-obj_name
            AND version = 'A'.
        CHECK sy-subrc = 0.
        CHECK lv_source IS NOT INITIAL.

        REPLACE ALL OCCURRENCES OF REGEX '\s+' IN lv_source WITH ' '.
        SPLIT lv_source AT space INTO TABLE lt_tokens.

        LOOP AT lt_tokens INTO lv_token.
          lv_keyword = lv_token.
          TRANSLATE lv_keyword TO UPPER CASE.
          CHECK lv_keyword = 'EXPOSE'.

          lv_index = sy-tabix + 1.
          CLEAR lv_entity.
          READ TABLE lt_tokens INTO lv_entity INDEX lv_index.
          CHECK sy-subrc = 0.
          REPLACE ALL OCCURRENCES OF ',' IN lv_entity WITH ''.
          REPLACE ALL OCCURRENCES OF ';' IN lv_entity WITH ''.
          CHECK lv_entity IS NOT INITIAL.

          CLEAR lv_ddls_name.
          TRY.
              SELECT SINGLE ddlname
                FROM ddldependency
                INTO lv_ddls_name
                WHERE objectname = lv_entity
                  AND objecttype = 'STOB'
                  AND state = 'A'.
              CHECK lv_ddls_name IS NOT INITIAL.

              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'DDLS' obj_name = lv_ddls_name
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " optional service definition API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

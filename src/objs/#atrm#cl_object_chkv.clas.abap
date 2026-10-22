CLASS /atrm/cl_object_chkv DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_chkv IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name       TYPE string,
      lv_where      TYPE string,
      lv_table      TYPE tabname,
      lt_contents   TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      lv_content    TYPE string,
      lt_results    TYPE match_result_tab,
      ls_result     TYPE match_result,
      ls_submatch   TYPE submatch_result,
      lv_value      TYPE string,
      lv_obj_name   TYPE sobj_name,
      ls_dependency TYPE /atrm/object_dependency.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.

    TRY.
        lv_table = 'CHKV_CONTENT'.
        CONCATENATE `NAME = '` lv_name `' AND VERSION = 'A'` INTO lv_where.
        SELECT ('CONTENT') FROM (lv_table) INTO TABLE lt_contents WHERE (lv_where).
      CATCH cx_root.
        CLEAR lt_contents.
    ENDTRY.

    LOOP AT lt_contents INTO lv_content.
      " Checks included in the variant
      FIND ALL OCCURRENCES OF PCRE `<[^>]*checkObject\s[^>]*adtcore:name="([^"]+)"`
        IN lv_content RESULTS lt_results.
      LOOP AT lt_results INTO ls_result.
        READ TABLE ls_result-submatches INTO ls_submatch INDEX 1.
        CHECK sy-subrc = 0.
        lv_value = lv_content+ls_submatch-offset(ls_submatch-length).
        lv_obj_name = lv_value.
        TRY.
            ls_dependency = get_tadir_dependency(
              object   = 'CHKO'
              obj_name = lv_obj_name ).
            APPEND ls_dependency TO dependencies.
          CATCH cx_root.
            " check may not exist in the target system
        ENDTRY.
      ENDLOOP.

      " Referenced (base) check variant
      FIND ALL OCCURRENCES OF PCRE `<[^>]*referencedVariant>([^<]+)<`
        IN lv_content RESULTS lt_results.
      LOOP AT lt_results INTO ls_result.
        READ TABLE ls_result-submatches INTO ls_submatch INDEX 1.
        CHECK sy-subrc = 0.
        lv_value = lv_content+ls_submatch-offset(ls_submatch-length).
        CONDENSE lv_value.
        lv_obj_name = lv_value.
        TRY.
            ls_dependency = get_tadir_dependency(
              object   = 'CHKV'
              obj_name = lv_obj_name ).
            APPEND ls_dependency TO dependencies.
          CATCH cx_root.
            " referenced variant may not exist in the target system
        ENDTRY.
      ENDLOOP.
    ENDLOOP.

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.

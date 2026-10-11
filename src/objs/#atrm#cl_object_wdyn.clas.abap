CLASS /atrm/cl_object_wdyn DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_wdyn IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name       TYPE string,
      lv_where      TYPE string,
      lv_table      TYPE tabname,
      lt_help_ids   TYPE STANDARD TABLE OF wdy_value_help_id WITH DEFAULT KEY,
      lv_help_id    TYPE wdy_value_help_id,
      lv_shlp       TYPE sobj_name,
      ls_dependency TYPE /atrm/object_dependency.

    super->/atrm/if_object~get_dependencies(
      IMPORTING dependencies = dependencies ).

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.

    " Context attributes with an explicit dictionary search help
    TRY.
        lv_table = 'WDY_CTX_ATTRIB'.
        CONCATENATE `COMPONENT_NAME = '` lv_name `' AND VERSION = 'A' AND VALUE_HELP_ID LIKE 'SEARCHHELP:%'` INTO lv_where.
        SELECT ('VALUE_HELP_ID') FROM (lv_table) INTO TABLE lt_help_ids WHERE (lv_where).
      CATCH cx_root.
        CLEAR lt_help_ids.
    ENDTRY.
    LOOP AT lt_help_ids INTO lv_help_id.
      lv_shlp = lv_help_id+11.
      CHECK lv_shlp IS NOT INITIAL.
      TRY.
          CLEAR ls_dependency.
          CALL METHOD get_tadir_dependency
            EXPORTING object = 'SHLP' obj_name = lv_shlp
            RECEIVING dependency = ls_dependency.
          APPEND ls_dependency TO dependencies.
        CATCH cx_root.
          " optional dependency may not exist in the target system
      ENDTRY.
    ENDLOOP.

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.

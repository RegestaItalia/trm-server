CLASS /atrm/cl_object_shma DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_shma IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name       TYPE string,
      lv_where      TYPE string,
      ls_dependency TYPE /atrm/object_dependency.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.

    " Root data class
    CONCATENATE `AREA_NAME = '` lv_name `'` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'SHMA_ATTRIBUTES'
                where_clause = lv_where
                object_field = 'ROOT'
                object_type  = 'CLAS'
      CHANGING  dependencies = dependencies ).

    " Area constructor class
    append_table_dependencies(
      EXPORTING table_name   = 'SHMA_ATTRIBUTES'
                where_clause = lv_where
                object_field = 'AUTO_BUILD_CLASS'
                object_type  = 'CLAS'
      CHANGING  dependencies = dependencies ).

    " Generated area class (same name as the area)
    TRY.
        ls_dependency = get_tadir_dependency(
          object   = 'CLAS'
          obj_name = me->key-obj_name ).
        APPEND ls_dependency TO dependencies.
      CATCH cx_root.
        " area class not generated yet
    ENDTRY.

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.

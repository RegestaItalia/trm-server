CLASS /atrm/cl_object_iwsv DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_iwsv IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name       TYPE sobj_name,
      lv_id         TYPE string,
      lv_version    TYPE string,
      lv_where      TYPE string,
      lv_table      TYPE tabname,
      lr_rows       TYPE REF TO data,
      lv_model      TYPE c LENGTH 36,
      lv_model_name TYPE sobj_name,
      ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_rows>    TYPE STANDARD TABLE,
      <ls_row>     TYPE any,
      <lv_model>   TYPE any,
      <lv_version> TYPE any.

    " TADIR name: technical service name (35 characters) + version (4)
    lv_name = me->key-obj_name.
    lv_id = lv_name(35).
    lv_version = lv_name+35(4).
    REPLACE ALL OCCURRENCES OF '''' IN lv_id WITH ''''''.
    REPLACE ALL OCCURRENCES OF '''' IN lv_version WITH ''''''.
    CONCATENATE 'TECHNICAL_NAME = ''' lv_id
      ''' AND VERSION = ''' lv_version '''' INTO lv_where.

    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = '/IWBEP/I_MGW_SRH'
        where_clause = lv_where
        object_field = 'CLASS_NAME'
        object_type  = 'CLAS'
      CHANGING
        dependencies = dependencies.

    " Assigned models: TADIR name = model name (32 characters) + version (4)
    CONCATENATE 'GROUP_TECH_NAME = ''' lv_id
      ''' AND GROUP_VERSION = ''' lv_version '''' INTO lv_where.
    TRY.
        lv_table = '/IWBEP/I_MGW_SRG'.
        CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_rows->* TO <lt_rows>.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).
        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'MODEL_TECH_NAME' OF STRUCTURE <ls_row> TO <lv_model>.
          CHECK sy-subrc = 0.
          ASSIGN COMPONENT 'MODEL_VERSION' OF STRUCTURE <ls_row> TO <lv_version>.
          CHECK sy-subrc = 0.
          CHECK <lv_model> IS NOT INITIAL.
          CLEAR lv_model.
          lv_model(32) = <lv_model>.
          lv_model+32(4) = <lv_version>.
          lv_model_name = lv_model.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'IWMO' obj_name = lv_model_name
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " optional repository table may not exist in the target system
    ENDTRY.

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.

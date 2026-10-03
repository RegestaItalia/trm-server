CLASS /atrm/cl_object_iwpr DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS append_registered
      IMPORTING object_type  TYPE trobjtype
                technical    TYPE csequence
                version      TYPE csequence OPTIONAL
      CHANGING  dependencies TYPE /atrm/object_dependency_t.
    METHODS append_type
      IMPORTING type_name    TYPE string
      CHANGING  dependencies TYPE /atrm/object_dependency_t.
ENDCLASS.



CLASS /atrm/cl_object_iwpr IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name       TYPE string,
      lv_where      TYPE string,
      lv_table      TYPE tabname,
      lr_rows       TYPE REF TO data,
      lt_structs    TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      lv_struct     TYPE string,
      lv_type       TYPE trobjtype,
      lv_value      TYPE string,
      ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_rows>     TYPE STANDARD TABLE,
      <ls_row>      TYPE any,
      <lv_type>     TYPE any,
      <lv_object>   TYPE any,
      <lv_version>  TYPE any.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.
    CONCATENATE `PROJECT = '` lv_name `'` INTO lv_where.

    " Entity types imported from or bound to a DDIC structure
    TRY.
        lv_table = '/IWBEP/I_SBO_ET'.
        SELECT ('ABAP_STRUCT') FROM (lv_table) INTO TABLE lt_structs WHERE (lv_where).
      CATCH cx_root.
        CLEAR lt_structs.
    ENDTRY.
    LOOP AT lt_structs INTO lv_struct.
      CHECK lv_struct IS NOT INITIAL.
      append_type( EXPORTING type_name = lv_struct
                   CHANGING  dependencies = dependencies ).
    ENDLOOP.

    " Generated runtime artifacts: classes, technical model and service
    TRY.
        lv_table = '/IWBEP/I_SBD_GA'.
        CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_rows->* TO <lt_rows>.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).
        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'TROBJ_TYPE' OF STRUCTURE <ls_row> TO <lv_type>.
          CHECK sy-subrc = 0.
          ASSIGN COMPONENT 'TROBJ_NAME' OF STRUCTURE <ls_row> TO <lv_object>.
          CHECK sy-subrc = 0.
          CHECK <lv_object> IS NOT INITIAL.
          lv_type = <lv_type>.
          IF lv_type = 'IWMO' OR lv_type = 'IWSV'.
            append_registered( EXPORTING object_type = lv_type technical = <lv_object>
                               CHANGING  dependencies = dependencies ).
          ELSE.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = lv_type obj_name = <lv_object>
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        " optional repository table may not exist in the target system
    ENDTRY.

    " Included, referenced or redefined services and models
    TRY.
        lv_table = '/IWBEP/I_SBO_MR'.
        CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_rows->* TO <lt_rows>.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).
        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'OBJECT_NAME' OF STRUCTURE <ls_row> TO <lv_object>.
          CHECK sy-subrc = 0.
          ASSIGN COMPONENT 'OBJECT_VERSION' OF STRUCTURE <ls_row> TO <lv_version>.
          CHECK sy-subrc = 0.
          CHECK <lv_object> IS NOT INITIAL.
          append_registered( EXPORTING object_type = 'IWSV' technical = <lv_object> version = <lv_version>
                             CHANGING  dependencies = dependencies ).
          append_registered( EXPORTING object_type = 'IWMO' technical = <lv_object> version = <lv_version>
                             CHANGING  dependencies = dependencies ).
        ENDLOOP.
      CATCH cx_root.
        " optional repository table may not exist in the target system
    ENDTRY.

    " Data-source mappings: CDS business entity (DS_GROUP CDS~<entity>) and RFC module
    TRY.
        lv_table = '/IWBEP/I_SBD_DS'.
        CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_rows->* TO <lt_rows>.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).
        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'DS_GROUP' OF STRUCTURE <ls_row> TO <lv_object>.
          IF sy-subrc = 0 AND <lv_object> IS NOT INITIAL.
            lv_value = <lv_object>.
            IF lv_value CP 'CDS~*'.
              lv_value = lv_value+4.
              TRY.
                  CLEAR ls_dependency.
                  CALL METHOD get_cds_dependency
                    EXPORTING entity = lv_value
                    IMPORTING dependency = ls_dependency.
                  IF ls_dependency IS NOT INITIAL.
                    APPEND ls_dependency TO dependencies.
                  ENDIF.
                CATCH cx_root.
                  " optional dependency may not exist in the target system
              ENDTRY.
            ENDIF.
          ENDIF.
          ASSIGN COMPONENT 'FUNCTION_NAME' OF STRUCTURE <ls_row> TO <lv_object>.
          IF sy-subrc = 0 AND <lv_object> IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tfdir_dependency
                  EXPORTING funcname = <lv_object>
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        " optional repository table may not exist in the target system
    ENDTRY.

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

  METHOD append_registered.
    " TADIR name of a technical model (32) or service (35) + 4-digit version
    DATA:
      lv_table      TYPE tabname,
      lv_length     TYPE i,
      lv_where      TYPE string,
      lv_technical  TYPE string,
      lt_versions   TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      lv_version    TYPE string,
      lv_key        TYPE c LENGTH 40,
      lv_object     TYPE sobj_name,
      ls_dependency TYPE /atrm/object_dependency.

    IF object_type = 'IWMO'.
      lv_table = '/IWBEP/I_MGW_OHD'.
      lv_length = 32.
    ELSE.
      lv_table = '/IWBEP/I_MGW_SRH'.
      lv_length = 35.
    ENDIF.

    IF version IS SUPPLIED AND version IS NOT INITIAL.
      lv_version = version.
      APPEND lv_version TO lt_versions.
    ELSE.
      lv_technical = technical.
      REPLACE ALL OCCURRENCES OF `'` IN lv_technical WITH `''`.
      CONCATENATE `TECHNICAL_NAME = '` lv_technical `'` INTO lv_where.
      TRY.
          SELECT ('VERSION') FROM (lv_table) INTO TABLE lt_versions WHERE (lv_where).
        CATCH cx_root.
          RETURN.
      ENDTRY.
    ENDIF.

    LOOP AT lt_versions INTO lv_version.
      CLEAR lv_key.
      lv_key(lv_length) = technical.
      lv_key+lv_length(4) = lv_version.
      lv_object = lv_key.
      TRY.
          CLEAR ls_dependency.
          CALL METHOD get_tadir_dependency
            EXPORTING object = object_type obj_name = lv_object
            RECEIVING dependency = ls_dependency.
          APPEND ls_dependency TO dependencies.
        CATCH cx_root.
          " optional dependency may not exist in the target system
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD append_type.
    DATA:
      lv_name       TYPE string,
      lv_object     TYPE sobj_name,
      lt_types      TYPE STANDARD TABLE OF trobjtype WITH DEFAULT KEY,
      lv_type       TYPE trobjtype,
      ls_dependency TYPE /atrm/object_dependency.

    lv_name = type_name.
    CONDENSE lv_name.
    TRANSLATE lv_name TO UPPER CASE.
    FIND REGEX '^([A-Z0-9_/]+)' IN lv_name SUBMATCHES lv_object.
    CHECK sy-subrc = 0 AND lv_object IS NOT INITIAL.

    APPEND 'TABL' TO lt_types.
    APPEND 'TTYP' TO lt_types.
    APPEND 'DTEL' TO lt_types.
    APPEND 'VIEW' TO lt_types.
    LOOP AT lt_types INTO lv_type.
      TRY.
          CLEAR ls_dependency.
          CALL METHOD get_tadir_dependency
            EXPORTING object = lv_type obj_name = lv_object
            RECEIVING dependency = ls_dependency.
          APPEND ls_dependency TO dependencies.
          RETURN.
        CATCH cx_root.
          " try the next repository type
      ENDTRY.
    ENDLOOP.

    TRY.
        CLEAR ls_dependency.
        CALL METHOD get_cds_dependency
          EXPORTING entity = lv_object
          IMPORTING dependency = ls_dependency.
        IF ls_dependency IS NOT INITIAL.
          APPEND ls_dependency TO dependencies.
        ENDIF.
      CATCH cx_root.
        " unknown type
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

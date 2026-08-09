CLASS /atrm/cl_object_ddlx DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ty_ddlx_name       TYPE c LENGTH 40,
           ty_entity_name     TYPE c LENGTH 40,
           ty_variant_name    TYPE c LENGTH 40,
           ty_annotation_name TYPE c LENGTH 240,
           ty_annotation_value TYPE c LENGTH 1300.

    TYPES:
      BEGIN OF ty_runtime_header,
        extended_artifact TYPE ty_entity_name,
        variant           TYPE ty_variant_name,
      END OF ty_runtime_header.

    TYPES:
      BEGIN OF ty_annotation,
        name  TYPE ty_annotation_name,
        value TYPE ty_annotation_value,
      END OF ty_annotation,
      tyt_annotation TYPE STANDARD TABLE OF ty_annotation WITH DEFAULT KEY.

    METHODS add_tadir_dependency
      IMPORTING
        iv_object       TYPE any
        iv_obj_name     TYPE any
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t.

    METHODS add_entity_dependency
      IMPORTING
        iv_entity       TYPE any
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t.

    CLASS-METHODS map_entity_to_ddls
      IMPORTING
        iv_entity          TYPE any
      RETURNING
        VALUE(rv_ddls_name) TYPE ty_ddlx_name.

    METHODS read_runtime_header
      IMPORTING
        iv_ddlx_name     TYPE ty_ddlx_name
      RETURNING
        VALUE(rs_header) TYPE ty_runtime_header.

    METHODS add_annotation_dependencies
      IMPORTING
        iv_ddlx_name     TYPE ty_ddlx_name
      CHANGING
        ct_dependencies  TYPE /atrm/object_dependency_t.

    CLASS-METHODS normalize_literal
      IMPORTING
        iv_value        TYPE any
      RETURNING
        VALUE(rv_value) TYPE string.
ENDCLASS.



CLASS /atrm/cl_object_ddlx IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_ddlx_name TYPE ty_ddlx_name,
          ls_header    TYPE ty_runtime_header.

    TRY.
        lv_ddlx_name = me->key-obj_name.

        CALL METHOD read_runtime_header
          EXPORTING
            iv_ddlx_name = lv_ddlx_name
          RECEIVING
            rs_header    = ls_header.

        IF ls_header-extended_artifact IS NOT INITIAL.
          add_entity_dependency(
            EXPORTING
              iv_entity       = ls_header-extended_artifact
            CHANGING
              ct_dependencies = dependencies
          ).
        ENDIF.

        IF ls_header-variant IS NOT INITIAL.
          add_tadir_dependency(
            EXPORTING
              iv_object       = 'DDLV'
              iv_obj_name     = ls_header-variant
            CHANGING
              ct_dependencies = dependencies
          ).
        ENDIF.

        add_annotation_dependencies(
          EXPORTING
            iv_ddlx_name     = lv_ddlx_name
          CHANGING
            ct_dependencies  = dependencies
        ).
      CATCH cx_root.
        " DDLX is optional. Missing DDLX repository objects must not prevent
        " this class from being used on systems without metadata extensions.
    ENDTRY.
  ENDMETHOD.

  METHOD add_tadir_dependency.
    DATA ls_dependency TYPE /atrm/object_dependency.

    CHECK iv_object IS NOT INITIAL.
    CHECK iv_obj_name IS NOT INITIAL.

    CLEAR ls_dependency.
    TRY.
        get_tadir_dependency(
          EXPORTING
            object     = iv_object
            obj_name   = iv_obj_name
          RECEIVING
            dependency = ls_dependency
        ).
        READ TABLE ct_dependencies
          WITH KEY tabname = 'TADIR'
                   tabkey  = ls_dependency-tabkey
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND ls_dependency TO ct_dependencies.
        ENDIF.
      CATCH /atrm/cx_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD map_entity_to_ddls.
    DATA lv_table_name TYPE tabname.

    CLEAR rv_ddls_name.
    CHECK iv_entity IS NOT INITIAL.

    lv_table_name = 'DDLDEPENDENCY'.
    TRY.
        SELECT SINGLE ddlname
          FROM (lv_table_name)
          INTO rv_ddls_name
          WHERE objectname EQ iv_entity
            AND objecttype EQ 'STOB'
            AND state EQ 'A'.
      CATCH cx_root.
        CLEAR rv_ddls_name.
    ENDTRY.
  ENDMETHOD.

  METHOD add_entity_dependency.
    DATA: lv_entity    TYPE ty_entity_name,
          lv_ddls_name TYPE ty_ddlx_name.

    CHECK iv_entity IS NOT INITIAL.
    lv_entity = iv_entity.

    CALL METHOD map_entity_to_ddls
      EXPORTING
        iv_entity    = lv_entity
      RECEIVING
        rv_ddls_name = lv_ddls_name.
    IF lv_ddls_name IS INITIAL.
      lv_ddls_name = lv_entity.
    ENDIF.

    add_tadir_dependency(
      EXPORTING
        iv_object       = 'DDLS'
        iv_obj_name     = lv_ddls_name
      CHANGING
        ct_dependencies = ct_dependencies
    ).
  ENDMETHOD.

  METHOD read_runtime_header.
    DATA: lv_table_name  TYPE tabname,
          lv_class_name  TYPE string,
          lv_method_name TYPE string,
          lo_provider    TYPE REF TO object.

    CLEAR rs_header.
    CHECK iv_ddlx_name IS NOT INITIAL.

    lv_table_name = 'DDLX_RT_HEADER'.
    TRY.
        SELECT SINGLE extended_artifact variant
          FROM (lv_table_name)
          INTO CORRESPONDING FIELDS OF rs_header
          WHERE ddlxname EQ iv_ddlx_name.
      CATCH cx_root.
        CLEAR rs_header.
    ENDTRY.

    IF rs_header-extended_artifact IS INITIAL.
      TRY.
          lv_class_name = 'CL_DDLX_METADATA_PROVIDER'.
          lv_method_name = 'GET_EXTENDED_ENTITY'.
          CREATE OBJECT lo_provider TYPE (lv_class_name).
          CALL METHOD lo_provider->(lv_method_name)
            EXPORTING
              i_ddlxname     = iv_ddlx_name
            RECEIVING
              r_entityname   = rs_header-extended_artifact.
        CATCH cx_root.
          CLEAR rs_header.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD normalize_literal.
    DATA: lv_length TYPE i,
          lv_offset TYPE i.

    CLEAR rv_value.
    CHECK iv_value IS NOT INITIAL.

    rv_value = iv_value.
    SHIFT rv_value LEFT DELETING LEADING space.
    SHIFT rv_value RIGHT DELETING TRAILING space.

    lv_length = strlen( rv_value ).
    IF lv_length GE 2.
      lv_offset = lv_length - 1.
      IF rv_value(1) EQ '''' AND rv_value+lv_offset(1) EQ ''''.
        lv_length = lv_length - 2.
        rv_value = rv_value+1(lv_length).
        REPLACE ALL OCCURRENCES OF '''''' IN rv_value WITH ''''.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD add_annotation_dependencies.
    DATA: lv_table_name       TYPE tabname,
          lt_annotations      TYPE tyt_annotation,
          ls_annotation       TYPE ty_annotation,
          lv_annotation_name  TYPE string,
          lv_root_name        TYPE string,
          lv_remainder        TYPE string,
          lv_value            TYPE string,
          lv_value_upper      TYPE string,
          lv_class_name       TYPE string,
          lv_reference_entity TYPE string,
          lv_reference_rest   TYPE string,
          lv_mapped_ddls      TYPE ty_ddlx_name,
          lv_factory_class    TYPE string,
          lv_factory_method   TYPE string,
          lv_definition_type  TYPE string,
          lv_definition_upper TYPE string,
          lv_type_name        TYPE string,
          lo_definitions      TYPE REF TO object,
          lr_definition       TYPE REF TO data.
    FIELD-SYMBOLS: <ls_definition>      TYPE any,
                   <lv_definition_type> TYPE any.

    CHECK iv_ddlx_name IS NOT INITIAL.

    add_tadir_dependency(
      EXPORTING
        iv_object       = 'DDLA'
        iv_obj_name     = 'METADATA'
      CHANGING
        ct_dependencies = ct_dependencies
    ).

    lv_table_name = 'DDLX_RT_DATA'.
    TRY.
        SELECT name value
          FROM (lv_table_name)
          INTO CORRESPONDING FIELDS OF TABLE lt_annotations
          WHERE ddlxname EQ iv_ddlx_name.
      CATCH cx_root.
        RETURN.
    ENDTRY.

    TRY.
        lv_factory_class = 'CL_CDS_ANNOTATION_DEFS_FACTORY'.
        lv_factory_method = 'CREATE_BUFFERED_ANNO_DEFS'.
        CALL METHOD (lv_factory_class)=>(lv_factory_method)
          RECEIVING
            r_result = lo_definitions.

        lv_type_name = 'IF_CDS_ANNOTATION_TYPES=>TY_ANNOTATION_DATA'.
        CREATE DATA lr_definition TYPE (lv_type_name).
        ASSIGN lr_definition->* TO <ls_definition>.
      CATCH cx_root.
        CLEAR lo_definitions.
        CLEAR lr_definition.
    ENDTRY.

    LOOP AT lt_annotations INTO ls_annotation.
      lv_annotation_name = ls_annotation-name.
      CLEAR: lv_root_name, lv_remainder.
      SPLIT lv_annotation_name AT '.' INTO lv_root_name lv_remainder.
      IF lv_root_name IS NOT INITIAL.
        add_tadir_dependency(
          EXPORTING
            iv_object       = 'DDLA'
            iv_obj_name     = lv_root_name
          CHANGING
            ct_dependencies = ct_dependencies
        ).
      ENDIF.

      CALL METHOD normalize_literal
        EXPORTING
          iv_value = ls_annotation-value
        RECEIVING
          rv_value = lv_value.
      CHECK lv_value IS NOT INITIAL.

      lv_value_upper = lv_value.
      TRANSLATE lv_value_upper TO UPPER CASE.
      IF lv_value_upper CP 'ABAP:*'.
        lv_class_name = lv_value+5.
        add_tadir_dependency(
          EXPORTING
            iv_object       = 'CLAS'
            iv_obj_name     = lv_class_name
          CHANGING
            ct_dependencies = ct_dependencies
        ).
      ENDIF.

      CLEAR lv_definition_type.
      IF lo_definitions IS BOUND AND <ls_definition> IS ASSIGNED.
        TRY.
            CLEAR <ls_definition>.
            CALL METHOD lo_definitions->('GET_ANNOTATION')
              EXPORTING
                i_annotation_name = lv_annotation_name
              RECEIVING
                r_result          = <ls_definition>.
            UNASSIGN <lv_definition_type>.
            ASSIGN COMPONENT 'TYPE' OF STRUCTURE <ls_definition>
              TO <lv_definition_type>.
            IF sy-subrc EQ 0.
              lv_definition_type = <lv_definition_type>.
            ENDIF.
          CATCH cx_root.
            CLEAR lv_definition_type.
        ENDTRY.
      ENDIF.

      lv_definition_upper = lv_definition_type.
      TRANSLATE lv_definition_upper TO UPPER CASE.
      IF lv_definition_upper EQ 'ENTITYREF'.
        add_entity_dependency(
          EXPORTING
            iv_entity       = lv_value
          CHANGING
            ct_dependencies = ct_dependencies
        ).
      ELSEIF lv_definition_upper EQ 'ELEMENTREF'
          OR lv_definition_upper EQ 'KEYELEMENTREF'
          OR lv_definition_upper EQ 'ASSOCIATIONREF'
          OR lv_definition_upper EQ 'PARAMETERREF'.
        CLEAR: lv_reference_entity, lv_reference_rest.
        SPLIT lv_value AT '.' INTO lv_reference_entity lv_reference_rest.
        IF lv_reference_rest IS NOT INITIAL.
          add_entity_dependency(
            EXPORTING
              iv_entity       = lv_reference_entity
            CHANGING
              ct_dependencies = ct_dependencies
          ).
        ENDIF.
      ELSEIF lv_annotation_name CP '*.ENTITY.NAME'.
        " Fallback for releases without the annotation definition API.
        add_entity_dependency(
          EXPORTING
            iv_entity       = lv_value
          CHANGING
            ct_dependencies = ct_dependencies
        ).
      ELSEIF lv_definition_upper IS INITIAL.
        " Older releases may have DDLX runtime data but no annotation
        " definition API. Only accept values that map to a real CDS source.
        CALL METHOD map_entity_to_ddls
          EXPORTING
            iv_entity    = lv_value
          RECEIVING
            rv_ddls_name = lv_mapped_ddls.
        IF lv_mapped_ddls IS NOT INITIAL.
          add_tadir_dependency(
            EXPORTING
              iv_object       = 'DDLS'
              iv_obj_name     = lv_mapped_ddls
            CHANGING
              ct_dependencies = ct_dependencies
          ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.


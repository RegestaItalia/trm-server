CLASS zcl_trm_senvi_map DEFINITION
  PUBLIC
  CREATE PROTECTED.

  PUBLIC SECTION.
    DATA: senvi  TYPE senvi READ-ONLY,
          origin TYPE REF TO zcl_trm_object READ-ONLY.

    METHODS constructor
      IMPORTING senvi  TYPE senvi
                origin TYPE REF TO zcl_trm_object.
    CLASS-METHODS get
      IMPORTING senvi      TYPE senvi
                origin     TYPE REF TO zcl_trm_object
      RETURNING VALUE(map) TYPE REF TO zcl_trm_senvi_map.
    METHODS map_dependencies
      CHANGING ct_dependencies TYPE ztrm_object_dependency_t.
  PROTECTED SECTION.
    DATA dependencies TYPE ztrm_object_dependency_t.
    METHODS determine
      RAISING zcx_trm_exception.
    CLASS-METHODS sanitize_object_name
      IMPORTING raw_name         TYPE any
                max_length       TYPE i
                no_gaps          TYPE flag DEFAULT 'X'
                no_dots          TYPE flag DEFAULT 'X'
      RETURNING VALUE(sanitized) TYPE string
      RAISING   zcx_trm_exception.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_trm_senvi_map IMPLEMENTATION.

  METHOD constructor.
    me->senvi = senvi.
    me->origin = origin.
    TRANSLATE me->senvi-object TO UPPER CASE.
    TRANSLATE me->senvi-encl_obj TO UPPER CASE.
    CONDENSE me->senvi-object.
    CONDENSE me->senvi-encl_obj.
  ENDMETHOD.

  METHOD get.
    DATA lv_class_name TYPE string.
    CONCATENATE 'ZCL_TRM_SENVI_' senvi-type INTO lv_class_name.
    TRANSLATE lv_class_name TO UPPER CASE.
    TRY.
        CREATE OBJECT map TYPE (lv_class_name)
          EXPORTING senvi = senvi
                    origin = origin.
      CATCH cx_dynamic_check.
        CREATE OBJECT map TYPE zcl_trm_senvi_map
          EXPORTING
            senvi  = senvi
            origin = origin.
    ENDTRY.
    TRY.
        map->determine( ).
      CATCH zcx_trm_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD determine.
    " for redefinition
  ENDMETHOD.

  METHOD map_dependencies.
    DATA ls_dependency LIKE LINE OF dependencies.
    LOOP AT dependencies INTO ls_dependency.
      READ TABLE ct_dependencies TRANSPORTING NO FIELDS WITH KEY tabname = ls_dependency-tabname tabkey = ls_dependency-tabkey.
      CHECK sy-subrc <> 0.
      APPEND ls_dependency TO ct_dependencies.
    ENDLOOP.
  ENDMETHOD.

  METHOD sanitize_object_name.
    DATA raw_copy TYPE string.
    raw_copy = raw_name.
    TRANSLATE raw_copy TO UPPER CASE.
    CONDENSE raw_copy.
    IF no_gaps EQ 'X'.
      CONDENSE raw_copy NO-GAPS.
    ENDIF.
    IF no_dots EQ 'X'.
      REPLACE ALL OCCURRENCES OF '.' IN raw_copy WITH ''.
    ENDIF.
    IF strlen( raw_copy ) <= max_length.
      sanitized = raw_copy.
    ELSE.
      zcx_trm_exception=>raise( iv_message = 'Object name ' && raw_copy && ' exceedes max. length ' && max_length ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

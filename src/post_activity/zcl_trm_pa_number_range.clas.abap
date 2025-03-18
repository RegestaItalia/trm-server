CLASS zcl_trm_pa_number_range DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_number_range,
             object    TYPE nrobj,
             subobject TYPE nrsobj,
             nrrangenr TYPE nrnr,
             toyear    TYPE nryear,
           END OF ty_number_range,
           BEGIN OF ty_nr_option,
             transport         TYPE abap_bool,
             transport_request TYPE trkorr,
           END OF ty_nr_option.

    METHODS constructor
      IMPORTING is_number_range TYPE ty_number_range.
    METHODS exists
      RETURNING VALUE(rv_exists) TYPE flag.

    CLASS-METHODS create_if_not_exists
      IMPORTING is_number_range    TYPE ty_number_range
                iv_fromnumber      TYPE nrfrom OPTIONAL
                iv_tonumber        TYPE nrto OPTIONAL
                iv_nrlevel         TYPE nrlevel OPTIONAL
                iv_externind       TYPE nrind OPTIONAL
                iv_procind         TYPE procind OPTIONAL
                is_option          TYPE ty_nr_option OPTIONAL
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_trm_pa_number_range
      RAISING   zcx_trm_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: gs_number_range TYPE ty_number_range.
ENDCLASS.



CLASS zcl_trm_pa_number_range IMPLEMENTATION.

  METHOD constructor.
    gs_number_range = is_number_range.
  ENDMETHOD.

  METHOD exists.
    DATA ls_nriv TYPE nriv.
    SELECT SINGLE *
      FROM nriv
      INTO ls_nriv
      WHERE object EQ gs_number_range-object
        AND subobject EQ gs_number_range-subobject
        AND nrrangenr EQ gs_number_range-nrrangenr
        AND toyear EQ gs_number_range-toyear.
    IF sy-subrc EQ 0.
      rv_exists = 'X'.
    ENDIF.
  ENDMETHOD.

  METHOD create_if_not_exists.
    DATA: lt_interval      TYPE lcl_numberrange_intervals=>nr_interval,
          ls_interval      LIKE LINE OF lt_interval,
          lv_object        TYPE lcl_numberrange_intervals=>nr_object,
          lv_subobject     TYPE lcl_numberrange_intervals=>nr_subobject,
          ls_option        TYPE lcl_numberrange_intervals=>nr_option,
          lv_error         TYPE lcl_numberrange_intervals=>nr_error,
          lv_error_message TYPE string.
    CREATE OBJECT ro_instance
      EXPORTING
        is_number_range = is_number_range.
    CHECK ro_instance->exists( ) <> 'X'.
    ls_interval-subobject = is_number_range-subobject.
    ls_interval-nrrangenr = is_number_range-nrrangenr.
    ls_interval-toyear = is_number_range-toyear.
    ls_interval-fromnumber = iv_fromnumber.
    ls_interval-tonumber = iv_tonumber.
    ls_interval-nrlevel = iv_nrlevel.
    ls_interval-externind = iv_externind.
    ls_interval-procind = iv_procind.
    APPEND ls_interval TO lt_interval.
    lv_object = is_number_range-object.
    lv_subobject = is_number_range-subobject.
    MOVE-CORRESPONDING is_option TO ls_option.
    lcl_numberrange_intervals=>create(
      EXPORTING
        interval  = lt_interval
        object    = lv_object
        subobject = lv_subobject
        option    = ls_option
      IMPORTING
        error     = lv_error
    ).
    IF lv_error EQ 'X'.
      CONCATENATE `Can't generate number range` lv_object lv_subobject INTO lv_error_message SEPARATED BY space.
      zcx_trm_exception=>raise(
      EXPORTING
        iv_reason  = zcx_trm_exception=>c_reason-generic
        iv_message = lv_error_message
    ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

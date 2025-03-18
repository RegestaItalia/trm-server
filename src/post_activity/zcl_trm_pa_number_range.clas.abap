CLASS zcl_trm_pa_number_range DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS trm_pa TYPE flag VALUE 'X' ##NO_TEXT.

    CLASS-METHODS execute
      IMPORTING
        !object            TYPE nrobj
        !subobject         TYPE nrsobj
        !nrrangenr         TYPE nrnr
        !toyear            TYPE nryear
        !fromnumber        TYPE nrfrom OPTIONAL
        !tonumber          TYPE nrto OPTIONAL
        !nrlevel           TYPE nrlevel OPTIONAL
        !externind         TYPE nrind OPTIONAL
        !procind           TYPE procind OPTIONAL
        !transport_request TYPE trkorr OPTIONAL
      EXPORTING
        !messages          TYPE symsg_tab
      RAISING
        zcx_trm_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_pa_number_range IMPLEMENTATION.


  METHOD execute.
    DATA: ls_nriv          TYPE nriv,
          lt_interval      TYPE lcl_numberrange_intervals=>nr_interval,
          ls_interval      LIKE LINE OF lt_interval,
          lv_object        TYPE lcl_numberrange_intervals=>nr_object,
          lv_subobject     TYPE lcl_numberrange_intervals=>nr_subobject,
          ls_option        TYPE lcl_numberrange_intervals=>nr_option,
          lv_error         TYPE lcl_numberrange_intervals=>nr_error,
          lv_error_message TYPE string.

    SELECT SINGLE *
      FROM nriv
      INTO ls_nriv
      WHERE object EQ object
        AND subobject EQ subobject
        AND nrrangenr EQ nrrangenr
        AND toyear EQ toyear.
    CHECK sy-subrc <> 0.

    ls_interval-subobject = subobject.
    ls_interval-nrrangenr = nrrangenr.
    ls_interval-toyear = toyear.
    ls_interval-fromnumber = fromnumber.
    ls_interval-tonumber = tonumber.
    ls_interval-nrlevel = nrlevel.
    ls_interval-externind = externind.
    ls_interval-procind = procind.
    APPEND ls_interval TO lt_interval.
    lv_object = object.
    lv_subobject = subobject.
    IF transport_request IS NOT INITIAL.
      ls_option-transport = 'X'.
      ls_option-transport_request = transport_request.
    ENDIF.
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

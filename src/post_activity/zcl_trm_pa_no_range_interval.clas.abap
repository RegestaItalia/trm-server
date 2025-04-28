CLASS zcl_trm_pa_no_range_interval DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS trm_pa TYPE flag VALUE 'X' ##NO_TEXT.

    TYPES: ty_nrobj   TYPE c LENGTH 10,
           ty_nrsobj  TYPE c LENGTH 6,
           ty_nrnr    TYPE c LENGTH 2,
           ty_nryear  TYPE n LENGTH 4,
           ty_nrfrom  TYPE c LENGTH 20,
           ty_nrto    TYPE c LENGTH 20,
           ty_nrlevel TYPE n LENGTH 20,
           ty_nrind   TYPE c LENGTH 1,
           ty_procind TYPE c LENGTH 1.

    CLASS-METHODS execute
      IMPORTING
        !object            TYPE ty_nrobj
        !subobject         TYPE ty_nrsobj
        !nrrangenr         TYPE ty_nrnr
        !toyear            TYPE ty_nryear
        !fromnumber        TYPE ty_nrfrom OPTIONAL
        !tonumber          TYPE ty_nrto OPTIONAL
        !nrlevel           TYPE ty_nrlevel OPTIONAL
        !externind         TYPE ty_nrind OPTIONAL
        !procind           TYPE ty_procind OPTIONAL
        !transport_request TYPE trkorr OPTIONAL
      EXPORTING
        !messages          TYPE symsg_tab
      RAISING
        zcx_trm_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_pa_no_range_interval IMPLEMENTATION.

  METHOD execute.
    DATA: ls_nriv      TYPE nriv,
          lt_interval  TYPE lcl_numberrange_intervals=>nr_interval,
          ls_interval  LIKE LINE OF lt_interval,
          lv_object    TYPE lcl_numberrange_intervals=>nr_object,
          lv_subobject TYPE lcl_numberrange_intervals=>nr_subobject,
          ls_option    TYPE lcl_numberrange_intervals=>nr_option,
          lv_error     TYPE lcl_numberrange_intervals=>nr_error,
          lv_message   TYPE string,
          ls_message   LIKE LINE OF messages.

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
      CONCATENATE `Can't generate number range` lv_object lv_subobject `interval` INTO lv_message SEPARATED BY space.
      zcx_trm_exception=>raise(
      EXPORTING
        iv_reason  = zcx_trm_exception=>c_reason-generic
        iv_message = lv_message
    ).
    ELSE.
      CONCATENATE 'Generated number range ' lv_object lv_subobject 'interval' INTO lv_message SEPARATED BY space.
      CONDENSE lv_message.
      cl_message_helper=>set_msg_vars_for_clike( lv_message ).
      MOVE-CORRESPONDING sy TO ls_message.
      ls_message-msgty = 'S'.
      APPEND ls_message TO messages.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

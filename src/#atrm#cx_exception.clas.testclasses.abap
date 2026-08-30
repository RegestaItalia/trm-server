CLASS ltc_exception DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS default_reason FOR TESTING.
    METHODS supplied_reason_and_log FOR TESTING.
ENDCLASS.

CLASS ltc_exception IMPLEMENTATION.
  METHOD default_reason.
    DATA lo_exception TYPE REF TO /atrm/cx_exception.
    TRY.
        /atrm/cx_exception=>raise( ).
      CATCH /atrm/cx_exception INTO lo_exception.
    ENDTRY.
    cl_abap_unit_assert=>assert_bound( act = lo_exception ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_exception->reason( )
      exp = /atrm/cx_exception=>c_reason-generic
    ).
  ENDMETHOD.

  METHOD supplied_reason_and_log.
    DATA: lo_exception TYPE REF TO /atrm/cx_exception,
          lt_log       TYPE /atrm/cx_exception=>tyt_log,
          lt_actual    TYPE /atrm/cx_exception=>tyt_log.
    APPEND 'first diagnostic line' TO lt_log.
    TRY.
        /atrm/cx_exception=>raise(
          iv_reason = /atrm/cx_exception=>c_reason-invalid_input
          it_log    = lt_log
        ).
      CATCH /atrm/cx_exception INTO lo_exception.
    ENDTRY.
    cl_abap_unit_assert=>assert_bound( act = lo_exception ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_exception->reason( )
      exp = /atrm/cx_exception=>c_reason-invalid_input
    ).
    lt_actual = lo_exception->log( ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_actual
      exp = lt_log
    ).
  ENDMETHOD.
ENDCLASS.

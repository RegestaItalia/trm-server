CLASS ltc_transport DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS constructor_keeps_request FOR TESTING.
    METHODS multiple_rejects_empty_input FOR TESTING.
ENDCLASS.

CLASS ltc_transport IMPLEMENTATION.
  METHOD constructor_keeps_request.
    DATA: lo_transport TYPE REF TO /atrm/cl_transport,
          lv_actual    TYPE trkorr,
          lv_expected  TYPE trkorr.
    lv_expected = 'DEVK900001'.
    CREATE OBJECT lo_transport
      EXPORTING
        trkorr = lv_expected.
    lv_actual = lo_transport->get_trkorr( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_actual
      exp = lv_expected
    ).
  ENDMETHOD.

  METHOD multiple_rejects_empty_input.
    DATA: lt_transports TYPE /atrm/cl_transport=>tyt_trkorr,
          lt_imports    TYPE stms_tp_imports,
          lo_exception  TYPE REF TO /atrm/cx_exception,
          lv_system     TYPE tmssysnam.
    lv_system = sy-sysid.
    TRY.
        lt_imports = /atrm/cl_transport=>import_multiple(
          system     = lv_system
          transports = lt_transports
        ).
      CATCH /atrm/cx_exception INTO lo_exception.
    ENDTRY.
    cl_abap_unit_assert=>assert_bound( act = lo_exception ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_exception->reason( )
      exp = /atrm/cx_exception=>c_reason-invalid_input
    ).
  ENDMETHOD.
ENDCLASS.

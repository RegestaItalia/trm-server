CLASS ltc_transport DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS constructor_keeps_request FOR TESTING.
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
ENDCLASS.

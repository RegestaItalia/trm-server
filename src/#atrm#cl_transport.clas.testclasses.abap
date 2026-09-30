CLASS ltc_transport DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS constructor_keeps_request FOR TESTING.
    METHODS shi3_untouched_without_shi3 FOR TESTING RAISING /atrm/cx_exception.
    METHODS shi3_unknown_structure_raises FOR TESTING.
    METHODS fdt0_ignores_other_objects FOR TESTING RAISING /atrm/cx_exception.
ENDCLASS.

CLASS /atrm/cl_transport DEFINITION LOCAL FRIENDS ltc_transport.

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

  METHOD shi3_untouched_without_shi3.
    DATA: lo_transport TYPE REF TO /atrm/cl_transport,
          lt_e071      TYPE /atrm/cl_transport=>tyt_e071,
          lt_expected  TYPE /atrm/cl_transport=>tyt_e071,
          lt_e071k     TYPE /atrm/cl_transport=>tyt_e071k,
          ls_e071      TYPE e071.
    CREATE OBJECT lo_transport
      EXPORTING
        trkorr = 'DEVK900001'.
    ls_e071-pgmid = 'R3TR'.
    ls_e071-object = 'TRAN'.
    ls_e071-obj_name = 'ZTEST'.
    APPEND ls_e071 TO lt_e071.
    ls_e071-object = 'DEVC'.
    ls_e071-obj_name = 'ZPACKAGE'.
    APPEND ls_e071 TO lt_e071.
    lt_expected = lt_e071.
    lo_transport->complete_shi3_entries( CHANGING e071 = lt_e071 e071k = lt_e071k ).
    cl_abap_unit_assert=>assert_equals( act = lt_e071 exp = lt_expected ).
    cl_abap_unit_assert=>assert_initial( lt_e071k ).
  ENDMETHOD.

  METHOD shi3_unknown_structure_raises.
    DATA: lo_transport TYPE REF TO /atrm/cl_transport,
          lt_e071      TYPE /atrm/cl_transport=>tyt_e071,
          lt_e071k     TYPE /atrm/cl_transport=>tyt_e071k,
          ls_e071      TYPE e071.
    CREATE OBJECT lo_transport
      EXPORTING
        trkorr = 'DEVK900001'.
    ls_e071-pgmid = 'R3TR'.
    ls_e071-object = 'SHI3'.
    ls_e071-obj_name = 'ZATRM_UNIT_NOT_EXISTING_SHI3'.
    APPEND ls_e071 TO lt_e071.
    TRY.
        lo_transport->complete_shi3_entries( CHANGING e071 = lt_e071 e071k = lt_e071k ).
        cl_abap_unit_assert=>fail( 'Exception expected for unknown structure' ).
      CATCH /atrm/cx_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD fdt0_ignores_other_objects.
    DATA: lo_transport TYPE REF TO /atrm/cl_transport,
          lt_e071      TYPE /atrm/cl_transport=>tyt_e071,
          ls_e071      TYPE e071.
    CREATE OBJECT lo_transport
      EXPORTING
        trkorr = 'DEVK900001'.
    ls_e071-pgmid = 'R3TR'.
    ls_e071-object = 'TRAN'.
    ls_e071-obj_name = 'ZTEST'.
    APPEND ls_e071 TO lt_e071.
    ls_e071-object = 'FDT0'.
    ls_e071-obj_name = 'ZATRM_UNIT_NOT_EXISTING_FDT0'.
    APPEND ls_e071 TO lt_e071.
    " no BRF+ application with that name: nothing to record, no exception
    lo_transport->record_fdt0_entries( lt_e071 ).
  ENDMETHOD.
ENDCLASS.

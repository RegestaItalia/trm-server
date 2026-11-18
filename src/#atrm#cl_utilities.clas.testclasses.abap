CLASS ltc_utilities DEFINITION FOR TESTING
  "#AU Risk_Level Harmless
  "#AU Duration Short
.
  PRIVATE SECTION.
    METHODS http_base_url_unknown_service FOR TESTING.
ENDCLASS.

CLASS ltc_utilities IMPLEMENTATION.

  METHOD http_base_url_unknown_service.
    " Replaces the string template |ICF service { lv_url } not found or inactive|;
    " the service path is normalized (leading slash, lower case) first
    DATA lo_exception TYPE REF TO /atrm/cx_exception.
    TRY.
        /atrm/cl_utilities=>get_http_base_url( service = 'ZZ_TRM_NONE' ).
      CATCH /atrm/cx_exception INTO lo_exception.
    ENDTRY.
    cl_aunit_assert=>assert_bound( act = lo_exception msg = 'Exception expected' ).
    cl_aunit_assert=>assert_equals(
      act = lo_exception->message-msgv1
      exp = 'ICF service /zz_trm_none not found or inactive' ).
  ENDMETHOD.

ENDCLASS.

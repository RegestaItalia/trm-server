CLASS ltc_object DEFINITION DEFERRED.
CLASS /atrm/cl_object DEFINITION LOCAL FRIENDS ltc_object.

CLASS ltc_object DEFINITION FOR TESTING
  "#AU Risk_Level Harmless
  "#AU Duration Short
.
  PRIVATE SECTION.
    METHODS tadir_missing_message FOR TESTING.
    METHODS tfdir_missing_message FOR TESTING.
    METHODS utf8_decoded FOR TESTING RAISING cx_static_check.
    METHODS utf8_empty FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_object IMPLEMENTATION.

  METHOD tadir_missing_message.
    " Message built with CONCATENATE ... SEPARATED BY space (no && on 7.00)
    DATA lo_exception TYPE REF TO /atrm/cx_exception.
    TRY.
        /atrm/cl_object=>get_tadir_dependency(
          object   = 'PROG'
          obj_name = '/ATRM/ZZ_NOT_EXISTING' ).
      CATCH /atrm/cx_exception INTO lo_exception.
    ENDTRY.
    cl_aunit_assert=>assert_bound( act = lo_exception msg = 'Exception expected' ).
    cl_aunit_assert=>assert_equals(
      act = lo_exception->message-msgv1
      exp = 'R3TR PROG /ATRM/ZZ_NOT_EXISTING is not in TADIR' ).
  ENDMETHOD.

  METHOD tfdir_missing_message.
    DATA lo_exception TYPE REF TO /atrm/cx_exception.
    TRY.
        /atrm/cl_object=>get_tfdir_dependency( funcname = '/ATRM/ZZ_NONE' ).
      CATCH /atrm/cx_exception INTO lo_exception.
    ENDTRY.
    cl_aunit_assert=>assert_bound( act = lo_exception msg = 'Exception expected' ).
    cl_aunit_assert=>assert_equals(
      act = lo_exception->message-msgv1
      exp = 'Cannot find function group of /ATRM/ZZ_NONE' ).
  ENDMETHOD.

  METHOD utf8_decoded.
    " Replaces CL_ABAP_CODEPAGE=>CONVERT_FROM (not on 7.00): UTF-8 multi-byte
    " characters must be decoded, '<A>' + e acute (C3A9) + '</A>'
    DATA: lv_utf8     TYPE xstring,
          lv_actual   TYPE string,
          lv_expected TYPE string,
          lv_char     TYPE string.
    lv_utf8 = '3C413EC3A93C2F413E'.
    lv_actual = /atrm/cl_object=>utf8_to_string( lv_utf8 ).
    lv_char = cl_abap_conv_in_ce=>uccp( '00E9' ).
    CONCATENATE '<A>' lv_char '</A>' INTO lv_expected.
    cl_aunit_assert=>assert_equals( act = lv_actual exp = lv_expected ).
  ENDMETHOD.

  METHOD utf8_empty.
    DATA: lv_utf8   TYPE xstring,
          lv_actual TYPE string.
    lv_actual = /atrm/cl_object=>utf8_to_string( lv_utf8 ).
    cl_aunit_assert=>assert_initial( act = lv_actual ).
  ENDMETHOD.

ENDCLASS.

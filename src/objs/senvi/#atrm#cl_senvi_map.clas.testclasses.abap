CLASS ltc_senvi_map DEFINITION DEFERRED.
CLASS /atrm/cl_senvi_map DEFINITION LOCAL FRIENDS ltc_senvi_map.

CLASS ltc_senvi_map DEFINITION FOR TESTING
  "#AU Risk_Level Harmless
  "#AU Duration Short
.
  PRIVATE SECTION.
    METHODS sanitize_within_length FOR TESTING.
    METHODS sanitize_too_long_message FOR TESTING.
ENDCLASS.

CLASS ltc_senvi_map IMPLEMENTATION.

  METHOD sanitize_within_length.
    DATA lv_sanitized TYPE string.
    TRY.
        lv_sanitized = /atrm/cl_senvi_map=>sanitize_object_name(
          raw_name   = ' zab.c d '
          max_length = 10 ).
      CATCH /atrm/cx_exception.
        cl_aunit_assert=>fail( msg = 'Unexpected exception' ).
    ENDTRY.
    cl_aunit_assert=>assert_equals( act = lv_sanitized exp = 'ZABCD' ).
  ENDMETHOD.

  METHOD sanitize_too_long_message.
    " The message is built with CONCATENATE (no && on 7.00): the integer
    " length must be rendered without blanks between the words
    DATA lo_exception TYPE REF TO /atrm/cx_exception.
    TRY.
        /atrm/cl_senvi_map=>sanitize_object_name(
          raw_name   = 'ABCDEF'
          max_length = 3 ).
      CATCH /atrm/cx_exception INTO lo_exception.
    ENDTRY.
    cl_aunit_assert=>assert_bound( act = lo_exception msg = 'Exception expected' ).
    cl_aunit_assert=>assert_equals(
      act = lo_exception->message-msgv1
      exp = 'Object name ABCDEF exceedes max. length 3' ).
  ENDMETHOD.

ENDCLASS.

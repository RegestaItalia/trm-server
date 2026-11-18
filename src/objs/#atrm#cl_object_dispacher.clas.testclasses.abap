CLASS ltc_dispacher DEFINITION DEFERRED.
CLASS /atrm/cl_object_dispacher DEFINITION LOCAL FRIENDS ltc_dispacher.

CLASS ltc_dispacher DEFINITION FOR TESTING
  "#AU Risk_Level Harmless
  "#AU Duration Short
.
  PRIVATE SECTION.
    METHODS reading_message FOR TESTING.
    METHODS progress_message FOR TESTING.
    METHODS progress_message_zero FOR TESTING.
ENDCLASS.

CLASS ltc_dispacher IMPLEMENTATION.

  METHOD reading_message.
    " Replaces the string template |Reading "{ package }" objects|
    DATA lv_message TYPE /atrm/polling_last_msg.
    lv_message = /atrm/cl_object_dispacher=>reading_message( '/ATRM/SERVER' ).
    cl_aunit_assert=>assert_equals(
      act = lv_message
      exp = 'Reading "/ATRM/SERVER" objects' ).
  ENDMETHOD.

  METHOD progress_message.
    " Replaces |Finding dependencies ({ lv_percentagep }%)|: one decimal,
    " rounded, no blanks around the number
    DATA: lv_percentage TYPE p LENGTH 16 DECIMALS 4 VALUE '12.3456',
          lv_message    TYPE /atrm/polling_last_msg.
    lv_message = /atrm/cl_object_dispacher=>progress_message( lv_percentage ).
    cl_aunit_assert=>assert_equals(
      act = lv_message
      exp = 'Finding dependencies (12.3%)' ).
  ENDMETHOD.

  METHOD progress_message_zero.
    DATA: lv_percentage TYPE p LENGTH 16 DECIMALS 4,
          lv_message    TYPE /atrm/polling_last_msg.
    lv_message = /atrm/cl_object_dispacher=>progress_message( lv_percentage ).
    cl_aunit_assert=>assert_equals(
      act = lv_message
      exp = 'Finding dependencies (0.0%)' ).
  ENDMETHOD.

ENDCLASS.

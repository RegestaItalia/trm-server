CLASS ltc_exception DEFINITION FOR TESTING
  "#AU Risk_Level Harmless
  "#AU Duration Short
.
  PRIVATE SECTION.
    METHODS default_reason FOR TESTING.
    METHODS supplied_reason_and_log FOR TESTING.
    METHODS message_from_sy FOR TESTING.
ENDCLASS.

CLASS ltc_exception IMPLEMENTATION.
  METHOD default_reason.
    DATA: lo_exception TYPE REF TO /atrm/cx_exception,
          lv_reason    TYPE string.
    TRY.
        /atrm/cx_exception=>raise( ).
      CATCH /atrm/cx_exception INTO lo_exception.
    ENDTRY.
    cl_aunit_assert=>assert_bound( act = lo_exception ).
    lv_reason = lo_exception->reason( ).
    cl_aunit_assert=>assert_equals(
      act = lv_reason
      exp = /atrm/cx_exception=>c_reason-generic
    ).
  ENDMETHOD.

  METHOD supplied_reason_and_log.
    " log( ) returns the supplied log followed by the exception stack:
    " only the supplied part is checked, the stack depends on the caller
    DATA: lo_exception TYPE REF TO /atrm/cx_exception,
          lt_log       TYPE /atrm/cx_exception=>tyt_log,
          lt_actual    TYPE /atrm/cx_exception=>tyt_log,
          lv_line      TYPE tdline,
          lv_reason    TYPE string.
    APPEND 'first diagnostic line' TO lt_log.
    TRY.
        /atrm/cx_exception=>raise(
          iv_reason = /atrm/cx_exception=>c_reason-invalid_input
          it_log    = lt_log
        ).
      CATCH /atrm/cx_exception INTO lo_exception.
    ENDTRY.
    cl_aunit_assert=>assert_bound( act = lo_exception ).
    lv_reason = lo_exception->reason( ).
    cl_aunit_assert=>assert_equals(
      act = lv_reason
      exp = /atrm/cx_exception=>c_reason-invalid_input
    ).
    lt_actual = lo_exception->log( ).
    READ TABLE lt_actual INTO lv_line INDEX 1.
    cl_aunit_assert=>assert_subrc( act = sy-subrc exp = 0 ).
    cl_aunit_assert=>assert_equals(
      act = lv_line
      exp = 'first diagnostic line'
    ).
  ENDMETHOD.

  METHOD message_from_sy.
    " Guards the hand-written constructor: it must copy the sy message
    " into MESSAGE and IF_T100_MESSAGE~T100KEY. A constructor regenerated
    " by the class editor drops this and fails here.
    DATA: lo_exception TYPE REF TO /atrm/cx_exception,
          lv_dummy     TYPE string.
    MESSAGE ID '00' TYPE 'I' NUMBER '001' WITH 'V1' 'V2' 'V3' 'V4' INTO lv_dummy.
    TRY.
        /atrm/cx_exception=>raise( ).
      CATCH /atrm/cx_exception INTO lo_exception.
    ENDTRY.
    cl_aunit_assert=>assert_bound( act = lo_exception ).
    cl_aunit_assert=>assert_equals( act = lo_exception->message-msgid exp = '00' ).
    cl_aunit_assert=>assert_equals( act = lo_exception->message-msgno exp = '001' ).
    cl_aunit_assert=>assert_equals( act = lo_exception->message-msgv1 exp = 'V1' ).
    cl_aunit_assert=>assert_equals( act = lo_exception->message-msgv4 exp = 'V4' ).
    cl_aunit_assert=>assert_equals( act = lo_exception->if_t100_message~t100key-msgid exp = '00' ).
    cl_aunit_assert=>assert_equals( act = lo_exception->if_t100_message~t100key-msgno exp = '001' ).
    cl_aunit_assert=>assert_equals( act = lo_exception->if_t100_message~t100key-attr1 exp = 'V1' ).
  ENDMETHOD.
ENDCLASS.

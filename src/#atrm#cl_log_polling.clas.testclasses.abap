CLASS ltc_log_polling DEFINITION DEFERRED.
CLASS /atrm/cl_log_polling DEFINITION LOCAL FRIENDS ltc_log_polling.

CLASS ltc_log_polling DEFINITION FOR TESTING
  "#AU Risk_Level Harmless
  "#AU Duration Short
.
  PRIVATE SECTION.
    METHODS new_id_is_unique FOR TESTING.
ENDCLASS.

CLASS ltc_log_polling IMPLEMENTATION.

  METHOD new_id_is_unique.
    " IDs come from GUID_CREATE (no CL_SYSTEM_UUID=>*_STATIC on 7.00)
    DATA: lv_first  TYPE /atrm/polling_id,
          lv_second TYPE /atrm/polling_id.
    lv_first = /atrm/cl_log_polling=>new_id( ).
    lv_second = /atrm/cl_log_polling=>new_id( ).
    cl_aunit_assert=>assert_not_initial( act = lv_first msg = 'Polling ID expected' ).
    IF lv_first = lv_second.
      cl_aunit_assert=>fail( msg = 'Two calls returned the same polling ID' ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

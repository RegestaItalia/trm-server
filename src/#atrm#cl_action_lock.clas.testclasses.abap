CLASS ltc_action_lock DEFINITION FOR TESTING
  "#AU Risk_Level Dangerous
  "#AU Duration Short
.
  PRIVATE SECTION.
    METHODS conflict_and_release FOR TESTING.
ENDCLASS.

CLASS ltc_action_lock IMPLEMENTATION.
  METHOD conflict_and_release.
    DATA: lt_keys TYPE /atrm/cl_action_lock=>tyt_keys,
          ls_key TYPE /atrm/act_lock_k,
          ls_row TYPE /atrm/act_lock,
          lv_owner TYPE /atrm/act_lock_owner_token,
          lv_other TYPE /atrm/act_lock_owner_token,
          lv_conflict TYPE abap_bool.

    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_32 = lv_owner.
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_32 = lv_other.
    cl_aunit_assert=>assert_not_initial( act = lv_owner msg = 'Could not create test lock tokens' ).
    IF lv_owner = lv_other.
      cl_aunit_assert=>fail( msg = 'GUID_CREATE returned the same token twice' ).
    ENDIF.
    ls_key-resource_type = 'TEST'.
    ls_key-resource_hash = lv_owner.
    ls_key-resource_name = 'ABAP Unit action lock'.
    APPEND ls_key TO lt_keys.

    TRY.
        /atrm/cl_action_lock=>acquire(
          it_keys = lt_keys
          iv_owner_token = lv_owner
          iv_action_name = 'ABAP Unit' ).
        TRY.
            /atrm/cl_action_lock=>acquire(
              it_keys = lt_keys
              iv_owner_token = lv_other
              iv_action_name = 'ABAP Unit' ).
          CATCH /atrm/cx_exception.
            lv_conflict = abap_true.
        ENDTRY.
        SELECT SINGLE * FROM /atrm/act_lock INTO ls_row
          WHERE resource_type = ls_key-resource_type
            AND resource_hash = ls_key-resource_hash.
        cl_aunit_assert=>assert_equals( act = lv_conflict exp = abap_true ).
        cl_aunit_assert=>assert_equals( act = ls_row-owner_token exp = lv_owner ).
        /atrm/cl_action_lock=>release(
          it_keys = lt_keys
          iv_owner_token = lv_owner ).
        CLEAR ls_row.
        SELECT SINGLE * FROM /atrm/act_lock INTO ls_row
          WHERE resource_type = ls_key-resource_type
            AND resource_hash = ls_key-resource_hash.
        cl_aunit_assert=>assert_subrc( act = sy-subrc exp = 4 ).
      CATCH /atrm/cx_exception.
        TRY.
            /atrm/cl_action_lock=>release(
              it_keys = lt_keys
              iv_owner_token = lv_owner ).
          CATCH /atrm/cx_exception.
        ENDTRY.
        cl_aunit_assert=>fail( msg = 'Action lock operation failed' ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS ltc_lock_table DEFINITION FOR TESTING
  "#AU Risk_Level Harmless
  "#AU Duration Short
.
  PRIVATE SECTION.
    METHODS no_client_field FOR TESTING.
ENDCLASS.

CLASS ltc_lock_table IMPLEMENTATION.
  METHOD no_client_field.
    DATA lv_fieldname TYPE dd03l-fieldname.
    SELECT SINGLE fieldname FROM dd03l INTO lv_fieldname
      WHERE tabname = '/ATRM/ACT_LOCK'
        AND fieldname = 'MANDT'
        AND as4local = 'A'.
    cl_aunit_assert=>assert_subrc( act = sy-subrc exp = 4 ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_action_lock DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL DANGEROUS.
  PRIVATE SECTION.
    METHODS conflict_and_release FOR TESTING.
ENDCLASS.

CLASS ltc_action_lock IMPLEMENTATION.
  METHOD conflict_and_release.
    DATA: lt_keys TYPE /atrm/cl_action_lock=>tyt_keys,
          ls_key TYPE /atrm/act_lock_k,
          ls_row TYPE /atrm/act_lock,
          lv_owner TYPE sysuuid_c32,
          lv_other TYPE sysuuid_c32,
          lv_conflict TYPE abap_bool.

    TRY.
        lv_owner = cl_system_uuid=>create_uuid_c32_static( ).
        lv_other = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error.
        cl_abap_unit_assert=>fail( msg = 'Could not create test lock tokens' ).
    ENDTRY.
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
        cl_abap_unit_assert=>assert_equals( act = lv_conflict exp = abap_true ).
        cl_abap_unit_assert=>assert_equals( act = ls_row-owner_token exp = lv_owner ).
        /atrm/cl_action_lock=>release(
          it_keys = lt_keys
          iv_owner_token = lv_owner ).
        CLEAR ls_row.
        SELECT SINGLE * FROM /atrm/act_lock INTO ls_row
          WHERE resource_type = ls_key-resource_type
            AND resource_hash = ls_key-resource_hash.
        cl_abap_unit_assert=>assert_subrc( exp = 4 ).
      CATCH /atrm/cx_exception.
        TRY.
            /atrm/cl_action_lock=>release(
              it_keys = lt_keys
              iv_owner_token = lv_owner ).
          CATCH /atrm/cx_exception.
        ENDTRY.
        cl_abap_unit_assert=>fail( msg = 'Action lock operation failed' ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS ltc_lock_table DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
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
    cl_abap_unit_assert=>assert_subrc( exp = 4 ).
  ENDMETHOD.
ENDCLASS.

FUNCTION ztrm_add_objs_tr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"     VALUE(IV_LOCK) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     VALUE(ET_LOG) TYPE  SPROT_U_TAB
*"  TABLES
*"      IT_E071 STRUCTURE  E071
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      zcl_trm_transport=>add_objects(
        EXPORTING
          iv_lock   = iv_lock
          iv_trkorr = iv_trkorr
          it_e071   = it_e071[]
        IMPORTING
          et_log    = et_log
      ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.
ENDFUNCTION.

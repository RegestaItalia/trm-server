FUNCTION ztrm_get_obj_lock_tr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PGMID) TYPE  PGMID
*"     VALUE(IV_OBJECT) TYPE  TROBJTYPE
*"     VALUE(IV_OBJ_NAME) TYPE  TROBJ_NAME
*"  EXPORTING
*"     VALUE(EV_TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      zcl_trm_transport=>find_object_lock(
        EXPORTING
          iv_pgmid    = iv_pgmid
          iv_object   = iv_object
          iv_obj_name = iv_obj_name
        RECEIVING
          ro_transport = lo_transport
      ).
      CHECK lo_transport IS BOUND.
      ev_trkorr = lo_transport->get_trkorr( ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

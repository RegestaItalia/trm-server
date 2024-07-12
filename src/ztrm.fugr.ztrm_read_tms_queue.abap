FUNCTION ztrm_read_tms_queue.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TARGET) TYPE  TMSSYSNAM
*"  EXPORTING
*"     VALUE(ET_REQUESTS) TYPE  TMSIQREQS
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      TMS_ALERT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      zcl_trm_transport=>read_queue(
        EXPORTING
          iv_target   = iv_target
        IMPORTING
          et_requests = et_requests
      ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

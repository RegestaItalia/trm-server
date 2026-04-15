FUNCTION /atrm/read_tms_queue.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TARGET) TYPE  TMSSYSNAM
*"  EXPORTING
*"     VALUE(REQUESTS) TYPE  TMSIQREQS
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      TMS_ALERT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    /atrm/cl_transport=>read_queue(
      EXPORTING
        target   = target
      IMPORTING
        requests = requests
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

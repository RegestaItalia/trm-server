FUNCTION /ATRM/READ_TMS_QUEUE.
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
    /ATRM/CL_TRANSPORT=>read_queue(
      EXPORTING
        target   = target
      IMPORTING
        requests = requests
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

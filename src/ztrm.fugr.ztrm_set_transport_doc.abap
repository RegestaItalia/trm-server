FUNCTION ztrm_set_transport_doc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"  TABLES
*"      IT_DOC STRUCTURE  TLINE
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      ENQUEUE_ERROR
*"      DEQUEUE_ERROR
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT lo_transport EXPORTING iv_trkorr = iv_trkorr.
    lo_transport->set_documentation(
      EXPORTING
        it_doc    = it_doc[]
    ).
  CATCH zcx_trm_exception INTO lo_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

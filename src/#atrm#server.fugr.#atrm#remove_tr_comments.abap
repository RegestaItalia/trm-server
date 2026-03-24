FUNCTION /ATRM/REMOVE_TR_COMMENTS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TRKORR) TYPE  TRKORR
*"     VALUE(OBJECT) TYPE  TROBJTYPE
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      CREATE OBJECT go_transport EXPORTING trkorr = trkorr.
      go_transport->remove_comments( object = object ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

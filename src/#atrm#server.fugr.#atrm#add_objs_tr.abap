FUNCTION /atrm/add_objs_tr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TRKORR) TYPE  TRKORR
*"     VALUE(LOCK) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     VALUE(LOG) TYPE  SPROT_U_TAB
*"  TABLES
*"      E071 STRUCTURE  E071
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT go_transport EXPORTING trkorr = trkorr.
    go_transport->add_objects(
      EXPORTING
        lock   = lock
        e071   = e071[]
      IMPORTING
        log    = log
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

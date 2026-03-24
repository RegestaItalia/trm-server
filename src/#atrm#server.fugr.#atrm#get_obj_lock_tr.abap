FUNCTION /ATRM/GET_OBJ_LOCK_TR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PGMID) TYPE  PGMID
*"     VALUE(OBJECT) TYPE  TROBJTYPE
*"     VALUE(OBJ_NAME) TYPE  TROBJ_NAME
*"  EXPORTING
*"     VALUE(TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      /ATRM/CL_TRANSPORT=>find_object_lock(
        EXPORTING
          pgmid    = pgmid
          object   = object
          obj_name = obj_name
        RECEIVING
          transport = go_transport
      ).
      CHECK go_transport IS BOUND.
      trkorr = go_transport->get_trkorr( ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

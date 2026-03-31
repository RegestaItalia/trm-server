FUNCTION /ATRM/GET_OBJS_LOCKS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(LOCKS) TYPE  /ATRM/OBJECT_LOCK_T
*"  TABLES
*"      OBJECTS STRUCTURE  TADIR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    locks = /ATRM/CL_UTILITIES=>get_objs_locks(
      EXPORTING
        objects = objects[]
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

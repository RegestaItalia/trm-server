FUNCTION /ATRM/GET_PACKAGE_OBJ_LOCKS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DEVCLASS) TYPE  DEVCLASS
*"  EXPORTING
*"     VALUE(LOCKS) TYPE  /ATRM/OBJECT_LOCK_T
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT go_package EXPORTING devclass = devclass.
    go_package->get_objs_lock(
      EXPORTING
        incl_sub = 'X'
      RECEIVING
        obj_lock = locks[]
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

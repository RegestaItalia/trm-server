FUNCTION ztrm_get_devclass_objs.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DEVCLASS) TYPE  DEVCLASS
*"  EXPORTING
*"     VALUE(ET_TADIR) TYPE  SCTS_TADIR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT lo_package EXPORTING iv_devclass = iv_devclass.
    lo_package->get_objects(
      IMPORTING
        et_tadir    = et_tadir
    ).
  CATCH zcx_trm_exception INTO lo_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

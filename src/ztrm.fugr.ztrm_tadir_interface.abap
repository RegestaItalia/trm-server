FUNCTION ztrm_tadir_interface.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PGMID) TYPE  PGMID
*"     VALUE(IV_OBJECT) TYPE  TROBJTYPE
*"     VALUE(IV_OBJ_NAME) TYPE  SOBJ_NAME
*"     VALUE(IV_DEVCLASS) TYPE  DEVCLASS OPTIONAL
*"     VALUE(IV_SRCSYSTEM) TYPE  SRCSYSTEM OPTIONAL
*"     VALUE(IV_AUTHOR) TYPE  RESPONSIBL OPTIONAL
*"     VALUE(IV_SET_GENFLAG) TYPE  GENFLAG OPTIONAL
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    zcl_trm_utility=>tadir_interface(
      EXPORTING
        iv_pgmid     = iv_pgmid
        iv_object    = iv_object
        iv_objname   = iv_obj_name
        iv_devclass  = iv_devclass
        iv_srcsystem = iv_srcsystem
        iv_author    = iv_author
        iv_genflag   = iv_set_genflag
    ).
  CATCH zcx_trm_exception INTO lo_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

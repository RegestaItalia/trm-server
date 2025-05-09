FUNCTION ztrm_get_abapgit_source.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DEVCLASS) TYPE  DEVCLASS
*"     VALUE(IV_IGNORE_DOT_ABAPGIT_OBJS) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     VALUE(EV_ZIP) TYPE  XSTRING
*"  TABLES
*"      ET_OBJECTS STRUCTURE  TADIR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      ABAPGIT_DATA_ERROR
*"      ABAPGIT_INTEGRATION
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      zcl_trm_abapgit=>serialize(
        EXPORTING
          iv_devclass                = iv_devclass
          iv_ignore_dot_abapgit_objs = iv_ignore_dot_abapgit_objs
        IMPORTING
          ev_zip                     = ev_zip
          et_objects                 = et_objects[]
      ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.
ENDFUNCTION.

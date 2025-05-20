FUNCTION ztrm_get_abapgit_source.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DEVCLASS) TYPE  DEVCLASS
*"  EXPORTING
*"     VALUE(EV_ZIP) TYPE  XSTRING
*"     VALUE(EV_OBJECTS) TYPE  XSTRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      ABAPGIT_DATA_ERROR
*"      ABAPGIT_INTEGRATION
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      DATA lt_objects TYPE zcl_trm_abapgit=>tyt_ser_objs.
      zcl_trm_abapgit=>serialize(
        EXPORTING
          iv_devclass = iv_devclass
        IMPORTING
          ev_zip      = ev_zip
          et_objects  = lt_objects[]
      ).
      CALL TRANSFORMATION id
      SOURCE objects = lt_objects
      RESULT XML ev_objects.
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.
ENDFUNCTION.

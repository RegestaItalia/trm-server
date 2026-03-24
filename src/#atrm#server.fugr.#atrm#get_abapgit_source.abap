FUNCTION /ATRM/GET_ABAPGIT_SOURCE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DEVCLASS) TYPE  DEVCLASS
*"  EXPORTING
*"     VALUE(ZIP) TYPE  XSTRING
*"     VALUE(OBJECTS) TYPE  XSTRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      ABAPGIT_DATA_ERROR
*"      ABAPGIT_INTEGRATION
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      DATA ser_objects TYPE /ATRM/CL_ABAPGIT=>tyt_ser_objs.
      /ATRM/CL_ABAPGIT=>serialize(
        EXPORTING
          devclass = devclass
        IMPORTING
          zip      = zip
          objects  = ser_objects[]
      ).
      CALL TRANSFORMATION id
      SOURCE objects = ser_objects
      RESULT XML objects.
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

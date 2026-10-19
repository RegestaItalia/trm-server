FUNCTION /atrm/get_transport_layers.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TARGET) TYPE  TARSYSTEM
*"  EXPORTING
*"     VALUE(LAYERS) TYPE  /ATRM/DEVLAYER_T
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      layers = /atrm/cl_transport=>get_transport_layers( target ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

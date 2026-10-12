FUNCTION /atrm/get_transport_layer.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(LAYER) TYPE  DEVLAYER
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      CALL METHOD ('/ATRM/CL_UTILITIES')=>('GET_DEFAULT_TRANSPORT_LAYER')
        IMPORTING
          layer = layer
      .
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

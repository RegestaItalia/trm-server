FUNCTION /ATRM/GET_TRANSPORT_LAYER.
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
      /ATRM/CL_UTILITIES=>get_default_transport_layer(
        IMPORTING
          layer = layer
      ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

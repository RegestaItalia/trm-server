FUNCTION ztrm_get_transport_layer.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_LAYER) TYPE  DEVLAYER
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      WRONG_CALL
*"      INVALID_INPUT
*"      CTS_INITIALIZATION_FAILURE
*"----------------------------------------------------------------------
  PERFORM check_auth.

  CALL FUNCTION 'TR_GET_TRANSPORT_TARGET'
    EXPORTING
      iv_use_default             = abap_true
      iv_get_layer_only          = abap_true
    IMPORTING
      ev_layer                   = ev_layer
    EXCEPTIONS
      wrong_call                 = 1
      invalid_input              = 2
      cts_initialization_failure = 3.

  IF sy-subrc EQ 1.
    RAISE wrong_call.
  ELSEIF sy-subrc EQ 2.
    RAISE invalid_input.
  ELSEIF sy-subrc EQ 3.
    RAISE cts_initialization_failure.
  ENDIF.

ENDFUNCTION.

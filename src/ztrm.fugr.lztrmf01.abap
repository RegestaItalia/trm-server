*&---------------------------------------------------------------------*
*& Include          LZTRMF01
*&---------------------------------------------------------------------*

FORM check_auth.
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.
ENDFORM.

FORM handle_exception.
  CHECK lo_exc IS BOUND.
  CASE lo_exc->reason( ).
    WHEN zcx_trm_exception=>c_reason-invalid_input.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING invalid_input.
    WHEN OTHERS.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING generic.
  ENDCASE.
ENDFORM.

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

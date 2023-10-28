FUNCTION ZTRM_SET_TRANSPORT_DOC.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"  TABLES
*"      IT_DOC STRUCTURE  TLINE
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      ERROR
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

  CALL FUNCTION 'TRINT_DOCU_INTERFACE'
    EXPORTING
      iv_object           = iv_trkorr
      iv_action           = 'M'
      iv_modify_appending = ''
    TABLES
      tt_line             = it_doc.

  IF sy-subrc <> 0.
    RAISE error.
  ENDIF.


ENDFUNCTION.

FUNCTION ztrm_set_transport_doc.
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
  PERFORM check_auth.

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

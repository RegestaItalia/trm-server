FUNCTION ZTRM_RENAME_TRANSPORT_REQUEST.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"     VALUE(IV_AS4TEXT) TYPE  AS4TEXT
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

  UPDATE e07t SET as4text = iv_as4text WHERE trkorr EQ iv_trkorr.
  COMMIT WORK AND WAIT.




ENDFUNCTION.

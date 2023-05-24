FUNCTION ztrm_rename_transport_request.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"     VALUE(IV_AS4TEXT) TYPE  AS4TEXT
*"----------------------------------------------------------------------
  UPDATE e07t SET as4text = iv_as4text WHERE trkorr EQ iv_trkorr.
  COMMIT WORK AND WAIT.




ENDFUNCTION.

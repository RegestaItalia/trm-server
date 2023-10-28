FUNCTION ZTRM_LIST_OBJECT_TYPES.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      ET_OBJECT_TEXT STRUCTURE  KO100
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

  CLEAR et_object_text.
  CALL FUNCTION 'TR_OBJECT_TABLE'
    TABLES
      wt_object_text = et_object_text.



ENDFUNCTION.

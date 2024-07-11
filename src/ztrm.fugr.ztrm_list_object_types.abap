FUNCTION ztrm_list_object_types.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      ET_OBJECT_TEXT STRUCTURE  KO100
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  PERFORM check_auth.

  CLEAR et_object_text.
  CALL FUNCTION 'TR_OBJECT_TABLE'
    TABLES
      wt_object_text = et_object_text.



ENDFUNCTION.

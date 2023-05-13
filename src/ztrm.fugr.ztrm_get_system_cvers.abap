FUNCTION ztrm_get_system_cvers.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      ET_CVERS STRUCTURE  CVERS
*"----------------------------------------------------------------------
  CLEAR et_cvers.
  SELECT * FROM cvers INTO CORRESPONDING FIELDS OF TABLE et_cvers.



ENDFUNCTION.

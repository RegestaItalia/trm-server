FUNCTION ZTRM_GET_TR_TARGETS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      ET_TMSCSYS STRUCTURE  TMSCSYS
*"----------------------------------------------------------------------
  SELECT * FROM tmscsys INTO CORRESPONDING FIELDS OF TABLE et_tmscsys.



ENDFUNCTION.

FUNCTION ztrm_get_published_packages.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      ET_PACKAGES STRUCTURE  ZTRM_PUBLISH
*"----------------------------------------------------------------------
  SELECT * FROM ztrm_publish INTO CORRESPONDING FIELDS OF TABLE et_packages.



ENDFUNCTION.

FUNCTION ztrm_get_artifact_devc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PACKAGE_NAME) TYPE  ZTRM_PACKAGE_NAME
*"     VALUE(IV_REGISTRY) TYPE  ZTRM_PACKAGE_REGISTRY
*"  TABLES
*"      ET_DEVC_MAP STRUCTURE  ZTRM_DEVC
*"----------------------------------------------------------------------
  SELECT * FROM ztrm_devc
  INTO CORRESPONDING FIELDS OF TABLE et_devc_map
  WHERE package_name EQ iv_package_name AND registry EQ iv_registry.



ENDFUNCTION.

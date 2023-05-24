FUNCTION ztrm_get_artifact_trkorr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_NAME) TYPE  ZTRM_PACKAGE_NAME
*"     VALUE(IV_REGISTRY) TYPE  ZTRM_PACKAGE_REGISTRY
*"  EXPORTING
*"     VALUE(EV_TRKORR) TYPE  TRKORR
*"     VALUE(EV_STATUS) TYPE  TRSTATUS
*"----------------------------------------------------------------------
  DATA ls_trkorr_stat TYPE ztrm_trkorr_stat.
  SELECT SINGLE trkorr trstatus FROM ztrm_trkorr_stat INTO CORRESPONDING FIELDS OF ls_trkorr_stat WHERE package_name EQ iv_name AND registry EQ iv_registry.

  ev_trkorr = ls_trkorr_stat-trkorr.
  ev_status = ls_trkorr_stat-trstatus.

ENDFUNCTION.

FUNCTION ZTRM_VERSION.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_VERSION) TYPE  STRING
*"----------------------------------------------------------------------
  "Avoid raising TRM_RFC_UNAUTHORIZED
  "otherwise clients may not be able to find out if this system has
  "trm-server installed or not

  ev_version = zif_trm=>version.


ENDFUNCTION.

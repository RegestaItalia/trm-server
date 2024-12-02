FUNCTION ztrm_version.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_VERSION) TYPE  STRING
*"     VALUE(EV_REST) TYPE  STRING
*"----------------------------------------------------------------------
  "Avoid raising TRM_RFC_UNAUTHORIZED
  "otherwise clients may not be able to find out if this system has
  "trm-server installed or not

  ev_version = zif_trm=>version.

  "read other versions
  FIELD-SYMBOLS <fs_rest> TYPE string.
  ASSIGN ('ZIF_TRM_REST')=>('VERSION') TO <fs_rest>.
  IF <fs_rest> IS ASSIGNED.
    ev_rest = <fs_rest>.
  ENDIF.

ENDFUNCTION.

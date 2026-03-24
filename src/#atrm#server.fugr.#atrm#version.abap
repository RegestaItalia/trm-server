FUNCTION /ATRM/VERSION.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(VERSION) TYPE  STRING
*"     VALUE(REST) TYPE  STRING
*"----------------------------------------------------------------------
  "Avoid raising TRM_RFC_UNAUTHORIZED
  "otherwise clients may not be able to find out if this system has
  "trm-server installed or not

  version = /ATRM/IF_SERVER=>version.

  "read other versions
  FIELD-SYMBOLS <fs_rest> TYPE string.
  ASSIGN ('/ATRM/IF_REST')=>('VERSION') TO <fs_rest>.
  IF <fs_rest> IS ASSIGNED.
    rest = <fs_rest>.
  ENDIF.

ENDFUNCTION.

FUNCTION ZTRM_GET_DEVCLASS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DEVCLASS) TYPE  DEVCLASS
*"  EXPORTING
*"     VALUE(ES_TDEVC) TYPE  TDEVC
*"  EXCEPTIONS
*"      DEVCLASS_NOT_FOUND
*"----------------------------------------------------------------------
  CALL FUNCTION 'TR_DEVCLASS_GET'
    EXPORTING
      iv_devclass        = iv_devclass
    IMPORTING
      es_tdevc           = es_tdevc
    EXCEPTIONS
      devclass_not_found = 1.

  IF sy-subrc EQ 1.
    RAISE devclass_not_found.
  ENDIF.


ENDFUNCTION.

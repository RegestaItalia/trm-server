FUNCTION ZTRM_get_devclass_objs.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DEVCLASS) TYPE  DEVCLASS
*"  EXPORTING
*"     VALUE(ET_TADIR) TYPE  SCTS_TADIR
*"  EXCEPTIONS
*"      CANCELLED_BY_USER
*"      INVALID_INPUT
*"----------------------------------------------------------------------
  CALL FUNCTION 'TRINT_SELECT_OBJECTS'
    EXPORTING
      iv_devclass       = iv_devclass
      iv_via_selscreen  = ' '
    IMPORTING
      et_objects_tadir  = et_tadir
    EXCEPTIONS
      cancelled_by_user = 1
      invalid_input     = 2.

  IF sy-subrc EQ 1.
    RAISE cancelled_by_user.
  ELSEIF sy-subrc EQ 2.
    RAISE invalid_input.
  ENDIF.

ENDFUNCTION.

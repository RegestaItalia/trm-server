FUNCTION ZTRM_GET_DEVCLASS_OBJS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DEVCLASS) TYPE  DEVCLASS
*"  EXPORTING
*"     VALUE(ET_TADIR) TYPE  SCTS_TADIR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      CANCELLED_BY_USER
*"      INVALID_INPUT
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

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

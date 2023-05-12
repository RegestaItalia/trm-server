FUNCTION ZTRM_GET_DIR_TRANS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_DIR_TRANS) TYPE  PFEVALUE
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------
  DATA lv_param_name TYPE pfeparname.
  lv_param_name = 'DIR_TRANS'.

  CALL FUNCTION 'SXPG_PROFILE_PARAMETER_GET'
    EXPORTING
      parameter_name  = lv_param_name
    IMPORTING
      parameter_value = ev_dir_trans.

  IF lv_param_name IS INITIAL.
    RAISE not_found.
  ENDIF.

ENDFUNCTION.

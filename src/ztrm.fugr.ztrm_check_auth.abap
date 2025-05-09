FUNCTION ztrm_check_auth.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  DATA lv_error TYPE string.
  IF zcl_trm_utility=>check_functions_authorization( ) NE 'X'.
    CONCATENATE 'User' sy-uname 'is not authorized to execute TRM RFC functions' INTO lv_error SEPARATED BY space.
    cl_message_helper=>set_msg_vars_for_clike( lv_error ).
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING trm_rfc_unauthorized.
  ENDIF.

ENDFUNCTION.

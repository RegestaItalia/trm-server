*&---------------------------------------------------------------------*
*& Include          LZTRMF01
*&---------------------------------------------------------------------*

FORM check_auth.
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING trm_rfc_unauthorized.
  ENDIF.
ENDFORM.

FORM handle_exception.
  CHECK lo_exc IS BOUND.
  CASE lo_exc->reason( ).
    WHEN zcx_trm_exception=>c_reason-invalid_input.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING invalid_input.
    WHEN zcx_trm_exception=>c_reason-enqueue_error.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING enqueue_error.
    WHEN zcx_trm_exception=>c_reason-dequeue_error.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING dequeue_error.
    WHEN zcx_trm_exception=>c_reason-dyn_call_param_not_found.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING dyn_call_param_not_found.
    WHEN zcx_trm_exception=>c_reason-not_found.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING not_found.
    WHEN zcx_trm_exception=>c_reason-tms_alert.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING tms_alert.
    WHEN zcx_trm_exception=>c_reason-r3trans_cmd_error.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING r3trans_cmd_error.
    WHEN zcx_trm_exception=>c_reason-snro_interval_not_found.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING snro_interval_not_found.
    WHEN zcx_trm_exception=>c_reason-abapgit_data_error.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING abapgit_data_error.
    WHEN zcx_trm_exception=>c_reason-abapgit_intergration.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING abapgit_intergration.
    WHEN zcx_trm_exception=>c_reason-pa_dynamic.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING pa_dynamic.
    WHEN zcx_trm_exception=>c_reason-program_not_found.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING program_not_found.
    WHEN OTHERS.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING generic.
  ENDCASE.
ENDFORM.

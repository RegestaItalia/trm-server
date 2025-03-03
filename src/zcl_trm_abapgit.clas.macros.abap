*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE create_data.
  TRY.
    CREATE DATA &1 TYPE (&2).
  CATCH cx_root INTO tmp_exception.
    zcx_trm_exception=>raise(
      EXPORTING
        iv_reason  = zcx_trm_exception=>c_reason-abapgit_data_error
        iv_message = tmp_exception->get_text( )
    ).
  ENDTRY.
END-OF-DEFINITION.

DEFINE create_data_ref.
  TRY.
    CREATE DATA &1 TYPE REF TO (&2).
  CATCH cx_root INTO tmp_exception.
    zcx_trm_exception=>raise(
      EXPORTING
        iv_reason  = zcx_trm_exception=>c_reason-abapgit_data_error
        iv_message = tmp_exception->get_text( )
    ).
  ENDTRY.
END-OF-DEFINITION.

DEFINE create_object.
  TRY.
    CREATE OBJECT &1 TYPE (&2)
      PARAMETER-TABLE tmp_param_tab.
    CLEAR tmp_param_tab.
  CATCH cx_root INTO tmp_exception.
    zcx_trm_exception=>raise(
      EXPORTING
        iv_reason  = zcx_trm_exception=>c_reason-abapgit_intergration
        iv_message = tmp_exception->get_text( )
    ).
  ENDTRY.
END-OF-DEFINITION.

DEFINE add_param.
  CLEAR tmp_param.
  tmp_param-name = &1.
  tmp_param-value = &2.
  tmp_param-kind = &3.
  INSERT tmp_param INTO TABLE tmp_param_tab.
END-OF-DEFINITION.

DEFINE call_object_method.
  TRY.
    CALL METHOD &1->(&2)
      PARAMETER-TABLE tmp_param_tab.
    clear tmp_param_tab.
  CATCH cx_root INTO tmp_exception.
    zcx_trm_exception=>raise(
      EXPORTING
        iv_reason  = zcx_trm_exception=>c_reason-abapgit_intergration
        iv_message = tmp_exception->get_text( )
    ).
  ENDTRY.
END-OF-DEFINITION.

DEFINE call_static_method.
  TRY.
    CALL METHOD (&1)=>(&2)
      PARAMETER-TABLE tmp_param_tab.
    clear tmp_param_tab.
  CATCH cx_root INTO tmp_exception.
    zcx_trm_exception=>raise(
      EXPORTING
        iv_reason  = zcx_trm_exception=>c_reason-abapgit_intergration
        iv_message = tmp_exception->get_text( )
    ).
  ENDTRY.
END-OF-DEFINITION.

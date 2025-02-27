*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE create_data.
  DATA lx_error TYPE REF TO cx_sy_create_data_error.
  TRY.
    CREATE DATA &1 TYPE (&2).
  CATCH cx_sy_create_data_error INTO lx_error.
    zcx_trm_exception=>raise(
    EXPORTING
      iv_reason  = zcx_trm_exception=>c_reason-abapgit_data_error
      iv_message = lx_error->get_text( )
    ).
  ENDTRY.
END-OF-DEFINITION.

DEFINE create_data_ref.
  DATA lx_error TYPE REF TO cx_sy_create_data_error.
  TRY.
    CREATE DATA &1 TYPE REF TO (&2).
  CATCH cx_sy_create_data_error INTO lx_error.
    zcx_trm_exception=>raise(
    EXPORTING
      iv_reason  = zcx_trm_exception=>c_reason-abapgit_data_error
      iv_message = lx_error->get_text( )
    ).
  ENDTRY.
END-OF-DEFINITION.

DEFINE create_object.
  CREATE OBJECT &1 TYPE (&2)
    PARAMETER-TABLE lt_param.
END-OF-DEFINITION.

DEFINE add_param.
  CLEAR ls_param.
  ls_param-name = &1.
  ls_param-value = &2.
  ls_param-kind = &3.
  INSERT ls_param INTO TABLE lt_param.
END-OF-DEFINITION.

DEFINE call_object_method.
  CALL METHOD &1->(&2)
    PARAMETER-TABLE lt_param.
END-OF-DEFINITION.

DEFINE call_static_method.
  CALL METHOD (&1)=>(&2)
    PARAMETER-TABLE lt_param.
END-OF-DEFINITION.

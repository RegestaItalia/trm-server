FUNCTION ztrm_execute_post_activity.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_CLASSNAME) TYPE  CLASSNAME
*"  TABLES
*"      IT_PARAMS STRUCTURE  ZTRM_PA_PARAM
*"----------------------------------------------------------------------
  DATA: lv_classname         LIKE iv_classname,
        lo_typedescr         TYPE REF TO cl_abap_typedescr,
        lo_classdescr        TYPE REF TO cl_abap_classdescr,
        lo_pa_attribute      TYPE REF TO cl_abap_datadescr,
        lv_pa_attribute_name TYPE string,
        ls_method            TYPE abap_methdescr,
        ls_method_param      TYPE abap_parmdescr,
        ls_param             LIKE LINE OF it_params,
        lt_parambind_tab     TYPE abap_parmbind_tab,
        ls_parambind         LIKE LINE OF lt_parambind_tab,
        lo_paramtype         TYPE REF TO cl_abap_datadescr.
  FIELD-SYMBOLS: <fs_pa_attribute_value> TYPE flag.

  lv_classname = iv_classname.
  TRANSLATE lv_classname TO UPPER CASE.
  CONDENSE lv_classname.
  cl_abap_classdescr=>describe_by_name(
    EXPORTING
      p_name         = lv_classname
    RECEIVING
      p_descr_ref    = lo_typedescr
    EXCEPTIONS
      type_not_found = 1
      OTHERS         = 2
  ).
  IF sy-subrc <> 0 OR lo_classdescr IS NOT BOUND.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  lo_classdescr ?= lo_typedescr.

  " read trm_pa attribute
  lo_classdescr->get_attribute_type(
    EXPORTING
      p_name              = 'TRM_PA'
    RECEIVING
      p_descr_ref         = lo_pa_attribute
    EXCEPTIONS
      attribute_not_found = 1
      OTHERS              = 2
  ).
  IF sy-subrc <> 0 OR lo_pa_attribute IS NOT BOUND.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  lv_pa_attribute_name = lo_pa_attribute->get_relative_name( ).
  IF lv_pa_attribute_name <> 'FLAG'.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  ASSIGN (lv_classname)=>('TRM_PA') TO <fs_pa_attribute_value>.
  IF sy-subrc <> 0 OR <fs_pa_attribute_value> <> 'X'.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  " execute
  READ TABLE lo_classdescr->methods INTO ls_method WITH KEY name = 'EXECUTE' visibility = 'U' is_class = 'X'.
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  LOOP AT ls_method-parameters INTO ls_method_param.
    READ TABLE it_params INTO ls_param WITH KEY parameter = ls_method_param-name.
    IF sy-subrc <> 0 AND ls_method_param-is_optional <> 'X'.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    IF ls_method_param-parm_kind <> 'I'.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    CLEAR ls_parambind.
    CLEAR lo_paramtype.
    ls_parambind-name = ls_method_param-name.
    ls_parambind-kind = 'E'.
    lo_classdescr->get_method_parameter_type(
      EXPORTING
        p_method_name       = ls_method-name
        p_parameter_name    = ls_method_param-name
      RECEIVING
        p_descr_ref         = lo_paramtype
      EXCEPTIONS
        parameter_not_found = 1
        method_not_found    = 2
        OTHERS              = 3
    ).
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    CREATE DATA ls_parambind-value TYPE HANDLE lo_paramtype.
    ls_parambind-value->* = ls_param-value.
    INSERT ls_parambind INTO TABLE lt_parambind_tab.
  ENDLOOP.
  check 1 eq 1.
ENDFUNCTION.

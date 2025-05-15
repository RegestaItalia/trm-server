CLASS zcl_trm_post_activity DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_pa_parameter,
             name  TYPE string,
             value TYPE string,
           END OF ty_pa_parameter,
           tyt_pa_parameter TYPE STANDARD TABLE OF ty_pa_parameter WITH DEFAULT KEY,
           BEGIN OF ty_pa_data,
             name       TYPE classname,
             parameters TYPE tyt_pa_parameter,
           END OF ty_pa_data.

    "! Pre check for post-activity
    "! @parameter iv_data     | XML input defining the class and parameters that will be execute
    "! @parameter et_messages | Table of messages returned from the pre-check
    "! @parameter ev_execute  | Flag that indicates if the post activity should be executed (X = true)
    CLASS-METHODS pre
      IMPORTING iv_data     TYPE xstring
      EXPORTING et_messages TYPE symsg_tab
                ev_execute  TYPE flag
      RAISING   zcx_trm_exception.

    "! Executes a post-activity class dynamically from XML input
    "! @parameter iv_data     | XML input defining the class and parameters to execute
    "! @parameter et_messages | Table of messages returned from the execution
    CLASS-METHODS execute
      IMPORTING iv_data     TYPE xstring
      EXPORTING et_messages TYPE symsg_tab
      RAISING   zcx_trm_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: lc_pa_attribute   TYPE abap_attrname VALUE 'TRM_PA',
               lc_pa_pre         TYPE abap_methname VALUE 'PRE',
               lc_pa_execute     TYPE abap_methname VALUE 'EXECUTE',
               lc_pa_messages    TYPE abap_parmname VALUE 'MESSAGES',
               lc_pa_precheckval TYPE abap_parmname VALUE 'EXECUTE'.

    CLASS-METHODS run
      IMPORTING iv_methname TYPE abap_methname
                iv_data     TYPE xstring
      EXPORTING et_messages TYPE symsg_tab
                ev_execute  TYPE flag
      RAISING   zcx_trm_exception.

ENDCLASS.



CLASS zcl_trm_post_activity IMPLEMENTATION.

  METHOD run.
    DATA: ls_data          TYPE ty_pa_data,
          lv_error         TYPE string,
          lo_typedescr     TYPE REF TO cl_abap_typedescr,
          lo_classdescr    TYPE REF TO cl_abap_classdescr,
          lo_pa_attribute  TYPE REF TO cl_abap_datadescr,
          ls_method        TYPE abap_methdescr,
          ls_method_param  TYPE abap_parmdescr,
          ls_param         TYPE ty_pa_parameter,
          lt_parambind_tab TYPE abap_parmbind_tab,
          ls_parambind     LIKE LINE OF lt_parambind_tab,
          lo_paramtype     TYPE REF TO cl_abap_datadescr,
          lx_root          TYPE REF TO cx_root.
    FIELD-SYMBOLS: <fs_pa_parameter>       TYPE ty_pa_parameter,
                   <fs_pa_attribute_value> TYPE flag.

    CALL TRANSFORMATION id
    SOURCE XML iv_data
    RESULT data = ls_data.

    TRANSLATE ls_data-name TO UPPER CASE.
    CONDENSE ls_data-name.
    LOOP AT ls_data-parameters ASSIGNING <fs_pa_parameter>.
      TRANSLATE <fs_pa_parameter>-name TO UPPER CASE.
      CONDENSE <fs_pa_parameter>-name.
    ENDLOOP.
    IF ls_data-name IS INITIAL.
      zcx_trm_exception=>raise(
        iv_message = 'Post activity was not specified'
        iv_reason  = zcx_trm_exception=>c_reason-invalid_input
      ).
    ELSE.
      CONCATENATE 'Post Activity' ls_data-name 'not found' INTO lv_error SEPARATED BY space.
    ENDIF.
    cl_abap_classdescr=>describe_by_name(
      EXPORTING
        p_name         = ls_data-name
      RECEIVING
        p_descr_ref    = lo_typedescr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0 OR lo_typedescr IS NOT BOUND.
      zcx_trm_exception=>raise(
        iv_message = lv_error
        iv_reason  = zcx_trm_exception=>c_reason-pa_not_found
      ).
    ENDIF.

    lo_classdescr ?= lo_typedescr.

    " read trm_pa attribute
    lo_classdescr->get_attribute_type(
      EXPORTING
        p_name              = lc_pa_attribute
      RECEIVING
        p_descr_ref         = lo_pa_attribute
      EXCEPTIONS
        attribute_not_found = 1
        OTHERS              = 2
    ).
    IF sy-subrc <> 0 OR lo_pa_attribute IS NOT BOUND.
      zcx_trm_exception=>raise(
        iv_message = lv_error
        iv_reason  = zcx_trm_exception=>c_reason-pa_not_found
      ).
    ENDIF.

    IF lo_pa_attribute->get_relative_name( ) <> 'FLAG'.
      zcx_trm_exception=>raise(
        iv_message = lv_error
        iv_reason  = zcx_trm_exception=>c_reason-pa_not_found
      ).
    ENDIF.
    ASSIGN (ls_data-name)=>(lc_pa_attribute) TO <fs_pa_attribute_value>.
    IF sy-subrc <> 0 OR <fs_pa_attribute_value> <> 'X'.
      zcx_trm_exception=>raise(
        iv_message = lv_error
        iv_reason  = zcx_trm_exception=>c_reason-pa_not_found
      ).
    ENDIF.

    " execute
    READ TABLE lo_classdescr->methods INTO ls_method WITH KEY name = iv_methname visibility = 'U' is_class = 'X'.
    IF sy-subrc <> 0.
      IF iv_methname EQ lc_pa_pre.
        ev_execute = 'X'.
        RETURN.
      ELSE.
        zcx_trm_exception=>raise(
          iv_message = lv_error
          iv_reason  = zcx_trm_exception=>c_reason-pa_not_found
        ).
      ENDIF.
    ENDIF.

    LOOP AT ls_method-parameters INTO ls_method_param.
      READ TABLE ls_data-parameters INTO ls_param WITH KEY name = ls_method_param-name.
      IF sy-subrc <> 0.
        IF ls_method_param-is_optional <> 'X'.
          CONCATENATE 'Obligatory parameter' ls_method_param-name 'missing' INTO lv_error SEPARATED BY space.
          zcx_trm_exception=>raise(
            iv_message = lv_error
            iv_reason  = zcx_trm_exception=>c_reason-pa_param_missing
          ).
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.
      IF ls_method_param-parm_kind <> 'I'.
        CONCATENATE 'Unexpected parameter' ls_method_param-name INTO lv_error SEPARATED BY space.
        zcx_trm_exception=>raise(
          iv_message = lv_error
          iv_reason  = zcx_trm_exception=>c_reason-pa_unexpected_param
        ).
      ENDIF.
      CLEAR lo_paramtype.
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
        zcx_trm_exception=>raise(
          iv_reason  = zcx_trm_exception=>c_reason-pa_param_missing
        ).
      ENDIF.
      IF lo_paramtype->kind <> cl_abap_datadescr=>kind_elem.
        CONCATENATE ls_method_param-name ': Only elementary data is supported' INTO lv_error SEPARATED BY space.
        zcx_trm_exception=>raise(
          iv_message = lv_error
          iv_reason  = zcx_trm_exception=>c_reason-pa_unexpected_param
        ).
      ENDIF.
      CLEAR ls_parambind.
      ls_parambind-name = ls_method_param-name.
      ls_parambind-kind = 'E'.
      CREATE DATA ls_parambind-value TYPE HANDLE lo_paramtype.
      ls_parambind-value->* = ls_param-value.
      INSERT ls_parambind INTO TABLE lt_parambind_tab.
    ENDLOOP.

    CLEAR ls_method_param.
    READ TABLE ls_method-parameters INTO ls_method_param WITH KEY name = lc_pa_messages.
    IF sy-subrc EQ 0.
      CLEAR lo_paramtype.
      lo_classdescr->get_method_parameter_type(
        EXPORTING
          p_method_name       = ls_method-name
          p_parameter_name    = lc_pa_messages
        RECEIVING
          p_descr_ref         = lo_paramtype
        EXCEPTIONS
          parameter_not_found = 1
          method_not_found    = 2
          OTHERS              = 3
      ).
      IF sy-subrc EQ 0.
        IF lo_paramtype->get_relative_name( ) EQ 'SYMSG_TAB'.
          CLEAR ls_parambind.
          ls_parambind-name = lc_pa_messages.
          ls_parambind-kind = 'I'.
          CREATE DATA ls_parambind-value TYPE HANDLE lo_paramtype.
          INSERT ls_parambind INTO TABLE lt_parambind_tab.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR ls_method_param.
    READ TABLE ls_method-parameters INTO ls_method_param WITH KEY name = lc_pa_precheckval.
    IF sy-subrc EQ 0.
      CLEAR lo_paramtype.
      lo_classdescr->get_method_parameter_type(
        EXPORTING
          p_method_name       = ls_method-name
          p_parameter_name    = lc_pa_precheckval
        RECEIVING
          p_descr_ref         = lo_paramtype
        EXCEPTIONS
          parameter_not_found = 1
          method_not_found    = 2
          OTHERS              = 3
      ).
      IF sy-subrc EQ 0.
        IF lo_paramtype->get_relative_name( ) EQ 'FLAG'.
          CLEAR ls_parambind.
          ls_parambind-name = lc_pa_precheckval.
          ls_parambind-kind = 'I'.
          CREATE DATA ls_parambind-value TYPE HANDLE lo_paramtype.
          INSERT ls_parambind INTO TABLE lt_parambind_tab.
        ENDIF.
      ENDIF.
    ENDIF.

    TRY.
        CALL METHOD (ls_data-name)=>(iv_methname)
          PARAMETER-TABLE lt_parambind_tab.
      CATCH cx_root INTO lx_root.
        zcx_trm_exception=>raise(
          io_root    = lx_root
          iv_reason  = zcx_trm_exception=>c_reason-pa_exception
        ).
    ENDTRY.

    CLEAR ls_parambind.
    READ TABLE lt_parambind_tab INTO ls_parambind WITH KEY name = lc_pa_messages.
    IF sy-subrc EQ 0.
      MOVE ls_parambind-value->* TO et_messages.
    ENDIF.
    CLEAR ls_parambind.
    READ TABLE lt_parambind_tab INTO ls_parambind WITH KEY name = lc_pa_precheckval.
    IF sy-subrc EQ 0.
      MOVE ls_parambind-value->* TO ev_execute.
    ENDIF.
  ENDMETHOD.

  METHOD pre.
    run(
      EXPORTING
        iv_methname = lc_pa_pre
        iv_data     = iv_data
      IMPORTING
        et_messages = et_messages
        ev_execute  = ev_execute
    ).
  ENDMETHOD.

  METHOD execute.
    run(
      EXPORTING
        iv_methname = lc_pa_execute
        iv_data     = iv_data
      IMPORTING
        et_messages = et_messages
    ).
  ENDMETHOD.

ENDCLASS.

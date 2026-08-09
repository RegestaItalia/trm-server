CLASS /atrm/cl_object_sicf DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ty_icf_name TYPE c LENGTH 15,
           ty_icfguid  TYPE c LENGTH 25.
    TYPES:
      BEGIN OF ty_service,
        icf_name   TYPE ty_icf_name,
        icfparguid TYPE ty_icfguid,
        icfnodguid TYPE ty_icfguid,
      END OF ty_service,
      tyt_service TYPE STANDARD TABLE OF ty_service WITH DEFAULT KEY,
      tyt_string  TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    CLASS-METHODS get_service_by_guid
      IMPORTING
        iv_node_guid     TYPE ty_icfguid
      RETURNING
        VALUE(rs_service) TYPE ty_service.

    METHODS add_tadir_dependency
      IMPORTING
        iv_object       TYPE any
        iv_obj_name     TYPE any
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t.

    METHODS add_component_dependency
      IMPORTING
        is_structure    TYPE any
        iv_component    TYPE string
        iv_object       TYPE any
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t.

    METHODS add_url_dependencies
      IMPORTING
        iv_url          TYPE string
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t
        ct_visited      TYPE tyt_service.

    METHODS add_guid_dependency
      IMPORTING
        is_structure     TYPE any
        iv_component     TYPE string
      CHANGING
        ct_dependencies  TYPE /atrm/object_dependency_t
        ct_visited       TYPE tyt_service.

    METHODS add_url_component_dependencies
      IMPORTING
        is_structure    TYPE any
        iv_url_component TYPE string
        iv_kind_component TYPE string OPTIONAL
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t
        ct_visited      TYPE tyt_service.

    METHODS collect_service_dependencies
      IMPORTING
        is_service      TYPE ty_service
        iv_add_service  TYPE flag
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t
        ct_visited      TYPE tyt_service.

    METHODS single_dependencies
      IMPORTING
        ir_serv_info    TYPE REF TO data
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t
        ct_visited      TYPE tyt_service.
ENDCLASS.



CLASS /atrm/cl_object_sicf IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: ls_service TYPE ty_service,
          lt_visited TYPE tyt_service.

    TRY.
        CLEAR ls_service.
        ls_service-icf_name = me->key-obj_name(15).
        ls_service-icfparguid = me->key-obj_name+15(25).

        collect_service_dependencies(
          EXPORTING
            is_service      = ls_service
            iv_add_service  = ' '
          CHANGING
            ct_dependencies = dependencies
            ct_visited      = lt_visited
        ).
      CATCH cx_root.
        " ICF is optional. Missing ICF repository objects must not prevent
        " this class from being used on systems without the framework.
    ENDTRY.
  ENDMETHOD.

  METHOD get_service_by_guid.
    DATA lv_table_name TYPE tabname.

    CLEAR rs_service.
    CHECK iv_node_guid IS NOT INITIAL.

    lv_table_name = 'ICFSERVICE'.
    SELECT SINGLE icf_name icfparguid icfnodguid
      FROM (lv_table_name)
      INTO CORRESPONDING FIELDS OF rs_service
      WHERE icfnodguid EQ iv_node_guid.
  ENDMETHOD.

  METHOD add_tadir_dependency.
    DATA ls_dependency TYPE /atrm/object_dependency.

    CHECK iv_object IS NOT INITIAL.
    CHECK iv_obj_name IS NOT INITIAL.

    CLEAR ls_dependency.
    TRY.
        get_tadir_dependency(
          EXPORTING
            object     = iv_object
            obj_name   = iv_obj_name
          RECEIVING
            dependency = ls_dependency
        ).
        READ TABLE ct_dependencies
          WITH KEY tabname = 'TADIR'
                   tabkey  = ls_dependency-tabkey
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND ls_dependency TO ct_dependencies.
        ENDIF.
      CATCH /atrm/cx_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD add_component_dependency.
    FIELD-SYMBOLS <lv_value> TYPE any.

    UNASSIGN <lv_value>.
    ASSIGN COMPONENT iv_component OF STRUCTURE is_structure TO <lv_value>.
    CHECK sy-subrc EQ 0.
    CHECK <lv_value> IS NOT INITIAL.

    add_tadir_dependency(
      EXPORTING
        iv_object       = iv_object
        iv_obj_name     = <lv_value>
      CHANGING
        ct_dependencies = ct_dependencies
    ).
  ENDMETHOD.

  METHOD add_url_component_dependencies.
    DATA lv_url TYPE string.
    FIELD-SYMBOLS: <lv_url>  TYPE any,
                   <lv_kind> TYPE any.

    UNASSIGN <lv_url>.
    ASSIGN COMPONENT iv_url_component OF STRUCTURE is_structure TO <lv_url>.
    CHECK sy-subrc EQ 0.
    CHECK <lv_url> IS NOT INITIAL.

    IF iv_kind_component IS NOT INITIAL.
      UNASSIGN <lv_kind>.
      ASSIGN COMPONENT iv_kind_component OF STRUCTURE is_structure TO <lv_kind>.
      IF sy-subrc EQ 0.
        CHECK <lv_kind> EQ 'X'.
      ENDIF.
    ENDIF.

    lv_url = <lv_url>.
    add_url_dependencies(
      EXPORTING
        iv_url          = lv_url
      CHANGING
        ct_dependencies = ct_dependencies
        ct_visited      = ct_visited
    ).
  ENDMETHOD.

  METHOD add_guid_dependency.
    DATA: lv_node_guid TYPE ty_icfguid,
          ls_service   TYPE ty_service.
    FIELD-SYMBOLS <lv_node_guid> TYPE any.

    UNASSIGN <lv_node_guid>.
    ASSIGN COMPONENT iv_component OF STRUCTURE is_structure TO <lv_node_guid>.
    CHECK sy-subrc EQ 0.
    CHECK <lv_node_guid> IS NOT INITIAL.

    lv_node_guid = <lv_node_guid>.
    CALL METHOD get_service_by_guid
      EXPORTING
        iv_node_guid = lv_node_guid
      RECEIVING
        rs_service   = ls_service.
    CHECK ls_service-icf_name IS NOT INITIAL.

    collect_service_dependencies(
      EXPORTING
        is_service      = ls_service
        iv_add_service  = 'X'
      CHANGING
        ct_dependencies = ct_dependencies
        ct_visited      = ct_visited
    ).
  ENDMETHOD.

  METHOD add_url_dependencies.
    DATA: lv_class_name TYPE string,
          lv_method_name TYPE string,
          lv_node_guid  TYPE ty_icfguid,
          lv_icf_name   TYPE ty_icf_name,
          ls_service    TYPE ty_service.

    CHECK iv_url IS NOT INITIAL.

    lv_class_name = 'CL_ICF_TREE'.
    lv_method_name = 'IF_ICF_TREE~SERVICE_FROM_URL'.
    CLEAR: lv_node_guid, lv_icf_name.
    CALL METHOD (lv_class_name)=>(lv_method_name)
      EXPORTING
        url                   = iv_url
        hostnumber            = 0
        authority_check       = ' '
      IMPORTING
        icfnodguid            = lv_node_guid
        icf_name              = lv_icf_name
      EXCEPTIONS
        wrong_application     = 1
        no_application        = 2
        not_allow_application = 3
        wrong_url             = 4
        no_authority          = 5
        OTHERS                = 6.
    CHECK sy-subrc EQ 0.

    CALL METHOD get_service_by_guid
      EXPORTING
        iv_node_guid = lv_node_guid
      RECEIVING
        rs_service   = ls_service.
    CHECK ls_service-icf_name IS NOT INITIAL.
    CHECK ls_service-icf_name EQ lv_icf_name.

    collect_service_dependencies(
      EXPORTING
        is_service      = ls_service
        iv_add_service  = 'X'
      CHANGING
        ct_dependencies = ct_dependencies
        ct_visited      = ct_visited
    ).
  ENDMETHOD.

  METHOD collect_service_dependencies.
    DATA: lv_obj_name      TYPE c LENGTH 40,
          lv_class_name    TYPE string,
          lv_method_name   TYPE string,
          lv_serv_info_type TYPE string,
          lr_serv_info     TYPE REF TO data,
          ls_parent        TYPE ty_service.
    FIELD-SYMBOLS <lt_serv_info> TYPE ANY TABLE.

    CHECK is_service-icf_name IS NOT INITIAL.

    READ TABLE ct_visited
      WITH KEY icf_name   = is_service-icf_name
               icfparguid = is_service-icfparguid
      TRANSPORTING NO FIELDS.
    CHECK sy-subrc <> 0.
    APPEND is_service TO ct_visited.

    IF iv_add_service EQ 'X'.
      CLEAR lv_obj_name.
      lv_obj_name+0(15)  = is_service-icf_name.
      lv_obj_name+15(25) = is_service-icfparguid.
      add_tadir_dependency(
        EXPORTING
          iv_object       = 'SICF'
          iv_obj_name     = lv_obj_name
        CHANGING
          ct_dependencies = ct_dependencies
      ).
    ENDIF.

    lv_serv_info_type = 'ICFSERVTBL'.
    CREATE DATA lr_serv_info TYPE (lv_serv_info_type).
    ASSIGN lr_serv_info->* TO <lt_serv_info>.
    IF sy-subrc EQ 0.
      lv_class_name = 'CL_ICF_TREE'.
      lv_method_name = 'IF_ICF_TREE~GET_INFO_FROM_SERV'.
      CALL METHOD (lv_class_name)=>(lv_method_name)
        EXPORTING
          icf_name          = is_service-icf_name
          icfparguid        = is_service-icfparguid
        IMPORTING
          serv_info         = <lt_serv_info>
        EXCEPTIONS
          wrong_name        = 1
          wrong_parguid     = 2
          incorrect_service = 3
          no_authority      = 4
          OTHERS            = 5.
      IF sy-subrc EQ 0.
        single_dependencies(
          EXPORTING
            ir_serv_info    = lr_serv_info
          CHANGING
            ct_dependencies = ct_dependencies
            ct_visited      = ct_visited
        ).
      ENDIF.
    ENDIF.

    CALL METHOD get_service_by_guid
      EXPORTING
        iv_node_guid = is_service-icfparguid
      RECEIVING
        rs_service   = ls_parent.
    IF ls_parent-icf_name IS NOT INITIAL.
      collect_service_dependencies(
        EXPORTING
          is_service      = ls_parent
          iv_add_service  = 'X'
        CHANGING
          ct_dependencies = ct_dependencies
          ct_visited      = ct_visited
      ).
    ENDIF.
  ENDMETHOD.

  METHOD single_dependencies.
    DATA: lt_otr_components TYPE tyt_string,
          lv_component      TYPE string.
    FIELD-SYMBOLS: <lt_serv_info> TYPE ANY TABLE,
                   <ls_info>      TYPE any,
                   <ls_service>   TYPE any,
                   <lt_handlers>  TYPE ANY TABLE,
                   <ls_handler>   TYPE any,
                   <lv_handler>   TYPE any.

    CHECK ir_serv_info IS BOUND.
    ASSIGN ir_serv_info->* TO <lt_serv_info>.
    CHECK sy-subrc EQ 0.

    APPEND 'HDROTRCONC' TO lt_otr_components.
    APPEND 'BODOTRCONC' TO lt_otr_components.
    APPEND 'HDROTR401' TO lt_otr_components.
    APPEND 'BODOTR401' TO lt_otr_components.
    APPEND 'HDROTRLPAG' TO lt_otr_components.
    APPEND 'BODOTRLPAG' TO lt_otr_components.
    APPEND 'HDROTRNFPAG' TO lt_otr_components.
    APPEND 'BODOTRNFPAG' TO lt_otr_components.

    LOOP AT <lt_serv_info> ASSIGNING <ls_info>.
      UNASSIGN <ls_service>.
      ASSIGN COMPONENT 'SERVICE' OF STRUCTURE <ls_info> TO <ls_service>.
      IF sy-subrc EQ 0.
        add_component_dependency(
          EXPORTING
            is_structure    = <ls_service>
            iv_component    = 'ICF_TCODE'
            iv_object       = 'TRAN'
          CHANGING
            ct_dependencies = ct_dependencies
        ).

        LOOP AT lt_otr_components INTO lv_component.
          add_component_dependency(
            EXPORTING
              is_structure    = <ls_service>
              iv_component    = lv_component
              iv_object       = 'SOTR'
            CHANGING
              ct_dependencies = ct_dependencies
          ).
        ENDLOOP.

        add_guid_dependency(
          EXPORTING
            is_structure     = <ls_service>
            iv_component     = 'ICFALIGUID'
          CHANGING
            ct_dependencies  = ct_dependencies
            ct_visited       = ct_visited
        ).

        add_url_component_dependencies(
          EXPORTING
            is_structure     = <ls_service>
            iv_url_component = 'ICF_REDIR'
          CHANGING
            ct_dependencies  = ct_dependencies
            ct_visited       = ct_visited
        ).
        add_url_component_dependencies(
          EXPORTING
            is_structure      = <ls_service>
            iv_url_component  = 'URL500'
            iv_kind_component = 'KIND500'
          CHANGING
            ct_dependencies   = ct_dependencies
            ct_visited        = ct_visited
        ).
        add_url_component_dependencies(
          EXPORTING
            is_structure      = <ls_service>
            iv_url_component  = 'URL401'
            iv_kind_component = 'KIND401'
          CHANGING
            ct_dependencies   = ct_dependencies
            ct_visited        = ct_visited
        ).
        add_url_component_dependencies(
          EXPORTING
            is_structure      = <ls_service>
            iv_url_component  = 'URLLPAG'
            iv_kind_component = 'KINDLPAG'
          CHANGING
            ct_dependencies   = ct_dependencies
            ct_visited        = ct_visited
        ).
        add_url_component_dependencies(
          EXPORTING
            is_structure      = <ls_service>
            iv_url_component  = 'URLNFPAG'
            iv_kind_component = 'KINDNFPAG'
          CHANGING
            ct_dependencies   = ct_dependencies
            ct_visited        = ct_visited
        ).
      ENDIF.

      UNASSIGN <lt_handlers>.
      ASSIGN COMPONENT 'HANDLERTBL' OF STRUCTURE <ls_info> TO <lt_handlers>.
      IF sy-subrc EQ 0.
        LOOP AT <lt_handlers> ASSIGNING <ls_handler>.
          UNASSIGN <lv_handler>.
          ASSIGN COMPONENT 'ICFHANDLER' OF STRUCTURE <ls_handler> TO <lv_handler>.
          IF sy-subrc EQ 0 AND <lv_handler> IS NOT INITIAL.
            add_tadir_dependency(
              EXPORTING
                iv_object       = 'CLAS'
                iv_obj_name     = <lv_handler>
              CHANGING
                ct_dependencies = ct_dependencies
            ).
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.


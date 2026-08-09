CLASS /atrm/cl_object_sqsc DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_sqsc IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lo_proxy       TYPE REF TO object,
      lr_header      TYPE REF TO data,
      lr_parameters  TYPE REF TO data,
      lr_types       TYPE REF TO data,
      lr_version     TYPE REF TO data,
      lt_bindings    TYPE abap_parmbind_tab,
      ls_binding     TYPE abap_parmbind,
      lv_version     TYPE c,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lv_proxy_name> TYPE any,
      <ls_header>     TYPE any,
      <lt_parameters> TYPE STANDARD TABLE,
      <lt_types>      TYPE STANDARD TABLE,
      <ls_row>        TYPE any,
      <lv_value>      TYPE any.

    TRY.
        CREATE OBJECT lo_proxy TYPE ('CL_DDIC_WB_DBPROC_PROXY').
        ASSIGN ('LO_PROXY->IF_DDIC_WB_DBPROC_PROXY~DBPROXYNAME')
          TO <lv_proxy_name>.
        CHECK sy-subrc = 0.
        <lv_proxy_name> = me->key-obj_name.

        CREATE DATA lr_header TYPE ('IF_DBPROC_PROXY_UI=>TY_HEADER_UI_S').
        CREATE DATA lr_parameters TYPE ('IF_DBPROC_PROXY_UI=>TY_PARAM_UI_T').
        CREATE DATA lr_types TYPE ('IF_DBPROC_PROXY_UI=>TY_PARAM_TYPE_UI_T').
        ASSIGN lr_header->* TO <ls_header>.
        ASSIGN lr_parameters->* TO <lt_parameters>.
        ASSIGN lr_types->* TO <lt_types>.

        lv_version = 'A'.
        GET REFERENCE OF lv_version INTO lr_version.
        CLEAR ls_binding.
        ls_binding-name = 'IF_VERSION'.
        ls_binding-kind = cl_abap_objectdescr=>exporting.
        ls_binding-value = lr_version.
        INSERT ls_binding INTO TABLE lt_bindings.
        CLEAR ls_binding.
        ls_binding-name = 'ES_HEADER'.
        ls_binding-kind = cl_abap_objectdescr=>importing.
        ls_binding-value = lr_header.
        INSERT ls_binding INTO TABLE lt_bindings.
        CLEAR ls_binding.
        ls_binding-name = 'ET_PARAMETER'.
        ls_binding-kind = cl_abap_objectdescr=>importing.
        ls_binding-value = lr_parameters.
        INSERT ls_binding INTO TABLE lt_bindings.
        CLEAR ls_binding.
        ls_binding-name = 'ET_PARAMETER_TYPE'.
        ls_binding-kind = cl_abap_objectdescr=>importing.
        ls_binding-value = lr_types.
        INSERT ls_binding INTO TABLE lt_bindings.

        CALL METHOD lo_proxy->('IF_DBPROC_PROXY_UI~READ_FROM_SOURCE')
          PARAMETER-TABLE lt_bindings.

        ASSIGN COMPONENT 'INTERFACE_POOL' OF STRUCTURE <ls_header>
          TO <lv_value>.
        IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
          CLEAR ls_dependency.
          CALL METHOD get_tadir_dependency
            EXPORTING object = 'INTF' obj_name = <lv_value>
            RECEIVING dependency = ls_dependency.
          IF ls_dependency IS NOT INITIAL.
            APPEND ls_dependency TO dependencies.
          ENDIF.
        ENDIF.

        LOOP AT <lt_parameters> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'DDIC_TABLE' OF STRUCTURE <ls_row> TO <lv_value>.
          CHECK sy-subrc = 0.
          CHECK <lv_value> IS NOT INITIAL.
          CLEAR ls_dependency.
          CALL METHOD get_entity_dependency
            EXPORTING entity = <lv_value>
            IMPORTING dependency = ls_dependency.
          IF ls_dependency IS NOT INITIAL.
            APPEND ls_dependency TO dependencies.
          ENDIF.
        ENDLOOP.

        LOOP AT <lt_types> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'DDIC_TYPE' OF STRUCTURE <ls_row> TO <lv_value>.
          CHECK sy-subrc = 0.
          CHECK <lv_value> IS NOT INITIAL.
          CLEAR ls_dependency.
          CALL METHOD get_entity_dependency
            EXPORTING entity = <lv_value>
            IMPORTING dependency = ls_dependency.
          IF ls_dependency IS NOT INITIAL.
            APPEND ls_dependency TO dependencies.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        " database procedure proxy APIs may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

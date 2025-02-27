*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_abapgit_dot_abapgit DEFINITION.

  PUBLIC SECTION.

    DATA: go_instance TYPE REF TO object.

    METHODS constructor
      IMPORTING io_abapgit_dot_abapgit TYPE REF TO object.

    METHODS serialize
      RETURNING VALUE(rv_xstr) TYPE xstring.

    METHODS set_folder_logic
      IMPORTING iv_logic TYPE string.

    CLASS-METHODS build_default
      RETURNING VALUE(ro_dot_abapgit) TYPE REF TO lcl_abapgit_dot_abapgit
      RAISING   zcx_trm_exception.

ENDCLASS.

CLASS lcl_abapgit_dot_abapgit IMPLEMENTATION.

  METHOD constructor.
    go_instance = io_abapgit_dot_abapgit.
  ENDMETHOD.

  METHOD serialize.
    DATA: lo_xstr  TYPE REF TO data,
          lt_param TYPE abap_parmbind_tab,
          ls_param LIKE LINE OF lt_param.
    GET REFERENCE OF rv_xstr INTO lo_xstr.
    add_param 'RV_XSTR' lo_xstr cl_abap_objectdescr=>receiving.
    call_object_method go_instance 'SERIALIZE'.
  ENDMETHOD.

  METHOD build_default.
    DATA: lo_dot_abapgit TYPE REF TO data,
          lt_param       TYPE abap_parmbind_tab,
          ls_param       LIKE LINE OF lt_param.
    create_data_ref lo_dot_abapgit 'ZCL_ABAPGIT_DOT_ABAPGIT'.
    add_param 'RO_DOT_ABAPGIT' lo_dot_abapgit cl_abap_objectdescr=>receiving.
    call_static_method 'ZCL_ABAPGIT_DOT_ABAPGIT' 'BUILD_DEFAULT'.
    CREATE OBJECT ro_dot_abapgit EXPORTING io_abapgit_dot_abapgit = lo_dot_abapgit->*.
  ENDMETHOD.

  METHOD set_folder_logic.
    DATA: lo_logic TYPE REF TO data,
          lt_param TYPE abap_parmbind_tab,
          ls_param LIKE LINE OF lt_param.
    GET REFERENCE OF iv_logic INTO lo_logic.
    add_param 'IV_LOGIC' lo_logic cl_abap_objectdescr=>exporting.
    call_object_method go_instance 'SET_FOLDER_LOGIC'.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abapgit_serialize DEFINITION.

  PUBLIC SECTION.

    DATA: go_instance TYPE REF TO object.

    METHODS constructor
      IMPORTING io_dot_abapgit    TYPE REF TO lcl_abapgit_dot_abapgit
                is_local_settings TYPE REF TO data.

ENDCLASS.

CLASS lcl_abapgit_serialize IMPLEMENTATION.

  METHOD constructor.
    DATA: lo_dot_abapgit    TYPE REF TO data,
          ls_local_settings TYPE REF TO data,
          lo_instance       TYPE REF TO data,
          lt_param          TYPE abap_parmbind_tab,
          ls_param          LIKE LINE OF lt_param.
*    GET REFERENCE OF io_dot_abapgit->go_instance INTO lo_dot_abapgit.
*    get REFERENCE OF lo_dot_abapgit->* into data(wddw).
*    add_param 'IO_DOT_ABAPGIT' wddw cl_abap_objectdescr=>exporting.
*    GET REFERENCE OF is_local_settings->* INTO ls_local_settings.
*    add_param 'IS_LOCAL_SETTINGS' ls_local_settings cl_abap_objectdescr=>exporting.
*    create_object go_instance 'ZCL_ABAPGIT_SERIALIZE'.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abapgit_repo DEFINITION.

  PUBLIC SECTION.

    DATA: go_instance TYPE REF TO object.

    METHODS constructor
      IMPORTING io_repo TYPE REF TO object.

    METHODS get_dot_abapgit
      RETURNING VALUE(ro_dot_abapgit) TYPE REF TO lcl_abapgit_dot_abapgit
      RAISING   zcx_trm_exception.

ENDCLASS.

CLASS lcl_abapgit_repo IMPLEMENTATION.

  METHOD constructor.
    go_instance = io_repo.
  ENDMETHOD.

  METHOD get_dot_abapgit.
    DATA: lo_dot_abapgit TYPE REF TO data,
          lt_param       TYPE abap_parmbind_tab,
          ls_param       LIKE LINE OF lt_param.
    create_data_ref lo_dot_abapgit 'ZCL_ABAPGIT_DOT_ABAPGIT'.
    add_param 'RO_DOT_ABAPGIT' lo_dot_abapgit cl_abap_objectdescr=>receiving.
    call_object_method go_instance 'GET_DOT_ABAPGIT'.
    CREATE OBJECT ro_dot_abapgit EXPORTING io_abapgit_dot_abapgit = lo_dot_abapgit->*.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abapgit_repo_srv DEFINITION.

  PUBLIC SECTION.

    DATA: go_instance TYPE REF TO object.

    METHODS constructor
      RAISING zcx_trm_exception.

    METHODS get_repo_from_package
      IMPORTING iv_package TYPE devclass
      EXPORTING eo_repo    TYPE REF TO lcl_abapgit_repo
      RAISING   zcx_trm_exception.

    CLASS-METHODS get_instance
      RETURNING VALUE(ro_srv) TYPE REF TO lcl_abapgit_repo_srv
      RAISING   zcx_trm_exception.

ENDCLASS.

CLASS lcl_abapgit_repo_srv IMPLEMENTATION.

  METHOD constructor.
    DATA: lo_instance TYPE REF TO data,
          lt_param    TYPE abap_parmbind_tab,
          ls_param    LIKE LINE OF lt_param.
    create_data_ref lo_instance 'ZIF_ABAPGIT_REPO_SRV'.
    add_param 'RI_SRV' lo_instance cl_abap_objectdescr=>receiving.
    call_static_method 'ZCL_ABAPGIT_REPO_SRV' 'GET_INSTANCE'.
    go_instance = lo_instance->*.
  ENDMETHOD.

  METHOD get_instance.
    CREATE OBJECT ro_srv.
  ENDMETHOD.

  METHOD get_repo_from_package.
    DATA: lo_repo    TYPE REF TO data,
          lo_package TYPE REF TO data,
          lt_param   TYPE abap_parmbind_tab,
          ls_param   LIKE LINE OF lt_param.
    create_data_ref lo_repo 'ZIF_ABAPGIT_REPO'.
    GET REFERENCE OF iv_package INTO lo_package.
    add_param 'IV_PACKAGE' lo_package cl_abap_objectdescr=>exporting.
    add_param 'EI_REPO' lo_repo cl_abap_objectdescr=>importing.
    call_object_method go_instance 'ZIF_ABAPGIT_REPO_SRV~GET_REPO_FROM_PACKAGE'.
    CHECK lo_repo->* IS BOUND.
    CREATE OBJECT eo_repo EXPORTING io_repo = lo_repo->*.
  ENDMETHOD.

ENDCLASS.

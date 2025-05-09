*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
INTERFACE lif_abapgit_definitions.
  TYPES ty_abap_language_version TYPE c LENGTH 1.
  TYPES:
    ty_sha1    TYPE c LENGTH 40 .
  TYPES:
    BEGIN OF ty_file_signature,
      path     TYPE string,
      filename TYPE string,
      sha1     TYPE ty_sha1,
    END OF ty_file_signature .
  TYPES:
    BEGIN OF ty_file.
      INCLUDE TYPE ty_file_signature.
  TYPES: data TYPE xstring,
    END OF ty_file .
  TYPES:
    BEGIN OF ty_item_signature,
      obj_type TYPE tadir-object,
      obj_name TYPE tadir-obj_name,
      devclass TYPE devclass,
    END OF ty_item_signature .
  TYPES:
    BEGIN OF ty_item.
      INCLUDE TYPE ty_item_signature.
  TYPES:
      srcsystem             TYPE tadir-srcsystem,
      origlang              TYPE tadir-masterlang,
      inactive              TYPE abap_bool,
      abap_language_version TYPE ty_abap_language_version,
    END OF ty_item .
  TYPES:
    BEGIN OF ty_file_item,
      file TYPE ty_file,
      item TYPE ty_item,
    END OF ty_file_item .
  TYPES:
    ty_files_item_tt TYPE STANDARD TABLE OF ty_file_item WITH DEFAULT KEY .
  TYPES:
    ty_item_tt TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY .
  TYPES:
ty_languages TYPE STANDARD TABLE OF laiso WITH DEFAULT KEY.
ENDINTERFACE.

INTERFACE lif_abapgit_git_definitions.
  TYPES:
    ty_sha1    TYPE c LENGTH 40 .
ENDINTERFACE.

INTERFACE lif_abapgit_dot_abapgit.
  CONSTANTS:
    BEGIN OF c_folder_logic,
      prefix TYPE string VALUE 'PREFIX',
      full   TYPE string VALUE 'FULL',
      mixed  TYPE string VALUE 'MIXED',
    END OF c_folder_logic.
  TYPES:
    BEGIN OF ty_requirement,
      component   TYPE tdevc-dlvunit,
      min_release TYPE saprelease,
      min_patch   TYPE sappatchlv,
    END OF ty_requirement .
  TYPES:
    ty_requirement_tt TYPE STANDARD TABLE OF ty_requirement WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_dot_abapgit,
      name                  TYPE string,
      master_language       TYPE spras,
      i18n_languages        TYPE lif_abapgit_definitions=>ty_languages,
      use_lxe               TYPE abap_bool,
      starting_folder       TYPE string,
      folder_logic          TYPE string,
      ignore                TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      requirements          TYPE ty_requirement_tt,
      version_constant      TYPE string,
      abap_language_version TYPE string,
      original_system       TYPE tadir-srcsystem,
    END OF ty_dot_abapgit .
ENDINTERFACE.

INTERFACE lif_abapgit_persistence.
  TYPES:
    ty_value TYPE c LENGTH 12 .
  TYPES:
    BEGIN OF ty_local_settings,
      display_name                 TYPE string,
      ignore_subpackages           TYPE abap_bool,
      write_protected              TYPE abap_bool,
      only_local_objects           TYPE abap_bool,
      code_inspector_check_variant TYPE sci_chkv,
      block_commit                 TYPE abap_bool,
      main_language_only           TYPE abap_bool,
      labels                       TYPE string,
      transport_request            TYPE trkorr,
      customizing_request          TYPE trkorr,
      flow                         TYPE abap_bool,
      exclude_remote_paths         TYPE string_table,
    END OF ty_local_settings.
  TYPES: BEGIN OF ty_repo_xml,
           url             TYPE string,
           branch_name     TYPE string,
           selected_commit TYPE lif_abapgit_git_definitions=>ty_sha1,
           package         TYPE devclass,
           created_by      TYPE syuname,
           created_at      TYPE timestampl,
           deserialized_by TYPE syuname,
           deserialized_at TYPE timestampl,
           offline         TYPE abap_bool,
           switched_origin TYPE string,
           dot_abapgit     TYPE lif_abapgit_dot_abapgit=>ty_dot_abapgit,
           head_branch     TYPE string,   " HEAD symref of the repo, master branch
           local_settings  TYPE ty_local_settings,
         END OF ty_repo_xml.
  TYPES: BEGIN OF ty_repo,
           key TYPE ty_value.
           INCLUDE TYPE ty_repo_xml.
  TYPES: END OF ty_repo.
ENDINTERFACE.

CLASS lcl_trm_abapgit DEFINITION.

  PUBLIC SECTION.

    DATA: go_instance TYPE REF TO object READ-ONLY.

    CLASS-DATA: tmp_param_tab TYPE abap_parmbind_tab READ-ONLY,
                tmp_param     LIKE LINE OF tmp_param_tab READ-ONLY,
                tmp_exception TYPE REF TO cx_root READ-ONLY.

  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_trm_abapgit IMPLEMENTATION.

ENDCLASS.

CLASS lcl_abapgit_log DEFINITION INHERITING FROM lcl_trm_abapgit.

  PUBLIC SECTION.

    METHODS constructor
      RAISING zcx_trm_exception.

ENDCLASS.

CLASS lcl_abapgit_log IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    create_object go_instance 'ZCL_ABAPGIT_LOG'.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abapgit_zip DEFINITION INHERITING FROM lcl_trm_abapgit.

  PUBLIC SECTION.

    CLASS-METHODS encode_files
      IMPORTING it_files       TYPE lif_abapgit_definitions=>ty_files_item_tt
      RETURNING VALUE(rv_xstr) TYPE xstring
      RAISING   zcx_trm_exception.

ENDCLASS.

CLASS lcl_abapgit_zip IMPLEMENTATION.

  METHOD encode_files.
    DATA: lt_files TYPE REF TO data,
          lv_xstr  TYPE REF TO data.
    create_data lt_files 'ZIF_ABAPGIT_DEFINITIONS=>TY_FILES_ITEM_TT'.
    MOVE-CORRESPONDING it_files TO lt_files->*.
    add_param 'IT_FILES' lt_files cl_abap_objectdescr=>exporting.
    GET REFERENCE OF rv_xstr INTO lv_xstr.
    add_param 'RV_XSTR' lv_xstr cl_abap_objectdescr=>receiving.
    call_static_method 'ZCL_ABAPGIT_ZIP' 'ENCODE_FILES'.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abapgit_dot_abapgit DEFINITION INHERITING FROM lcl_trm_abapgit.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING io_abapgit_dot_abapgit TYPE REF TO object.

    METHODS serialize
      RETURNING VALUE(rv_xstr) TYPE xstring
      RAISING   zcx_trm_exception.

    METHODS set_folder_logic
      IMPORTING iv_logic TYPE string
      RAISING   zcx_trm_exception.

    METHODS get_data
      RETURNING VALUE(rs_data) TYPE lif_abapgit_dot_abapgit=>ty_dot_abapgit
      RAISING   zcx_trm_exception.


    METHODS remove_ignore
      IMPORTING iv_path     TYPE string
                iv_filename TYPE string
      RAISING   zcx_trm_exception.

    CLASS-METHODS build_default
      RETURNING VALUE(ro_dot_abapgit) TYPE REF TO lcl_abapgit_dot_abapgit
      RAISING   zcx_trm_exception.

ENDCLASS.

CLASS lcl_abapgit_dot_abapgit IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    go_instance = io_abapgit_dot_abapgit.
  ENDMETHOD.

  METHOD serialize.
    DATA: lo_xstr  TYPE REF TO data.
    GET REFERENCE OF rv_xstr INTO lo_xstr.
    add_param 'RV_XSTR' lo_xstr cl_abap_objectdescr=>receiving.
    call_object_method go_instance 'SERIALIZE'.
  ENDMETHOD.

  METHOD build_default.
    DATA: lo_dot_abapgit TYPE REF TO data.
    create_data_ref lo_dot_abapgit 'ZCL_ABAPGIT_DOT_ABAPGIT'.
    add_param 'RO_DOT_ABAPGIT' lo_dot_abapgit cl_abap_objectdescr=>receiving.
    call_static_method 'ZCL_ABAPGIT_DOT_ABAPGIT' 'BUILD_DEFAULT'.
    CREATE OBJECT ro_dot_abapgit EXPORTING io_abapgit_dot_abapgit = lo_dot_abapgit->*.
  ENDMETHOD.

  METHOD set_folder_logic.
    DATA: lo_logic TYPE REF TO data.
    GET REFERENCE OF iv_logic INTO lo_logic.
    add_param 'IV_LOGIC' lo_logic cl_abap_objectdescr=>exporting.
    call_object_method go_instance 'SET_FOLDER_LOGIC'.
  ENDMETHOD.

  METHOD get_data.
    DATA ls_data   TYPE REF TO data.
    create_data ls_data 'ZIF_ABAPGIT_DOT_ABAPGIT=>TY_DOT_ABAPGIT'.
    GET REFERENCE OF rs_data INTO ls_data.
    add_param 'RS_DATA' ls_data cl_abap_objectdescr=>receiving.
    call_object_method go_instance 'GET_DATA'.
  ENDMETHOD.

  METHOD remove_ignore.
    DATA: lo_path     TYPE REF TO data,
          lo_filename TYPE REF TO data.
    GET REFERENCE OF iv_path INTO lo_path.
    add_param 'IV_PATH' lo_path cl_abap_objectdescr=>exporting.
    GET REFERENCE OF iv_filename INTO lo_filename.
    add_param 'IV_FILENAME' lo_filename cl_abap_objectdescr=>exporting.
    call_object_method go_instance 'REMOVE_IGNORE'.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abapgit_serialize DEFINITION INHERITING FROM lcl_trm_abapgit.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING io_dot_abapgit    TYPE REF TO lcl_abapgit_dot_abapgit
                is_local_settings TYPE lif_abapgit_persistence=>ty_repo-local_settings
      RAISING   zcx_trm_exception.
    METHODS files_local
      IMPORTING iv_package      TYPE devclass
                ii_log          TYPE REF TO lcl_abapgit_log
      RETURNING VALUE(rt_files) TYPE lif_abapgit_definitions=>ty_files_item_tt
      RAISING   zcx_trm_exception.

ENDCLASS.

CLASS lcl_abapgit_serialize IMPLEMENTATION.

  METHOD constructor.
    DATA: lo_dot_abapgit    TYPE REF TO data,
          ls_local_settings TYPE REF TO data.
    FIELD-SYMBOLS: <fs_dot_abapgit> TYPE any.
    super->constructor( ).
    CREATE DATA lo_dot_abapgit TYPE REF TO ('ZCL_ABAPGIT_DOT_ABAPGIT').
    ASSIGN lo_dot_abapgit->* TO <fs_dot_abapgit>.
    <fs_dot_abapgit> ?= io_dot_abapgit->go_instance.
    add_param 'IO_DOT_ABAPGIT' lo_dot_abapgit cl_abap_objectdescr=>exporting.
    create_data ls_local_settings 'ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-LOCAL_SETTINGS'.
    MOVE-CORRESPONDING is_local_settings TO ls_local_settings->*.
    add_param 'IS_LOCAL_SETTINGS' ls_local_settings cl_abap_objectdescr=>exporting.
    create_object go_instance 'ZCL_ABAPGIT_SERIALIZE'.
  ENDMETHOD.

  METHOD files_local.
    DATA: lv_package TYPE REF TO data,
          lo_log     TYPE REF TO data,
          lt_files   TYPE REF TO data.
    FIELD-SYMBOLS: <fs_log> TYPE any.
    create_data lt_files 'ZIF_ABAPGIT_DEFINITIONS=>TY_FILES_ITEM_TT'.
    GET REFERENCE OF iv_package INTO lv_package.
    add_param 'IV_PACKAGE' lv_package cl_abap_objectdescr=>exporting.
    CREATE DATA lo_log TYPE REF TO ('ZCL_ABAPGIT_LOG').
    ASSIGN lo_log->* TO <fs_log>.
    <fs_log> ?= ii_log->go_instance.
    add_param 'II_LOG' lo_log cl_abap_objectdescr=>exporting.
    add_param 'RT_FILES' lt_files cl_abap_objectdescr=>receiving.
    call_object_method go_instance 'FILES_LOCAL'.
    MOVE-CORRESPONDING lt_files->* TO rt_files.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abapgit_repo DEFINITION INHERITING FROM lcl_trm_abapgit.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING io_repo TYPE REF TO object.

    METHODS get_dot_abapgit
      RETURNING VALUE(ro_dot_abapgit) TYPE REF TO lcl_abapgit_dot_abapgit
      RAISING   zcx_trm_exception.

ENDCLASS.

CLASS lcl_abapgit_repo IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    go_instance = io_repo.
  ENDMETHOD.

  METHOD get_dot_abapgit.
    DATA: lo_dot_abapgit TYPE REF TO data.
    create_data_ref lo_dot_abapgit 'ZCL_ABAPGIT_DOT_ABAPGIT'.
    add_param 'RO_DOT_ABAPGIT' lo_dot_abapgit cl_abap_objectdescr=>receiving.
    call_object_method go_instance 'GET_DOT_ABAPGIT'.
    CREATE OBJECT ro_dot_abapgit EXPORTING io_abapgit_dot_abapgit = lo_dot_abapgit->*.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abapgit_repo_srv DEFINITION INHERITING FROM lcl_trm_abapgit.

  PUBLIC SECTION.

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
    DATA: lo_instance TYPE REF TO data.
    super->constructor( ).
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
          lo_package TYPE REF TO data.
    create_data_ref lo_repo 'ZIF_ABAPGIT_REPO'.
    GET REFERENCE OF iv_package INTO lo_package.
    add_param 'IV_PACKAGE' lo_package cl_abap_objectdescr=>exporting.
    add_param 'EI_REPO' lo_repo cl_abap_objectdescr=>importing.
    call_object_method go_instance 'ZIF_ABAPGIT_REPO_SRV~GET_REPO_FROM_PACKAGE'.
    CHECK lo_repo->* IS BOUND.
    CREATE OBJECT eo_repo EXPORTING io_repo = lo_repo->*.
  ENDMETHOD.

ENDCLASS.

*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_abapgit_dot_abapgit DEFINITION.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        io_abapgit_dot_abapgit TYPE REF TO object.

    METHODS serialize
      RETURNING
        VALUE(rv_xstr) TYPE xstring.

  PRIVATE SECTION.

    DATA: go_instance TYPE REF TO object.

ENDCLASS.

CLASS lcl_abapgit_dot_abapgit IMPLEMENTATION.

  METHOD constructor.
    go_instance = io_abapgit_dot_abapgit.
  ENDMETHOD.

  METHOD serialize.
    CALL METHOD go_instance->('SERIALIZE')
      RECEIVING
        rv_xstr = rv_xstr.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abapgit_repo DEFINITION.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        io_repo TYPE REF TO object.

    METHODS get_dot_abapgit
      RETURNING
        VALUE(ro_dot_abapgit) TYPE REF TO lcl_abapgit_dot_abapgit.

  PRIVATE SECTION.

    DATA: go_instance TYPE REF TO object.

ENDCLASS.

CLASS lcl_abapgit_repo IMPLEMENTATION.

  METHOD constructor.
    go_instance = io_repo.
  ENDMETHOD.

  METHOD get_dot_abapgit.
    DATA: lo_repo        TYPE REF TO object,
          lo_dot_abapgit TYPE REF TO data.
    CREATE DATA lo_dot_abapgit TYPE REF TO ('ZCL_ABAPGIT_DOT_ABAPGIT').
    CALL METHOD go_instance->('GET_DOT_ABAPGIT')
      RECEIVING
        ro_dot_abapgit = lo_dot_abapgit->*.
    CREATE OBJECT ro_dot_abapgit EXPORTING io_abapgit_dot_abapgit = lo_dot_abapgit->*.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_abapgit_repo_srv DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS get_repo_from_package
      IMPORTING
        iv_package TYPE devclass
      EXPORTING
        eo_repo    TYPE REF TO lcl_abapgit_repo.

    CLASS-METHODS get_instance
      RETURNING VALUE(ro_srv) TYPE REF TO lcl_abapgit_repo_srv.

  PRIVATE SECTION.

    DATA: go_instance TYPE REF TO object.

ENDCLASS.

CLASS lcl_abapgit_repo_srv IMPLEMENTATION.

  METHOD constructor.
    DATA lo_instance TYPE REF TO data.
    CREATE DATA lo_instance TYPE REF TO ('ZIF_ABAPGIT_REPO_SRV').
    CALL METHOD ('ZCL_ABAPGIT_REPO_SRV')=>get_instance
      RECEIVING
        ri_srv = lo_instance->*.
    go_instance = lo_instance->*.
  ENDMETHOD.

  METHOD get_instance.
    CREATE OBJECT ro_srv.
  ENDMETHOD.

  METHOD get_repo_from_package.
    DATA: lo_srv  TYPE REF TO object,
          lo_repo TYPE REF TO data.
    CREATE DATA lo_repo TYPE REF TO ('ZIF_ABAPGIT_REPO').
    CALL METHOD go_instance->('ZIF_ABAPGIT_REPO_SRV~GET_REPO_FROM_PACKAGE')
      EXPORTING
        iv_package = iv_package
      IMPORTING
        ei_repo    = lo_repo->*.
    CREATE OBJECT eo_repo EXPORTING io_repo = lo_repo->*.
  ENDMETHOD.

ENDCLASS.

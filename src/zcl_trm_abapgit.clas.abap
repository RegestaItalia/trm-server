CLASS zcl_trm_abapgit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.
    TYPES: tyt_tadir TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY.

    CLASS-METHODS get_dot_abapgit
      IMPORTING iv_devclass           TYPE devclass
      RETURNING VALUE(rv_dot_abapgit) TYPE xstring
      RAISING   zcx_trm_exception.

    CLASS-METHODS serialize
      IMPORTING iv_devclass TYPE devclass
      EXPORTING ev_zip      TYPE xstring
                et_objects  TYPE tyt_tadir
      RAISING   zcx_trm_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS handle_root_exception
      IMPORTING io_exception TYPE REF TO cx_root
      RAISING   zcx_trm_exception.

ENDCLASS.



CLASS zcl_trm_abapgit IMPLEMENTATION.

  METHOD handle_root_exception.
    DATA: lo_exception     TYPE REF TO cx_root,
          lo_trm_exception TYPE REF TO zcx_trm_exception,
          lv_dummy         TYPE string.

    lo_exception = io_exception.
    WHILE lo_exception->previous IS BOUND.
      lo_exception = lo_exception->previous.
    ENDWHILE.
    IF lo_exception IS BOUND.
      IF lo_exception IS INSTANCE OF zcx_trm_exception.
        lo_trm_exception ?= lo_exception.
        IF lo_trm_exception->reason( ) IS INITIAL OR lo_trm_exception->reason( ) EQ zcx_trm_exception=>c_reason-generic.
          MESSAGE ID lo_trm_exception->message-msgid
            TYPE 'E'
            NUMBER lo_trm_exception->message-msgno
            WITH lo_trm_exception->message-msgv1
                 lo_trm_exception->message-msgv2
                 lo_trm_exception->message-msgv3
                 lo_trm_exception->message-msgv4
            INTO lv_dummy.
          zcx_trm_exception=>raise(
            EXPORTING
              iv_reason  = zcx_trm_exception=>c_reason-abapgit_intergration
          ).
        ELSE.
          RAISE EXCEPTION lo_trm_exception.
        ENDIF.
      ELSE.
        zcx_trm_exception=>raise(
          EXPORTING
            iv_reason  = zcx_trm_exception=>c_reason-abapgit_intergration
            iv_message = lo_exception->get_text( )
        ).
      ENDIF.
    ELSE.
      zcx_trm_exception=>raise(
        EXPORTING
          iv_reason  = zcx_trm_exception=>c_reason-abapgit_intergration
          iv_message = 'Unknown exception raised'
      ).
    ENDIF.
  ENDMETHOD.

  METHOD get_dot_abapgit.
    DATA lo_repo TYPE REF TO lcl_abapgit_repo.
    lcl_abapgit_repo_srv=>get_instance( )->get_repo_from_package(
      EXPORTING
        iv_package = iv_devclass
      IMPORTING
        eo_repo    = lo_repo
    ).
    IF lo_repo IS NOT BOUND.
      zcx_trm_exception=>raise( iv_message = 'Repository for package ' && iv_devclass && ' not found' ).
    ENDIF.
    rv_dot_abapgit = lo_repo->get_dot_abapgit( )->serialize( ).
  ENDMETHOD.

  METHOD serialize.
    DATA: lx_root           TYPE REF TO cx_root,
          ls_local_settings TYPE REF TO data,
          lo_repo           TYPE REF TO lcl_abapgit_repo,
          lo_dot_abapgit    TYPE REF TO lcl_abapgit_dot_abapgit,
          lo_serialize      TYPE REF TO lcl_abapgit_serialize.
    FIELD-SYMBOLS: <fs_folder_logic>  TYPE string.

    create_data ls_local_settings 'ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-LOCAL_SETTINGS'.

    lcl_abapgit_repo_srv=>get_instance( )->get_repo_from_package(
      EXPORTING
        iv_package = iv_devclass
      IMPORTING
        eo_repo    = lo_repo
    ).
    IF lo_repo IS BOUND.
      lo_dot_abapgit = lo_repo->get_dot_abapgit( ).
    ENDIF.
    IF lo_dot_abapgit IS NOT BOUND.
      lo_dot_abapgit = lcl_abapgit_dot_abapgit=>build_default( ).
      ASSIGN ('ZIF_ABAPGIT_DOT_ABAPGIT=>C_FOLDER_LOGIC-FULL') TO <fs_folder_logic>.
      IF sy-subrc EQ 0.
        lo_dot_abapgit->set_folder_logic( <fs_folder_logic> ).
      ELSE.
        zcx_trm_exception=>raise( iv_message = 'Cannot set folder logic to FULL' ).
      ENDIF.
    ENDIF.
    CREATE OBJECT lo_serialize
      EXPORTING
        io_dot_abapgit    = lo_dot_abapgit
        is_local_settings = ls_local_settings.
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.
    serialize(
      EXPORTING
        iv_devclass = 'ZTRM'
*      IMPORTING
*        ev_zip      =
*        et_objects  =
    ).
  ENDMETHOD.

ENDCLASS.

CLASS zcl_trm_abapgit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

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

ENDCLASS.



CLASS zcl_trm_abapgit IMPLEMENTATION.

  METHOD get_dot_abapgit.
    DATA lo_repo TYPE REF TO lcl_abapgit_repo.
    lcl_abapgit_repo_srv=>get_instance( )->get_repo_from_package(
      EXPORTING
        iv_package = iv_devclass
      IMPORTING
        eo_repo    = lo_repo
    ).
    IF lo_repo IS NOT BOUND.
      zcx_trm_exception=>raise(
        iv_reason  = zcx_trm_exception=>c_reason-abapgit_intergration
        iv_message = 'Repository for package ' && iv_devclass && ' not found'
      ).
    ENDIF.
    rv_dot_abapgit = lo_repo->get_dot_abapgit( )->serialize( ).
  ENDMETHOD.

  METHOD serialize.
    DATA: ls_local_settings TYPE lif_abapgit_persistence=>ty_repo-local_settings,
          lo_repo           TYPE REF TO lcl_abapgit_repo,
          lo_dot_abapgit    TYPE REF TO lcl_abapgit_dot_abapgit,
          lo_log            TYPE REF TO lcl_abapgit_log,
          lo_serialize      TYPE REF TO lcl_abapgit_serialize,
          lt_files          TYPE lif_abapgit_definitions=>ty_files_item_tt,
          ls_file           LIKE LINE OF lt_files,
          lt_items          TYPE lif_abapgit_definitions=>ty_item_tt.

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
      lo_dot_abapgit->set_folder_logic( lif_abapgit_dot_abapgit=>c_folder_logic-full ).
    ENDIF.
    CREATE OBJECT lo_log.
    CREATE OBJECT lo_serialize
      EXPORTING
        io_dot_abapgit    = lo_dot_abapgit
        is_local_settings = ls_local_settings.
    lt_files = lo_serialize->files_local(
      iv_package = iv_devclass
      ii_log     = lo_log
    ).
    ev_zip = lcl_abapgit_zip=>encode_files(
      EXPORTING
        it_files = lt_files
    ).
    LOOP AT lt_files INTO ls_file WHERE item IS NOT INITIAL.
      READ TABLE lt_items TRANSPORTING NO FIELDS WITH KEY obj_type = ls_file-item-obj_type obj_name = ls_file-item-obj_name.
      CHECK sy-subrc <> 0.
      APPEND ls_file-item TO lt_items.
    ENDLOOP.
    IF lt_items[] IS NOT INITIAL.
      SELECT *
        FROM tadir
        INTO CORRESPONDING FIELDS OF TABLE et_objects
        FOR ALL ENTRIES IN lt_items
        WHERE pgmid EQ 'R3TR'
          AND object EQ lt_items-obj_type
          AND obj_name EQ lt_items-obj_name.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

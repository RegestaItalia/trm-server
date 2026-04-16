"! ABAPGit API exposure
CLASS /atrm/cl_abapgit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_ser_obj,
             pgmid     TYPE pgmid,
             object    TYPE trobjtype,
             obj_name  TYPE sobj_name,
             full_path TYPE string,
           END OF ty_ser_obj.
    TYPES: tyt_ser_objs TYPE STANDARD TABLE OF ty_ser_obj WITH DEFAULT KEY.

    "! Retrieves the serialized `.abapgit` configuration file for a given package
    "! @parameter devclass | Name of the development class (package)
    "! parameter dot_abapgit | Serialized .abapgit content as xstring
    "! @raising /atrm/cx_exception | Raised if repository cannot be found
    CLASS-METHODS get_dot_abapgit
      IMPORTING devclass           TYPE devclass
      RETURNING VALUE(dot_abapgit) TYPE xstring
      RAISING   /atrm/cx_exception.

    "! Serializes the ABAP package contents into ZIP format using abapGit
    "! @parameter devclass | Name of the development class (package)
    "! @parameter zip | ZIP file in xstring format
    "! @parameter objects | List of TADIR objects found in the package
    "! @raising /atrm/cx_exception | Raised on serialization errors
    CLASS-METHODS serialize
      IMPORTING devclass TYPE devclass
      EXPORTING zip      TYPE xstring
                objects  TYPE tyt_ser_objs
      RAISING   /atrm/cx_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS /atrm/cl_abapgit IMPLEMENTATION.

  METHOD get_dot_abapgit.
    DATA lo_repo TYPE REF TO lcl_abapgit_repo.
    lcl_abapgit_repo_srv=>get_instance( )->get_repo_from_package(
      EXPORTING
        iv_package = devclass
      IMPORTING
        eo_repo    = lo_repo
    ).
    IF lo_repo IS NOT BOUND.
      /atrm/cx_exception=>raise(
        iv_reason  = /atrm/cx_exception=>c_reason-abapgit_intergration
        iv_message = 'Repository for package ' && devclass && ' not found'
      ).
    ENDIF.
    dot_abapgit = lo_repo->get_dot_abapgit( )->serialize( ).
  ENDMETHOD.

  METHOD serialize.
    DATA: lo_repo        TYPE REF TO lcl_abapgit_repo,
          lo_dot_abapgit TYPE REF TO lcl_abapgit_dot_abapgit,
          lv_ignore      TYPE string,
          lo_log         TYPE REF TO lcl_abapgit_log,
          lo_serialize   TYPE REF TO lcl_abapgit_serialize,
          lt_files       TYPE lif_abapgit_definitions=>ty_files_item_tt,
          ls_file        LIKE LINE OF lt_files,
          ls_ser_obj     LIKE LINE OF objects.

    lcl_abapgit_repo_srv=>get_instance( )->get_repo_from_package(
      EXPORTING
        iv_package = devclass
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
    LOOP AT lo_dot_abapgit->get_data( )-ignore INTO lv_ignore.
      lo_dot_abapgit->remove_ignore(
        iv_path     = lv_ignore
        iv_filename = ''
      ).
    ENDLOOP.
    CREATE OBJECT lo_log.
    CREATE OBJECT lo_serialize
      EXPORTING
        io_dot_abapgit = lo_dot_abapgit.
    lt_files = lo_serialize->files_local(
      iv_package = devclass
      ii_log     = lo_log
    ).
    zip = lcl_abapgit_zip=>encode_files(
      EXPORTING
        it_files = lt_files
    ).
    LOOP AT lt_files INTO ls_file WHERE item IS NOT INITIAL.
      CLEAR ls_ser_obj.
      ls_ser_obj-pgmid = 'R3TR'.
      ls_ser_obj-object = ls_file-item-obj_type.
      ls_ser_obj-obj_name = ls_file-item-obj_name.
      ls_ser_obj-full_path = ls_file-file-path && ls_file-file-filename.
      APPEND ls_ser_obj TO objects.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

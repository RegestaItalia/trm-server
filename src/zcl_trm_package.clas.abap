"! ABAP package API
CLASS zcl_trm_package DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    "! Constructor
    "! @parameter iv_devclass | Development class (package name) to operate on
    METHODS constructor
      IMPORTING iv_devclass TYPE devclass.

    "! Creates a new ABAP package based on the given data
    "! @parameter is_data | Input structure containing package metadata
    "! @parameter ro_package | Created package instance
    "! @raising zcx_trm_exception | Raised if package creation fails
    CLASS-METHODS create
      IMPORTING is_data           TYPE scompkdtln
      RETURNING VALUE(ro_package) TYPE REF TO zcl_trm_package
      RAISING   zcx_trm_exception.

    "! Retrieves all TADIR objects
    "! @parameter et_tadir | Table of TADIR entries
    "! @raising zcx_trm_exception | Raised if object retrieval fails
    METHODS get_objects
      EXPORTING et_tadir TYPE scts_tadir
      RAISING   zcx_trm_exception.

    "! Adjusts package interface data like super package and devlayer
    "! @parameter iv_parentcl | Superpackage (optional)
    "! @parameter iv_rm_parentcl | Flag to remove superpackage link
    "! @parameter iv_devlayer | Development layer to assign (optional)
    "! @raising zcx_trm_exception | Raised if update fails
    METHODS interface
      IMPORTING iv_parentcl    TYPE devclass OPTIONAL
                iv_rm_parentcl TYPE flag OPTIONAL
                iv_devlayer    TYPE devlayer OPTIONAL
      RAISING   zcx_trm_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: gv_devclass TYPE devclass.

    CLASS-METHODS modify_package_data
      IMPORTING is_package_data_sign TYPE scompksign
                iv_suppress_dialog   TYPE flag
      CHANGING  cs_package_data      TYPE scompkdtln
                cv_transport_request TYPE e070-trkorr
      RAISING   zcx_trm_exception.

ENDCLASS.



CLASS zcl_trm_package IMPLEMENTATION.


  METHOD constructor.
    gv_devclass = iv_devclass.
  ENDMETHOD.


  METHOD create.
    DATA ls_data LIKE is_data.
    DATA lo_package TYPE REF TO if_package.
    MOVE is_data TO ls_data.

    IF ls_data-as4user IS INITIAL.
      ls_data-as4user = sy-uname.
    ENDIF.
    IF ls_data-masterlang IS INITIAL.
      ls_data-masterlang = sy-langu.
    ENDIF.

    TRY.
        CALL METHOD ('CL_PACKAGE_FACTORY')=>('IF_PACKAGE_FACTORY~CREATE_NEW_PACKAGE')
          EXPORTING
            i_reuse_deleted_object = 'X'
            i_suppress_dialog      = 'X'
          IMPORTING
            e_package              = lo_package
          CHANGING
            c_package_data         = ls_data
          EXCEPTIONS
            OTHERS                 = 1.
      CATCH cx_sy_dyn_call_param_not_found.
        zcx_trm_exception=>raise( iv_reason  = zcx_trm_exception=>c_reason-dyn_call_param_not_found ).
    ENDTRY.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.

    lo_package->save(
      EXPORTING
        i_suppress_dialog      = 'X'
        i_suppress_corr_insert = 'X'
      EXCEPTIONS
        OTHERS                 = 1
    ).
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.

    lo_package->set_changeable(
      EXPORTING
        i_changeable      = ' '
        i_suppress_dialog = 'X'
      EXCEPTIONS
        OTHERS            = 1
    ).
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.

    IF ls_data-devclass(1) <> '$'.
      DATA: lv_objname    TYPE sobj_name,
            lv_devclass   TYPE devclass,
            lv_author     TYPE responsibl,
            lv_masterlang TYPE masterlang.
      lv_objname = ls_data-devclass.
      lv_devclass = ls_data-devclass.
      lv_author = ls_data-as4user.
      lv_masterlang = ls_data-masterlang.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_test_modus       = abap_false
          wi_tadir_pgmid      = 'R3TR'
          wi_tadir_object     = 'DEVC'
          wi_tadir_obj_name   = lv_objname
          wi_tadir_devclass   = lv_devclass
          wi_tadir_author     = lv_author
          wi_tadir_masterlang = lv_masterlang
        EXCEPTIONS
          OTHERS              = 0.
    ENDIF.

    CREATE OBJECT ro_package EXPORTING iv_devclass = ls_data-devclass.
  ENDMETHOD.


  METHOD get_objects.
    CALL FUNCTION 'TRINT_SELECT_OBJECTS'
      EXPORTING
        iv_devclass      = gv_devclass
        iv_via_selscreen = ' '
      IMPORTING
        et_objects_tadir = et_tadir
      EXCEPTIONS
        OTHERS           = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
  ENDMETHOD.


  METHOD modify_package_data.
    DATA: subrc LIKE sy-subrc.
    DATA: lo_package TYPE REF TO if_package.

* load package
    CALL METHOD cl_package_factory=>load_package
      EXPORTING
        i_package_name = cs_package_data-devclass
        i_force_reload = 'X'
      IMPORTING
        e_package      = lo_package
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.

* lock package
    CALL METHOD lo_package->set_changeable
      EXPORTING
        i_changeable              = 'X'
        i_suppress_dialog         = 'D'
      EXCEPTIONS
        object_already_changeable = 0                       "ignore it
        OTHERS                    = 1.

    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.

* modify package
    CALL METHOD lo_package->set_all_attributes
      EXPORTING
        i_package_data = cs_package_data
        i_data_sign    = is_package_data_sign
      EXCEPTIONS
        OTHERS         = 1.
*
    IF sy-subrc <> 0.
*   try to unlock the package, exceptions are tolerated
      subrc = sy-subrc.

      CALL METHOD lo_package->set_changeable
        EXPORTING
          i_changeable      = ' '
          i_suppress_dialog = 'D'
        EXCEPTIONS
          OTHERS            = 0.
      zcx_trm_exception=>raise( ).
    ENDIF.

* save package
    CALL METHOD lo_package->save
      EXPORTING
        i_transport_request    = cv_transport_request
        i_suppress_dialog      = iv_suppress_dialog
        i_suppress_corr_insert = 'X'
      IMPORTING
        e_transport_request    = cv_transport_request
      EXCEPTIONS
        OTHERS                 = 1.

    IF sy-subrc <> 0.
*   try to undo the changes, exceptions are tolerated
*   (Note: if successful, this also unlocks the package)
      subrc = sy-subrc.
      CALL METHOD lo_package->undo_all_changes
        EXCEPTIONS
          OTHERS = 0.
      zcx_trm_exception=>raise( ).
    ENDIF.

* unlock package
    CALL METHOD lo_package->set_changeable
      EXPORTING
        i_changeable            = ' '
        i_suppress_dialog       = 'D'
      EXCEPTIONS
        object_already_unlocked = 0                       "ignore
        OTHERS                  = 1.

    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
  ENDMETHOD.


  METHOD interface.
    DATA: ls_modify_sign TYPE scompksign,
          ls_pack_data   TYPE scompkdtln,
          ls_cr          TYPE e070-trkorr.
    IF iv_rm_parentcl EQ 'X'.
      "SAP Note 636704
      ls_modify_sign-parentcl = 'X'.
      ls_pack_data-devclass   = gv_devclass.
      modify_package_data(
        EXPORTING
          is_package_data_sign = ls_modify_sign
          iv_suppress_dialog   = 'X'
        CHANGING
          cs_package_data      = ls_pack_data
          cv_transport_request = ls_cr
      ).
      CLEAR ls_modify_sign.
      CLEAR ls_pack_data.
      CLEAR ls_cr.
    ELSEIF iv_parentcl IS NOT INITIAL.
      DATA lo_package TYPE REF TO if_package.
      cl_package_factory=>load_package(
        EXPORTING
          i_package_name = gv_devclass
        IMPORTING
          e_package      = lo_package
        EXCEPTIONS
          OTHERS         = 1 ).
      lo_package->set_changeable(
        EXPORTING
          i_changeable      = 'X'
          i_suppress_dialog = 'D'
        EXCEPTIONS
          object_already_changeable = 0                       "ignore it
          OTHERS            = 1 ).
      IF sy-subrc <> 0.
        zcx_trm_exception=>raise( ).
      ENDIF.
      lo_package->set_super_package_name(
        EXPORTING
          i_super_package_name = iv_parentcl
        EXCEPTIONS
          OTHERS               = 1 ).
      IF sy-subrc <> 0.
        zcx_trm_exception=>raise( ).
      ENDIF.
      lo_package->save(
        EXPORTING
          i_suppress_dialog      = 'X'
          i_suppress_corr_insert = 'X'
        EXCEPTIONS
          OTHERS                 = 1 ).
      IF sy-subrc <> 0.
        zcx_trm_exception=>raise( ).
      ENDIF.
      lo_package->set_changeable(
        EXPORTING
          i_changeable      = ' '
          i_suppress_dialog = 'D'
        EXCEPTIONS
          OTHERS            = 1 ).
      IF sy-subrc <> 0.
        zcx_trm_exception=>raise( ).
      ENDIF.
    ENDIF.
    IF iv_devlayer IS NOT INITIAL.
      ls_modify_sign-pdevclass = 'X'.
      ls_pack_data-devclass   = gv_devclass.
      ls_pack_data-pdevclass   = iv_devlayer.
      modify_package_data(
        EXPORTING
          is_package_data_sign = ls_modify_sign
          iv_suppress_dialog   = 'X'
        CHANGING
          cs_package_data      = ls_pack_data
          cv_transport_request = ls_cr
      ).
      CLEAR ls_modify_sign.
      CLEAR ls_pack_data.
      CLEAR ls_cr.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

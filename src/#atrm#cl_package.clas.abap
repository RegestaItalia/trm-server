CLASS /atrm/cl_package DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING devclass TYPE devclass.

    CLASS-METHODS create
      IMPORTING data           TYPE scompkdtln
      RETURNING VALUE(package) TYPE REF TO /atrm/cl_package
      RAISING   /atrm/cx_exception.

    METHODS get_subpackages
      RETURNING VALUE(subpackages) TYPE cl_pak_package_queries=>tt_subpackage_info.

    METHODS get_objects
      IMPORTING incl_sub TYPE flag DEFAULT ' '
      EXPORTING tadir    TYPE scts_tadir
      RAISING   /atrm/cx_exception.

    METHODS interface
      IMPORTING parentcl    TYPE devclass OPTIONAL
                rm_parentcl TYPE flag OPTIONAL
                devlayer    TYPE devlayer OPTIONAL
      RAISING   /atrm/cx_exception.

    METHODS get_objs_lock
      IMPORTING incl_sub        TYPE flag DEFAULT ' '
      RETURNING VALUE(obj_lock) TYPE /atrm/object_lock_t
      RAISING   /atrm/cx_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: gv_devclass TYPE devclass.

    CLASS-METHODS modify_package_data
      IMPORTING package_data_sign TYPE scompksign
      CHANGING  package_data      TYPE scompkdtln
                transport_request TYPE e070-trkorr
      RAISING   /atrm/cx_exception.
ENDCLASS.



CLASS /atrm/cl_package IMPLEMENTATION.


  METHOD constructor.
    gv_devclass = devclass.
  ENDMETHOD.


  METHOD create.
    DATA ls_data LIKE data.
    DATA lo_package TYPE REF TO if_package.
    MOVE data TO ls_data.

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
        /atrm/cx_exception=>raise( iv_reason  = /atrm/cx_exception=>c_reason-dyn_call_param_not_found ).
    ENDTRY.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.

    lo_package->save(
      EXPORTING
        i_suppress_dialog      = 'X'
        i_suppress_corr_insert = 'X'
      EXCEPTIONS
        OTHERS                 = 1
    ).
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.

    lo_package->set_changeable(
      EXPORTING
        i_changeable      = ' '
        i_suppress_dialog = 'X'
      EXCEPTIONS
        OTHERS            = 1
    ).
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
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

    CREATE OBJECT package EXPORTING devclass = ls_data-devclass.
  ENDMETHOD.


  METHOD get_objects.
    DATA: lt_devclass    TYPE STANDARD TABLE OF devclass,
          lt_subpackages TYPE cl_pak_package_queries=>tt_subpackage_info,
          ls_subpackage  LIKE LINE OF lt_subpackages,
          lv_devclass    TYPE devclass,
          lt_tadir       LIKE tadir.
    IF incl_sub EQ 'X'.
      lt_subpackages = get_subpackages( ).
      LOOP AT lt_subpackages INTO ls_subpackage.
        APPEND ls_subpackage-package TO lt_devclass.
      ENDLOOP.
    ENDIF.
    APPEND gv_devclass TO lt_devclass.
    LOOP AT lt_devclass INTO lv_devclass.
      CLEAR lt_tadir[].
      CALL FUNCTION 'TRINT_SELECT_OBJECTS'
        EXPORTING
          iv_devclass      = lv_devclass
          iv_via_selscreen = ' '
        IMPORTING
          et_objects_tadir = lt_tadir
        EXCEPTIONS
          OTHERS           = 1.
      IF sy-subrc <> 0.
        /atrm/cx_exception=>raise( ).
      ENDIF.
      APPEND LINES OF lt_tadir TO tadir.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_package_data.
    DATA: subrc LIKE sy-subrc.
    DATA: lo_package TYPE REF TO if_package.

* load package
    CALL METHOD cl_package_factory=>load_package
      EXPORTING
        i_package_name = package_data-devclass
        i_force_reload = 'X'
      IMPORTING
        e_package      = lo_package
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.

* lock package
    CALL METHOD lo_package->set_changeable
      EXPORTING
        i_changeable                 = 'X'
        i_suppress_dialog            = 'D'
        i_suppress_access_permission = 'X'
      EXCEPTIONS
        object_already_changeable    = 0                       "ignore it
        OTHERS                       = 1.

    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.

* modify package
    CALL METHOD lo_package->set_all_attributes
      EXPORTING
        i_package_data = package_data
        i_data_sign    = package_data_sign
      EXCEPTIONS
        OTHERS         = 1.
*
    IF sy-subrc <> 0.
*   try to unlock the package, exceptions are tolerated
      subrc = sy-subrc.

      CALL METHOD lo_package->set_changeable
        EXPORTING
          i_changeable                 = ' '
          i_suppress_dialog            = 'D'
          i_suppress_access_permission = 'X'
        EXCEPTIONS
          OTHERS                       = 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.

* save package
    CALL METHOD lo_package->save
      EXPORTING
        i_transport_request    = transport_request
        i_suppress_dialog      = 'X'
        i_suppress_corr_insert = 'X'
      IMPORTING
        e_transport_request    = transport_request
      EXCEPTIONS
        OTHERS                 = 1.

    IF sy-subrc <> 0.
*   try to undo the changes, exceptions are tolerated
*   (Note: if successful, this also unlocks the package)
      subrc = sy-subrc.
      CALL METHOD lo_package->undo_all_changes
        EXCEPTIONS
          OTHERS = 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.

* unlock package
    CALL METHOD lo_package->set_changeable
      EXPORTING
        i_changeable                 = ' '
        i_suppress_dialog            = 'D'
        i_suppress_access_permission = 'X'
      EXCEPTIONS
        object_already_unlocked      = 0                       "ignore
        OTHERS                       = 1.

    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.
  ENDMETHOD.


  METHOD interface.
    DATA: ls_modify_sign TYPE scompksign,
          ls_pack_data   TYPE scompkdtln,
          ls_cr          TYPE e070-trkorr.
    IF rm_parentcl EQ 'X'.
      "SAP Note 636704
      ls_modify_sign-parentcl = 'X'.
      ls_pack_data-devclass   = gv_devclass.
      modify_package_data(
        EXPORTING
          package_data_sign = ls_modify_sign
        CHANGING
          package_data      = ls_pack_data
          transport_request = ls_cr
      ).
      CLEAR ls_modify_sign.
      CLEAR ls_pack_data.
      CLEAR ls_cr.
    ELSEIF parentcl IS NOT INITIAL.
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
          i_changeable                 = 'X'
          i_suppress_dialog            = 'D'
          i_suppress_access_permission = 'X'
        EXCEPTIONS
          object_already_changeable = 0                       "ignore it
          OTHERS            = 1 ).
      IF sy-subrc <> 0.
        /atrm/cx_exception=>raise( ).
      ENDIF.
      lo_package->set_super_package_name(
        EXPORTING
          i_super_package_name = parentcl
        EXCEPTIONS
          OTHERS               = 1 ).
      IF sy-subrc <> 0.
        /atrm/cx_exception=>raise( ).
      ENDIF.
      lo_package->save(
        EXPORTING
          i_suppress_dialog      = 'X'
          i_suppress_corr_insert = 'X'
        EXCEPTIONS
          OTHERS                 = 1 ).
      IF sy-subrc <> 0.
        /atrm/cx_exception=>raise( ).
      ENDIF.
      lo_package->set_changeable(
        EXPORTING
          i_changeable                 = ' '
          i_suppress_dialog            = 'D'
          i_suppress_access_permission = 'X'
        EXCEPTIONS
          OTHERS            = 1 ).
      IF sy-subrc <> 0.
        /atrm/cx_exception=>raise( ).
      ENDIF.
    ENDIF.
    IF devlayer IS NOT INITIAL.
      ls_modify_sign-pdevclass = 'X'.
      ls_pack_data-devclass   = gv_devclass.
      ls_pack_data-pdevclass   = devlayer.
      modify_package_data(
        EXPORTING
          package_data_sign = ls_modify_sign
        CHANGING
          package_data      = ls_pack_data
          transport_request = ls_cr
      ).
      CLEAR ls_modify_sign.
      CLEAR ls_pack_data.
      CLEAR ls_cr.
    ENDIF.
  ENDMETHOD.

  METHOD get_subpackages.
    cl_pak_package_queries=>get_all_subpackages(
      EXPORTING
        im_package                    = gv_devclass
        im_also_local_packages        = 'X'
      IMPORTING
        et_subpackages                = subpackages
      EXCEPTIONS
        no_package_specified          = 1
        package_has_no_tdevc_record   = 2
        package_has_no_tadir_record   = 3
        package_does_not_exist        = 4
        invalid_superpackage          = 5
        no_output_parameter_requested = 6
        OTHERS                        = 7
    ).
  ENDMETHOD.

  METHOD get_objs_lock.
    TYPES: BEGIN OF ty_aux,
             pgmid    TYPE e071-pgmid,
             object   TYPE e071-object,
             obj_name TYPE e071-obj_name,
             trkorr   TYPE e070-trkorr,
             strkorr  TYPE e070-strkorr,
           END OF ty_aux.
    DATA: lt_objects     TYPE scts_tadir,
          ls_object      LIKE LINE OF lt_objects,
          lt_objects_aux TYPE STANDARD TABLE OF ty_aux,
          lt_result_aux  TYPE STANDARD TABLE OF ty_aux,
          ls_result_aux  LIKE LINE OF lt_result_aux.
    FIELD-SYMBOLS: <fs_object_aux> TYPE ty_aux,
                   <fs_obj_lock>   TYPE /atrm/object_lock.
    get_objects(
      EXPORTING
        incl_sub = incl_sub
      IMPORTING
        tadir    = lt_objects
    ).
    DELETE lt_objects WHERE pgmid EQ 'R3TR' AND object EQ 'DEVC'.
    CHECK lt_objects[] IS NOT INITIAL.
    LOOP AT lt_objects INTO ls_object.
      UNASSIGN <fs_object_aux>.
      APPEND INITIAL LINE TO lt_objects_aux ASSIGNING <fs_object_aux>.
      MOVE-CORRESPONDING ls_object TO <fs_object_aux>.
    ENDLOOP.
    SELECT pgmid object obj_name trkorr strkorr
      FROM /atrm/v_obj_lock
      INTO CORRESPONDING FIELDS OF TABLE lt_result_aux
      FOR ALL ENTRIES IN lt_objects_aux
      WHERE pgmid EQ lt_objects_aux-pgmid
        AND object EQ lt_objects_aux-object
        AND obj_name EQ lt_objects_aux-obj_name
        AND trstatus EQ 'D' OR trstatus EQ 'L'.
    LOOP AT lt_result_aux INTO ls_result_aux.
      UNASSIGN <fs_obj_lock>.
      READ TABLE obj_lock TRANSPORTING NO FIELDS WITH KEY pgmid = ls_result_aux-pgmid object = ls_result_aux-object obj_name = ls_result_aux-obj_name.
      CHECK sy-subrc <> 0.
      APPEND INITIAL LINE TO obj_lock ASSIGNING <fs_obj_lock>.
      <fs_obj_lock>-pgmid = ls_result_aux-pgmid.
      <fs_obj_lock>-object = ls_result_aux-object.
      <fs_obj_lock>-obj_name = ls_result_aux-obj_name.
      IF ls_result_aux-strkorr IS NOT INITIAL.
        <fs_obj_lock>-trkorr = ls_result_aux-strkorr.
      ELSE.
        <fs_obj_lock>-trkorr = ls_result_aux-trkorr.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS zcl_trm_object DEFINITION
  PUBLIC
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_trm_senvi_map.

  PUBLIC SECTION.
    INTERFACES zif_trm_object.

    TYPES: tyt_senvi TYPE STANDARD TABLE OF senvi.

    DATA: key   TYPE ztrm_object READ-ONLY,
          senvi TYPE tyt_senvi READ-ONLY.

    METHODS constructor
      IMPORTING key TYPE ztrm_object.

  PROTECTED SECTION.
    CLASS-METHODS get_tadir_dependency
      IMPORTING object            TYPE any
                obj_name          TYPE any
      RETURNING VALUE(dependency) TYPE ztrm_object_dependency
      RAISING   zcx_trm_exception.
    CLASS-METHODS get_tfdir_dependency
      IMPORTING funcname            TYPE any
      RETURNING VALUE(dependency) TYPE ztrm_object_dependency
      RAISING   zcx_trm_exception.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_object IMPLEMENTATION.

  METHOD constructor.
    me->key = key.
  ENDMETHOD.

  METHOD zif_trm_object~get_dependencies.
    DATA: lv_obj_type TYPE seu_obj,
          lv_obj_name TYPE sobj_name,
          ls_senvi    TYPE senvi.
    lv_obj_type = key-object.
    lv_obj_name = key-obj_name.
    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_ALL'
      EXPORTING
        obj_type        = lv_obj_type
        object_name     = lv_obj_name
        deep            = '1'
      TABLES
        environment_tab = senvi.

    LOOP AT senvi INTO ls_senvi.
      zcl_trm_senvi_map=>get(
        senvi = ls_senvi
        origin = me
      )->map_dependencies(
        CHANGING
          ct_dependencies = et_dependencies
      ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_tadir_dependency.
    DATA: lv_tabname                 TYPE tabname,
          lv_tabkey                  TYPE string,
          ls_dispatcher_dependencies TYPE ztrm_object_dependencies,
          lt_trm_packages            TYPE zcl_trm_core=>tyt_trm_package,
          lv_devclass                TYPE devclass,
          ls_trm_package             LIKE LINE OF lt_trm_packages.

    lv_tabname = 'TADIR'.
    CONCATENATE 'R3TR' object obj_name INTO lv_tabkey.

    LOOP AT zcl_trm_object_dispacher=>dependencies INTO ls_dispatcher_dependencies.
      READ TABLE ls_dispatcher_dependencies-dependencies INTO dependency WITH KEY tabname = lv_tabname tabkey = lv_tabkey.
      CHECK sy-subrc EQ 0.
      RETURN.
    ENDLOOP.

    lt_trm_packages = zcl_trm_singleton=>get( )->get_installed_packages( ).
    SELECT SINGLE devclass FROM tadir INTO lv_devclass WHERE pgmid = 'R3TR' AND object = object AND obj_name = obj_name.
    IF sy-subrc EQ 0.
      dependency-tabname = lv_tabname.
      dependency-tabkey = lv_tabkey.
      dependency-devclass = lv_devclass.
      CHECK lv_devclass IS NOT INITIAL.
      LOOP AT lt_trm_packages INTO ls_trm_package.
        READ TABLE ls_trm_package-tdevc TRANSPORTING NO FIELDS WITH KEY devclass = lv_devclass.
        CHECK sy-subrc EQ 0.
        dependency-trm_package_name = ls_trm_package-name.
        dependency-trm_package_registry = ls_trm_package-registry.
        EXIT.
      ENDLOOP.
    ELSE.
      zcx_trm_exception=>raise(
        iv_reason  = zcx_trm_exception=>c_reason-generic
        iv_message = 'R3TR ' && object && ' ' && obj_name && ' is not in TADIR'
      ).
    ENDIF.
  ENDMETHOD.

  METHOD get_tfdir_dependency.
    DATA: lv_tabname                 TYPE tabname,
          lv_tabkey                  TYPE string,
          ls_dispatcher_dependencies TYPE ztrm_object_dependencies,
          lt_trm_packages            TYPE zcl_trm_core=>tyt_trm_package,
          lv_devclass                TYPE devclass,
          ls_trm_package             LIKE LINE OF lt_trm_packages.

    lv_tabname = 'TFDIR'.
    lv_tabkey = funcname.

    LOOP AT zcl_trm_object_dispacher=>dependencies INTO ls_dispatcher_dependencies.
      READ TABLE ls_dispatcher_dependencies-dependencies INTO dependency WITH KEY tabname = lv_tabname tabkey = lv_tabkey.
      CHECK sy-subrc EQ 0.
      RETURN.
    ENDLOOP.

    lt_trm_packages = zcl_trm_singleton=>get( )->get_installed_packages( ).
    SELECT SINGLE tadir~devclass
      FROM tadir
      INNER JOIN v_fdir ON tadir~obj_name = v_fdir~area
      INTO lv_devclass WHERE tadir~pgmid = 'R3TR' AND tadir~object = 'FUGR' AND v_fdir~funcname = funcname.
    IF sy-subrc EQ 0.
      dependency-tabname = lv_tabname.
      dependency-tabkey = lv_tabkey.
      dependency-devclass = lv_devclass.
      CHECK lv_devclass IS NOT INITIAL.
      LOOP AT lt_trm_packages INTO ls_trm_package.
        READ TABLE ls_trm_package-tdevc TRANSPORTING NO FIELDS WITH KEY devclass = lv_devclass.
        CHECK sy-subrc EQ 0.
        dependency-trm_package_name = ls_trm_package-name.
        dependency-trm_package_registry = ls_trm_package-registry.
        EXIT.
      ENDLOOP.
    ELSE.
      zcx_trm_exception=>raise(
        iv_reason  = zcx_trm_exception=>c_reason-generic
        iv_message = 'Cannot find function group of ' && funcname
      ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

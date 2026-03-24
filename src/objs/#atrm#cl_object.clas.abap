CLASS /atrm/cl_object DEFINITION
  PUBLIC
  CREATE PUBLIC
  GLOBAL FRIENDS /atrm/cl_senvi_map.

  PUBLIC SECTION.
    INTERFACES /atrm/if_object.

    TYPES: tyt_senvi TYPE STANDARD TABLE OF senvi.

    DATA: key   TYPE /atrm/object READ-ONLY,
          senvi TYPE tyt_senvi READ-ONLY.

    METHODS constructor
      IMPORTING key TYPE /atrm/object.

  PROTECTED SECTION.
    CLASS-METHODS get_tadir_dependency
      IMPORTING object            TYPE any
                obj_name          TYPE any
      RETURNING VALUE(dependency) TYPE /atrm/object_dependency
      RAISING   /atrm/cx_exception.
    CLASS-METHODS get_tfdir_dependency
      IMPORTING funcname          TYPE any
      RETURNING VALUE(dependency) TYPE /atrm/object_dependency
      RAISING   /atrm/cx_exception.
  PRIVATE SECTION.
ENDCLASS.



CLASS /ATRM/CL_OBJECT IMPLEMENTATION.


  METHOD constructor.
    me->key = key.
  ENDMETHOD.


  METHOD get_tadir_dependency.
    DATA: lv_tabname                 TYPE tabname,
          lv_tabkey                  TYPE string,
          ls_dispatcher_dependencies TYPE /atrm/object_dependencies,
          lt_trm_packages            TYPE /atrm/cl_core=>tyt_trm_package,
          lv_devclass                TYPE devclass,
          ls_trm_package             LIKE LINE OF lt_trm_packages.

    lv_tabname = 'TADIR'.
    CONCATENATE 'R3TR' object obj_name INTO lv_tabkey.

    LOOP AT /atrm/cl_object_dispacher=>dependencies INTO ls_dispatcher_dependencies.
      READ TABLE ls_dispatcher_dependencies-dependencies INTO dependency WITH KEY tabname = lv_tabname tabkey = lv_tabkey.
      CHECK sy-subrc EQ 0.
      RETURN.
    ENDLOOP.

    lt_trm_packages = /atrm/cl_singleton=>get( )->get_installed_packages( ).
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
      /atrm/cx_exception=>raise(
        iv_reason  = /atrm/cx_exception=>c_reason-generic
        iv_message = 'R3TR ' && object && ' ' && obj_name && ' is not in TADIR'
      ).
    ENDIF.
  ENDMETHOD.


  METHOD get_tfdir_dependency.
    DATA: lv_tabname                 TYPE tabname,
          lv_tabkey                  TYPE string,
          ls_dispatcher_dependencies TYPE /atrm/object_dependencies,
          lt_trm_packages            TYPE /atrm/cl_core=>tyt_trm_package,
          lv_devclass                TYPE devclass,
          ls_trm_package             LIKE LINE OF lt_trm_packages.

    lv_tabname = 'TFDIR'.
    lv_tabkey = funcname.

    LOOP AT /atrm/cl_object_dispacher=>dependencies INTO ls_dispatcher_dependencies.
      READ TABLE ls_dispatcher_dependencies-dependencies INTO dependency WITH KEY tabname = lv_tabname tabkey = lv_tabkey.
      CHECK sy-subrc EQ 0.
      RETURN.
    ENDLOOP.

    lt_trm_packages = /atrm/cl_singleton=>get( )->get_installed_packages( ).
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
      /atrm/cx_exception=>raise(
        iv_reason  = /atrm/cx_exception=>c_reason-generic
        iv_message = 'Cannot find function group of ' && funcname
      ).
    ENDIF.
  ENDMETHOD.


  METHOD /atrm/if_object~get_dependencies.
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
      /atrm/cl_senvi_map=>get(
        senvi = ls_senvi
        origin = me
      )->map_dependencies(
        CHANGING
          dependencies = dependencies
      ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

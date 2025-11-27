CLASS zcl_trm_object DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_trm_object.

    TYPES: tyt_senvi TYPE STANDARD TABLE OF senvi.

    DATA: key   TYPE ztrm_object READ-ONLY,
          senvi TYPE tyt_senvi READ-ONLY.

    METHODS constructor
      IMPORTING key TYPE ztrm_object.

  PROTECTED SECTION.
    METHODS get_tadir_dependency
      IMPORTING pgmid             TYPE pgmid
                object            TYPE trobjtype
                obj_name          TYPE any
      RETURNING VALUE(dependency) TYPE ztrm_object_dependency
      RAISING
                zcx_trm_exception.
  PRIVATE SECTION.

    METHODS parse_senvi
      IMPORTING is_senvi        TYPE senvi
      EXPORTING et_dependencies TYPE ztrm_object_dependency_t.
ENDCLASS.



CLASS zcl_trm_object IMPLEMENTATION.

  METHOD constructor.
    me->key = key.
  ENDMETHOD.

  METHOD zif_trm_object~get_dependencies.
    DATA: lv_obj_type         TYPE seu_obj,
          lv_obj_name         TYPE sobj_name,
          ls_senvi            TYPE senvi,
          lt_dependencies_tmp TYPE ztrm_object_dependency_t,
          ls_dependencies_tmp LIKE LINE OF lt_dependencies_tmp.
    lv_obj_type = key-object.
    lv_obj_name = key-obj_name.
    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_ALL'
      EXPORTING
        obj_type        = lv_obj_type
*       environment_types =
        object_name     = lv_obj_name
*       parallel_task   =
        deep            = '1'
*       with_memory     = ' '
*       with_parameters = ' '
*       aggregate_level = '1'
*       compiler_grade  = '2'
*       cds_view_elements = ' '
      TABLES
        environment_tab = senvi
*       source_objects  =
      .
    LOOP AT senvi INTO ls_senvi.
      CLEAR lt_dependencies_tmp[].
      CLEAR ls_dependencies_tmp.
      parse_senvi(
        EXPORTING
          is_senvi = ls_senvi
        IMPORTING
          et_dependencies = lt_dependencies_tmp
      ).
      LOOP AT lt_dependencies_tmp INTO ls_dependencies_tmp.
        READ TABLE et_dependencies TRANSPORTING NO FIELDS WITH KEY tabname = ls_dependencies_tmp-tabname tabkey = ls_dependencies_tmp-tabkey.
        CHECK sy-subrc <> 0.
        APPEND ls_dependencies_tmp TO et_dependencies.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_senvi.
    DATA: lt_tadir_objects  TYPE zcl_trm_utility=>tyt_ko100,
          ls_tadir_object   LIKE LINE OF lt_tadir_objects,
          lv_tadir_object   TYPE trobjtype,
          lv_tadir_obj_name TYPE sobj_name,
          ls_dependency     TYPE ztrm_object_dependency.
    FIELD-SYMBOLS <fs_dep> TYPE ztrm_object_dependency.
    TRY.
        zcl_trm_singleton=>get( )->get_supported_object_types(
          IMPORTING
            et_object_text = lt_tadir_objects
        ).
        lv_tadir_object = is_senvi-type.
        lv_tadir_obj_name = is_senvi-object.
        READ TABLE lt_tadir_objects INTO ls_tadir_object WITH KEY pgmid = 'R3TR' object = lv_tadir_object.
        IF sy-subrc EQ 0.
          APPEND get_tadir_dependency(
            pgmid    = 'R3TR'
            object   = lv_tadir_object
            obj_name = lv_tadir_obj_name
          ) TO et_dependencies.
          RETURN.
        ENDIF.
        " if we get here, type is a non-tadir type and should be parsed
        CASE is_senvi-type.
          WHEN 'INCL'.
            " used in object specific implementation
          WHEN 'OM'.
            APPEND get_tadir_dependency(
              pgmid    = 'R3TR'
              object   = 'CLAS'
              obj_name = is_senvi-encl_obj
            ) TO et_dependencies.
          WHEN 'FUNC'.
            ls_dependency = get_tadir_dependency(
              pgmid    = 'R3TR'
              object   = 'FUGR'
              obj_name = is_senvi-encl_obj
            ).
            APPEND ls_dependency TO et_dependencies.
            APPEND ls_dependency TO et_dependencies ASSIGNING <fs_dep>.
            <fs_dep>-tabname = 'TFDIR'.
            <fs_dep>-tabkey = is_senvi-object.
          WHEN 'DGT'.
            APPEND get_tadir_dependency(
              pgmid    = 'R3TR'
              object   = 'TYPE'
              obj_name = is_senvi-encl_obj
            ) TO et_dependencies.
          WHEN 'STRU'.
            APPEND get_tadir_dependency(
              pgmid    = 'R3TR'
              object   = 'TABL'
              obj_name = is_senvi-object
            ) TO et_dependencies.
        ENDCASE.
      CATCH zcx_trm_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD get_tadir_dependency.
    DATA: lv_message      TYPE string,
          lv_devclass     TYPE devclass,
          lt_trm_packages TYPE zcl_trm_core=>tyt_trm_package,
          ls_trm_package  LIKE LINE OF lt_trm_packages.
    lt_trm_packages = zcl_trm_singleton=>get( )->get_installed_packages( ).
    SELECT SINGLE devclass FROM tadir INTO lv_devclass WHERE pgmid = pgmid AND object = object AND obj_name = obj_name.
    IF lv_devclass IS NOT INITIAL.
      dependency-tabname = 'TADIR'.
      dependency-devclass = lv_devclass.
      CONCATENATE pgmid object obj_name INTO dependency-tabkey.
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
        iv_message = pgmid && ' ' && object && ' ' && obj_name && ' is not in TADIR'
      ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

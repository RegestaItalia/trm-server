CLASS zcl_trm_object DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_trm_object_dispacher.

  PUBLIC SECTION.
    INTERFACES zif_trm_object.

    METHODS constructor
      IMPORTING key TYPE ztrm_object.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: tyt_senvi TYPE STANDARD TABLE OF senvi.
    DATA: key TYPE ztrm_object.

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
          lt_senvi            TYPE tyt_senvi,
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
        environment_tab = lt_senvi
*       source_objects  =
      .
    LOOP AT lt_senvi INTO ls_senvi.
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
          lv_tadir_obj_name TYPE sobj_name.
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
          APPEND INITIAL LINE TO et_dependencies ASSIGNING <fs_dep>.
          <fs_dep>-tabname = 'TADIR'.
          CONCATENATE 'R3TR' lv_tadir_object lv_tadir_obj_name INTO <fs_dep>-tabkey.
          RETURN.
        ENDIF.
      CATCH zcx_trm_exception.
    ENDTRY.
    CASE is_senvi-type.
      WHEN 'OM'.
        APPEND INITIAL LINE TO et_dependencies ASSIGNING <fs_dep>.
        <fs_dep>-tabname = 'TADIR'.
        CONCATENATE 'R3TR' 'CLAS' is_senvi-encl_obj INTO <fs_dep>-tabkey.
      WHEN 'FUNC'.
        APPEND INITIAL LINE TO et_dependencies ASSIGNING <fs_dep>.
        <fs_dep>-tabname = 'TFDIR'.
        <fs_dep>-tabkey = is_senvi-object.
        APPEND INITIAL LINE TO et_dependencies ASSIGNING <fs_dep>.
        <fs_dep>-tabname = 'TADIR'.
        CONCATENATE 'R3TR' 'FUGR' is_senvi-encl_obj INTO <fs_dep>-tabkey.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

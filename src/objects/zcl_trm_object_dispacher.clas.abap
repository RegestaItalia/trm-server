CLASS zcl_trm_object_dispacher DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_trm_object.

  PUBLIC SECTION.

    CLASS-METHODS get_package_dependencies
      IMPORTING package             TYPE devclass
                incl_sub            TYPE flag
                log_id              TYPE ztrm_polling_id OPTIONAL
      RETURNING VALUE(dependencies) TYPE ztrm_object_dependencies_t
      RAISING   zcx_trm_exception.

    CLASS-METHODS get
      IMPORTING key                TYPE ztrm_object
      RETURNING VALUE(ro_instance) TYPE REF TO zif_trm_object.

  PROTECTED SECTION.
    CLASS-DATA: dependencies TYPE ztrm_object_dependencies_t,
                log          TYPE REF TO zcl_trm_log_polling.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_object_dispacher IMPLEMENTATION.

  METHOD get_package_dependencies.
    DATA: lo_package    TYPE REF TO zcl_trm_package,
          lt_objects    TYPE scts_tadir,
          ls_object     LIKE LINE OF lt_objects,
          ls_key        TYPE ztrm_object,
          lo_object     TYPE REF TO zif_trm_object,
          lv_total_objs TYPE i,
          lv_percentage TYPE i.
    FIELD-SYMBOLS <fs_object_deps> TYPE ztrm_object_dependencies.

    CREATE OBJECT log EXPORTING id = log_id.

    CREATE OBJECT lo_package EXPORTING iv_devclass = package.
    log->update_message( 'Reading package ' && package && ' objects' ).
    lo_package->get_objects(
      EXPORTING
        iv_incl_sub = incl_sub
      IMPORTING
        et_tadir    = lt_objects
    ).
    DESCRIBE TABLE lt_objects LINES lv_total_objs.
    CLEAR lv_percentage.
    CLEAR zcl_trm_object_dispacher=>dependencies[].
    LOOP AT lt_objects INTO ls_object.
      log->update_message( 'Reading dependencies ' && lv_percentage && '%' ).
      CLEAR ls_key.
      CLEAR lo_object.
      UNASSIGN <fs_object_deps>.
      MOVE-CORRESPONDING ls_object TO ls_key.
      APPEND INITIAL LINE TO zcl_trm_object_dispacher=>dependencies ASSIGNING <fs_object_deps>.
      MOVE-CORRESPONDING ls_key TO <fs_object_deps>.
      lo_object = get(
        EXPORTING
          key = ls_key
      ).
      lo_object->get_dependencies(
        IMPORTING
          et_dependencies = <fs_object_deps>-dependencies
      ).
      SORT <fs_object_deps>-dependencies BY tabname tabkey.
      DELETE ADJACENT DUPLICATES FROM <fs_object_deps>-dependencies COMPARING tabname tabkey.
      lv_percentage = ( ( sy-tabix + 1 ) / lv_total_objs ) * 100.
    ENDLOOP.
    log->update_message( 'Reading dependencies 100%' ).
    dependencies = zcl_trm_object_dispacher=>dependencies.
  ENDMETHOD.

  METHOD get.
    DATA: lv_class_name TYPE string,
          lo_badi       TYPE REF TO ztrm_badi_objects_handler.
    CONCATENATE 'ZCL_TRM_OBJECT_' key-object INTO lv_class_name.
    TRANSLATE lv_class_name TO UPPER CASE.
    TRY.
        CREATE OBJECT ro_instance TYPE (lv_class_name)
          EXPORTING key = key.
      CATCH cx_dynamic_check.
        CREATE OBJECT ro_instance TYPE zcl_trm_object
          EXPORTING
            key = key.
    ENDTRY.
    TRY.
        GET BADI lo_badi.
        CALL BADI lo_badi->change_object_handler
          EXPORTING
            key     = key
          CHANGING
            handler = ro_instance.
      CATCH cx_badi_not_implemented cx_badi_initial_reference.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

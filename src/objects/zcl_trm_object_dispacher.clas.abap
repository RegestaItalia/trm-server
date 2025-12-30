CLASS zcl_trm_object_dispacher DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    METHODS constructor
      IMPORTING devclass TYPE devclass OPTIONAL.

    METHODS get_objects_dependencies
      IMPORTING incl_sub            TYPE flag
      RETURNING VALUE(dependencies) TYPE ztrm_object_dependencies_t
      RAISING   zcx_trm_exception.

    CLASS-METHODS get
      IMPORTING key                TYPE ztrm_object
      RETURNING VALUE(ro_instance) TYPE REF TO zif_trm_object.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: go_package  TYPE REF TO zcl_trm_package,
          gv_incl_sub TYPE flag.
ENDCLASS.



CLASS zcl_trm_object_dispacher IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT go_package EXPORTING iv_devclass = devclass.
  ENDMETHOD.

  METHOD get_objects_dependencies.
    DATA: lt_objects TYPE scts_tadir,
          ls_object  LIKE LINE OF lt_objects,
          ls_key     TYPE ztrm_object,
          lo_object  TYPE REF TO zif_trm_object.
    FIELD-SYMBOLS <fs_object_deps> TYPE ztrm_object_dependencies.
    go_package->get_objects(
      EXPORTING
        iv_incl_sub = incl_sub
      IMPORTING
        et_tadir    = lt_objects
    ).
    LOOP AT lt_objects INTO ls_object.
      CLEAR ls_key.
      CLEAR lo_object.
      UNASSIGN <fs_object_deps>.
      MOVE-CORRESPONDING ls_object TO ls_key.
      APPEND INITIAL LINE TO dependencies ASSIGNING <fs_object_deps>.
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
    ENDLOOP.
  ENDMETHOD.

  METHOD get.
    DATA lv_class_name TYPE string.
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
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.
    zcl_trm_object_dispacher=>get(
      EXPORTING
        key         = VALUE #( object = 'CLAS' obj_name = 'CB_DEVIATION_SHOW_COMMAND_POC' )
    )->get_dependencies( ).
*    SELECT SINGLE * FROM tadir INTO @DATA(ls_tadir) WHERE object EQ 'DEVC' AND obj_name EQ 'ZTEST'.
*    CHECK sy-subrc EQ 0.
*    zcl_trm_object_dispacher=>get( CORRESPONDING #( ls_tadir ) )->get_dependencies(
*      IMPORTING
*        et_dependencies = DATA(lt_deps)
*    ).
*    LOOP AT lt_deps INTO DATA(ls_dep).
*      IF ls_dep-tabname EQ 'TADIR'.
*        out->write( |TADIR: { ls_dep-tabkey(4) } { ls_dep-tabkey+4(4) } { ls_dep-tabkey+8 }| ).
*      ELSEIF ls_dep-tabname EQ 'TFDIR'.
*        out->write( |TFDIR: { ls_dep-tabkey }| ).
*      ELSE.
*        out->write( |{ ls_dep-tabname }: { ls_dep-tabkey }| ).
*      ENDIF.
*    ENDLOOP.
*    CHECK 1 EQ 1.
  ENDMETHOD.

ENDCLASS.

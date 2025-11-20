CLASS zcl_trm_object_dispacher DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CLASS-METHODS get
      IMPORTING key                TYPE ztrm_object
      RETURNING VALUE(ro_instance) TYPE REF TO zif_trm_object.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_object_dispacher IMPLEMENTATION.

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
     SELECT SINGLE * FROM tadir INTO @DATA(ls_tadir) WHERE object EQ 'DDLX' AND obj_name EQ 'YCB_PROTES'.
*    SELECT SINGLE * FROM tadir INTO @DATA(ls_tadir) WHERE object EQ 'CLAS' AND obj_name EQ 'ZCL_RD_REST_HANDLER'.
    zcl_trm_object_dispacher=>get( CORRESPONDING #( ls_tadir ) )->get_dependencies(
      IMPORTING
        et_dependencies = DATA(lt_deps)
    ).
    LOOP AT lt_deps INTO DATA(ls_dep).
      IF ls_dep-tabname EQ 'TADIR'.
        out->write( |TADIR: { ls_dep-tabkey(4) } { ls_dep-tabkey+4(4) } { ls_dep-tabkey+8 }| ).
      ELSEIF ls_dep-tabname EQ 'TFDIR'.
        out->write( |TFDIR: { ls_dep-tabkey }| ).
      ELSE.
        out->write( |{ ls_dep-tabname }: { ls_dep-tabkey }| ).
      ENDIF.
    ENDLOOP.
    CHECK 1 EQ 1.
  ENDMETHOD.

ENDCLASS.

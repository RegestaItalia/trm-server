CLASS zcl_trm_singleton DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get
      RETURNING VALUE(ro_singleton) TYPE REF TO zcl_trm_singleton.

    METHODS get_installed_packages
      RETURNING VALUE(rt_packages) TYPE zcl_trm_core=>tyt_trm_package.

    METHODS get_supported_object_types
      EXPORTING et_object_text TYPE zcl_trm_utility=>tyt_ko100
      RAISING   zcx_trm_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: go_singleton TYPE REF TO zcl_trm_singleton.
    DATA: gt_packages    TYPE zcl_trm_core=>tyt_trm_package,
          gt_object_text TYPE zcl_trm_utility=>tyt_ko100.

ENDCLASS.



CLASS zcl_trm_singleton IMPLEMENTATION.

  METHOD get.
    IF go_singleton IS NOT BOUND.
      CREATE OBJECT go_singleton.
    ENDIF.
    ro_singleton = go_singleton.
  ENDMETHOD.

  METHOD get_installed_packages.
    IF gt_packages[] IS INITIAL.
      gt_packages = zcl_trm_core=>get_installed_packages( ).
    ENDIF.
    rt_packages[] = gt_packages[].
  ENDMETHOD.

  METHOD get_supported_object_types.
    IF gt_object_text[] IS INITIAL.
      zcl_trm_utility=>get_supported_object_types(
        IMPORTING et_object_text = gt_object_text
      ).
    ENDIF.
    et_object_text[] = gt_object_text[].
  ENDMETHOD.

ENDCLASS.

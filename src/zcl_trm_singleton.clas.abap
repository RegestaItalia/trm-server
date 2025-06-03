CLASS zcl_trm_singleton DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get
      RETURNING VALUE(ro_singleton) TYPE REF TO zcl_trm_singleton.

    METHODS get_installed_packages
      RETURNING VALUE(rt_packages) TYPE zcl_trm_core=>tyt_trm_package.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA: go_singleton TYPE REF TO zcl_trm_singleton.
    DATA: gt_packages TYPE zcl_trm_core=>tyt_trm_package.

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

ENDCLASS.

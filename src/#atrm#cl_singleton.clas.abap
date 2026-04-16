CLASS /atrm/cl_singleton DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get
      RETURNING VALUE(singleton) TYPE REF TO /atrm/cl_singleton.

    METHODS get_installed_packages
      RETURNING VALUE(packages) TYPE /atrm/packages_t.

    METHODS get_supported_object_types
      EXPORTING object_text TYPE /atrm/cl_utilities=>tyt_ko100
      RAISING   /atrm/cx_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: go_singleton TYPE REF TO /atrm/cl_singleton.
    DATA: gt_packages    TYPE /atrm/packages_t,
          gt_object_text TYPE /atrm/cl_utilities=>tyt_ko100.

ENDCLASS.



CLASS /atrm/cl_singleton IMPLEMENTATION.

  METHOD get.
    IF go_singleton IS NOT BOUND.
      CREATE OBJECT go_singleton.
    ENDIF.
    singleton = go_singleton.
  ENDMETHOD.

  METHOD get_installed_packages.
    IF gt_packages[] IS INITIAL.
      gt_packages = /atrm/cl_core=>get_installed_packages( ).
    ENDIF.
    packages[] = gt_packages[].
  ENDMETHOD.

  METHOD get_supported_object_types.
    IF gt_object_text[] IS INITIAL.
      /atrm/cl_utilities=>get_supported_object_types(
        IMPORTING object_text = gt_object_text
      ).
    ENDIF.
    object_text[] = gt_object_text[].
  ENDMETHOD.

ENDCLASS.

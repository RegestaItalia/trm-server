CLASS zcl_trm_abapgit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_dot_abapgit
      IMPORTING iv_devclass           TYPE devclass
      RETURNING VALUE(rv_dot_abapgit) TYPE xstring
      RAISING   zcx_trm_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS handle_root_exception
      IMPORTING io_exception TYPE REF TO cx_root
      RAISING   zcx_trm_exception.

ENDCLASS.



CLASS zcl_trm_abapgit IMPLEMENTATION.

  METHOD handle_root_exception.
    DATA: lo_exception         TYPE REF TO cx_root.

    lo_exception = io_exception.
    WHILE lo_exception->previous IS BOUND.
      lo_exception = lo_exception->previous.
    ENDWHILE.
    IF io_exception IS BOUND.
      zcx_trm_exception=>raise(
        EXPORTING
          iv_reason  = zcx_trm_exception=>c_reason-abapgit_intergration
          iv_message = lo_exception->get_text( )
      ).
    ELSE.
      zcx_trm_exception=>raise(
        EXPORTING
          iv_reason  = zcx_trm_exception=>c_reason-abapgit_intergration
          iv_message = 'Unknown exception raised'
      ).
    ENDIF.
  ENDMETHOD.

  METHOD get_dot_abapgit.
    TRY.
        DATA lo_repo TYPE REF TO lcl_abapgit_repo.
        lcl_abapgit_repo_srv=>get_instance( )->get_repo_from_package(
          EXPORTING
            iv_package = iv_devclass
          IMPORTING
            eo_repo    = lo_repo
        ).
        rv_dot_abapgit = lo_repo->get_dot_abapgit( )->serialize( ).
      CATCH cx_root INTO DATA(lx_error).
        handle_root_exception( lx_error ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

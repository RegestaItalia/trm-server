CLASS zcl_trm_package DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING iv_devclass TYPE devclass.

    CLASS-METHODS create
      IMPORTING is_data TYPE scompkdtln
      RETURNING VALUE(ro_package) TYPE REF TO zcl_trm_package
      RAISING   zcx_trm_exception.

    METHODS get_objects
      EXPORTING et_tadir    TYPE scts_tadir
      RAISING   zcx_trm_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: gv_devclass TYPE devclass.
ENDCLASS.



CLASS zcl_trm_package IMPLEMENTATION.

  METHOD constructor.
    gv_devclass = iv_devclass.
  ENDMETHOD.

  METHOD create.
    DATA ls_data LIKE is_data.
    DATA lo_package TYPE REF TO if_package.
    MOVE is_data TO ls_data.

    TRY.
        CALL METHOD ('CL_PACKAGE_FACTORY')=>('CREATE_NEW_PACKAGE')
          EXPORTING
            i_reuse_deleted_object = 'X'
            i_suppress_dialog      = 'X'
            i_boh                  = 'X'
          IMPORTING
            e_package              = lo_package
          CHANGING
            c_package_data         = ls_data
          EXCEPTIONS
            OTHERS                 = 1.
      CATCH cx_sy_dyn_call_param_not_found.
        zcx_trm_exception=>raise( iv_reason  = zcx_trm_exception=>c_reason-dyn_call_param_not_found ).
    ENDTRY.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.

    TRY.
        CALL METHOD lo_package->('SAVE')
          EXPORTING
            i_suppress_dialog      = 'X'
            i_suppress_corr_insert = 'X'
          EXCEPTIONS
            OTHERS                 = 1.
      CATCH cx_sy_dyn_call_param_not_found.
        zcx_trm_exception=>raise( iv_reason  = zcx_trm_exception=>c_reason-dyn_call_param_not_found ).
    ENDTRY.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.

    TRY.
        CALL METHOD lo_package->('SET_CHANGEABLE')
          EXPORTING
            i_changeable      = ' '
            i_suppress_dialog = 'X'
          EXCEPTIONS
            OTHERS            = 1.
      CATCH cx_sy_dyn_call_param_not_found.
        zcx_trm_exception=>raise( iv_reason  = zcx_trm_exception=>c_reason-dyn_call_param_not_found ).
    ENDTRY.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.

    CREATE OBJECT ro_package EXPORTING iv_devclass = ls_data-devclass.
  ENDMETHOD.

  METHOD get_objects.
    CALL FUNCTION 'TRINT_SELECT_OBJECTS'
      EXPORTING
        iv_devclass      = gv_devclass
        iv_via_selscreen = ' '
      IMPORTING
        et_objects_tadir = et_tadir
      EXCEPTIONS
        OTHERS           = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

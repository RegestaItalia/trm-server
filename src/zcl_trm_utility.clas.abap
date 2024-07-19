CLASS zcl_trm_utility DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: tyt_ko100       TYPE STANDARD TABLE OF ko100 WITH DEFAULT KEY,
           tyt_installdevc TYPE STANDARD TABLE OF ztrm_installdevc WITH DEFAULT KEY.

    CLASS-METHODS check_functions_authorization
      RETURNING VALUE(rv_authorized) TYPE flag.

    CLASS-METHODS add_skip_trkorr
      IMPORTING iv_trkorr TYPE trkorr
      RAISING   zcx_trm_exception.

    CLASS-METHODS add_source_trkorr
      IMPORTING iv_trkorr TYPE trkorr
      RAISING   zcx_trm_exception.

    CLASS-METHODS get_binary_file
      IMPORTING iv_file_path TYPE string
      EXPORTING ev_file      TYPE xstring
      RAISING   zcx_trm_exception.

    CLASS-METHODS write_binary_file
      IMPORTING iv_file_path TYPE string
                iv_file      TYPE xstring
      RAISING   zcx_trm_exception.

    CLASS-METHODS get_dir_trans
      EXPORTING ev_dir_trans TYPE pfevalue
      RAISING   zcx_trm_exception.

    CLASS-METHODS get_file_sys
      EXPORTING ev_file_sys TYPE filesys
      RAISING   zcx_trm_exception.

    CLASS-METHODS get_default_transport_layer
      EXPORTING ev_layer TYPE devlayer
      RAISING   zcx_trm_exception.

    CLASS-METHODS get_supported_object_types
      EXPORTING et_object_text TYPE tyt_ko100
      RAISING   zcx_trm_exception.

    CLASS-METHODS add_install_devclass
      IMPORTING it_installdevc TYPE tyt_installdevc
      RAISING   zcx_trm_exception.

    CLASS-METHODS add_package_integrity
      IMPORTING is_integrity TYPE ztrm_integrity
      RAISING   zcx_trm_exception.

    CLASS-METHODS tadir_interface
      IMPORTING iv_pgmid     TYPE pgmid
                iv_object    TYPE trobjtype
                iv_objname   TYPE sobj_name
                iv_devclass  TYPE devclass OPTIONAL
                iv_srcsystem TYPE srcsystem OPTIONAL
                iv_author    TYPE responsibl OPTIONAL
                iv_genflag   TYPE genflag OPTIONAL
      RAISING   zcx_trm_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS enqueue
      IMPORTING iv_mode_rstable TYPE enqmode DEFAULT 'E'
                iv_tabname      TYPE tabname
      RAISING   zcx_trm_exception.
    CLASS-METHODS dequeue
      IMPORTING iv_mode_rstable TYPE enqmode DEFAULT 'E'
                iv_tabname      TYPE tabname
      RAISING   zcx_trm_exception.
ENDCLASS.



CLASS zcl_trm_utility IMPLEMENTATION.

  METHOD add_skip_trkorr.
    DATA ls_dummy TYPE ztrm_skip_trkorr.
    ls_dummy-trkorr = iv_trkorr.
    enqueue( iv_tabname = 'ZTRM_SKIP_TRKORR' ).
    INSERT ztrm_skip_trkorr FROM ls_dummy.
    COMMIT WORK AND WAIT.
    dequeue( iv_tabname = 'ZTRM_SKIP_TRKORR' ).
  ENDMETHOD.

  METHOD add_source_trkorr.
    DATA ls_dummy TYPE ztrm_src_trkorr.
    ls_dummy-trkorr = iv_trkorr.
    enqueue( iv_tabname = 'ZTRM_SRC_TRKORR' ).
    INSERT ztrm_src_trkorr FROM ls_dummy.
    COMMIT WORK AND WAIT.
    dequeue( iv_tabname = 'ZTRM_SRC_TRKORR' ).
  ENDMETHOD.

  METHOD check_functions_authorization.
    CLEAR rv_authorized.
    SELECT COUNT( * ) FROM ztrm_users WHERE uname = sy-uname.
    IF sy-dbcnt EQ 1.
      rv_authorized = 'X'.
    ENDIF.
  ENDMETHOD.

  METHOD enqueue.
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        mode_rstable   = iv_mode_rstable
        tabname        = iv_tabname
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( iv_reason  = zcx_trm_exception=>c_reason-enqueue_error ).
    ENDIF.
  ENDMETHOD.

  METHOD dequeue.
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable   = iv_mode_rstable
        tabname        = iv_tabname
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( iv_reason  = zcx_trm_exception=>c_reason-dequeue_error ).
    ENDIF.
  ENDMETHOD.

  METHOD get_binary_file.
    OPEN DATASET iv_file_path FOR INPUT IN BINARY MODE.
    READ DATASET iv_file_path INTO ev_file.
    CLOSE DATASET iv_file_path.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD write_binary_file.
    OPEN DATASET iv_file_path FOR OUTPUT IN BINARY MODE.
    TRANSFER iv_file TO iv_file_path.
    CLOSE DATASET iv_file_path.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_dir_trans.
    DATA lv_param_name TYPE pfeparname.
    lv_param_name = 'DIR_TRANS'.
    CALL FUNCTION 'SXPG_PROFILE_PARAMETER_GET'
      EXPORTING
        parameter_name  = lv_param_name
      IMPORTING
        parameter_value = ev_dir_trans
      EXCEPTIONS
        OTHERS          = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_file_sys.
    SELECT SINGLE filesys INTO ev_file_sys FROM opsystem WHERE opsys = sy-opsys.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( iv_message = 'File system not found'
                                iv_reason  = zcx_trm_exception=>c_reason-not_found ).
    ENDIF.
  ENDMETHOD.

  METHOD get_default_transport_layer.
    CALL FUNCTION 'TR_GET_TRANSPORT_TARGET'
      EXPORTING
        iv_use_default    = 'X'
        iv_get_layer_only = 'X'
      IMPORTING
        ev_layer          = ev_layer
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_supported_object_types.
    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = et_object_text
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD add_install_devclass.
    DATA ls_installdevc LIKE LINE OF it_installdevc.
    enqueue( iv_tabname = 'ZTRM_INSTALLDEVC' ).
    LOOP AT it_installdevc INTO ls_installdevc.
      MODIFY ztrm_installdevc FROM ls_installdevc.
    ENDLOOP.
    COMMIT WORK AND WAIT.
    dequeue( iv_tabname = 'ZTRM_INSTALLDEVC' ).
  ENDMETHOD.

  METHOD add_package_integrity.
    enqueue( iv_tabname = 'ZTRM_INTEGRITY' ).
    MODIFY ztrm_integrity FROM is_integrity.
    COMMIT WORK AND WAIT.
    dequeue( iv_tabname = 'ZTRM_INTEGRITY' ).
  ENDMETHOD.

  METHOD tadir_interface.
    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus      = ' '
        wi_tadir_pgmid     = iv_pgmid
        wi_tadir_object    = iv_object
        wi_tadir_obj_name  = iv_objname
        wi_tadir_devclass  = iv_devclass
        wi_tadir_srcsystem = iv_srcsystem
        wi_tadir_author    = iv_author
        wi_set_genflag     = iv_genflag
*       iv_no_pak_check    = 'X'
      EXCEPTIONS
        OTHERS             = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

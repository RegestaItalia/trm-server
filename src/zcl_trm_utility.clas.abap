CLASS zcl_trm_utility DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS check_functions_authorization
      RETURNING VALUE(rv_authorized) TYPE flag.

    CLASS-METHODS add_skip_trkorr
      IMPORTING iv_trkorr TYPE trkorr
      RAISING   zcx_trm_exception.

    CLASS-METHODS add_source_trkorr
      IMPORTING iv_trkorr TYPE trkorr
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

ENDCLASS.

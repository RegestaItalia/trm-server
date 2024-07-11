CLASS zcl_trm_transport DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tyt_lxe_packg TYPE STANDARD TABLE OF lxe_tt_packg_line WITH DEFAULT KEY,
           tyt_e071      TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY.

    CLASS-METHODS add_translations
      IMPORTING iv_trkorr   TYPE trkorr
                it_devclass TYPE tyt_lxe_packg
      RAISING   zcx_trm_exception.

    CLASS-METHODS add_objects
      IMPORTING iv_lock   TYPE flag
                iv_trkorr TYPE trkorr
                it_e071   TYPE tyt_e071
      EXPORTING et_log    TYPE sprot_u_tab
      RAISING   zcx_trm_exception.

    CLASS-METHODS create_workbench
      IMPORTING iv_text   TYPE as4text
                iv_target TYPE tr_target
      EXPORTING ev_trkorr TYPE trkorr
      RAISING   zcx_trm_exception.

    CLASS-METHODS create_transport_of_copies
      IMPORTING iv_text   TYPE as4text
                iv_target TYPE tr_target
      EXPORTING ev_trkorr TYPE trkorr
      RAISING   zcx_trm_exception.

    CLASS-METHODS delete
      IMPORTING iv_trkorr TYPE trkorr
      RAISING   zcx_trm_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS create
      IMPORTING iv_text   TYPE as4text
                iv_target TYPE tr_target
                iv_type   TYPE trfunction
      EXPORTING ev_trkorr TYPE trkorr
      RAISING   zcx_trm_exception.
ENDCLASS.



CLASS zcl_trm_transport IMPLEMENTATION.

  METHOD add_translations.
    DATA: lt_langs   TYPE TABLE OF lxeisolang,
          wa_langs   LIKE LINE OF lt_langs,
          lo_explang TYPE REF TO cl_lxe_log_export,
          lt_date    TYPE lxe_tt_date,
          lv_ret_cnt TYPE i,
          lt_objct   TYPE lxe_tt_objct,
          lt_user    TYPE lxe_tt_user,
          lt_comp    TYPE lxe_tt_comp.

    IF it_devclass[] IS INITIAL.
      zcx_trm_exception=>raise( iv_message = 'No input packages defined'
                                iv_reason  = zcx_trm_exception=>c_reason-invalid_input ).
    ENDIF.

    SELECT DISTINCT targlng FROM lxe_log INTO TABLE lt_langs WHERE custmnr EQ cl_lxe_constants=>c_trl_area_local.

    LOOP AT lt_langs INTO wa_langs.
      CALL FUNCTION 'LXE_T002_CHECK_LANGUAGE'
        EXPORTING
          language           = wa_langs
        EXCEPTIONS
          language_not_in_cp = 1
          unknown            = 2
          OTHERS             = 3.

      IF sy-subrc <> 0.
        zcx_trm_exception=>raise( ).
      ENDIF.

      CLEAR lo_explang.

      CREATE OBJECT lo_explang TYPE cl_lxe_log_export_local
        EXPORTING
          custmnr = cl_lxe_constants=>c_trl_area_local
          isolang = wa_langs.

      DATA ls_date LIKE LINE OF lt_date.
      ls_date-sign = 'I'.
      ls_date-option = 'LE'.
      ls_date-low = sy-datum.
      APPEND ls_date TO lt_date.

      CLEAR lv_ret_cnt.
      lv_ret_cnt = lo_explang->log_select( st_objct = lt_objct[]
                                           st_user  = lt_user[]
                                           st_date  = lt_date[] ).
      CHECK lv_ret_cnt NE 0.

      CLEAR lv_ret_cnt.
      lv_ret_cnt = lo_explang->apply_filters( st_packg  = it_devclass[]
                                              st_comp   = lt_comp[]
                                              piecelist = ' '
                                              check_ex  = 'X'
                                              chckorlg  = ' '
                                              clientdp  = ' ' ).

      CHECK lv_ret_cnt NE 0.

      CALL METHOD lo_explang->export_objects
        EXPORTING
          tr_auto                      = ''
          tr_request                   = iv_trkorr
          force                        = ''
          forcekey                     = ''
          object_transport             = ''
          tr_system                    = ''
          tr_project                   = ''
          tr_mixed                     = 'X'
          t_transport                  = ''
          tr_text                      = ''
          tr_release                   = ''
          rfc_destination              = ''
        EXCEPTIONS
          create_request_failed        = 1
          write_to_request_failed      = 2
          release_request_failed       = 3
          invalid_request              = 4
          no_correct_objects_available = 5
          OTHERS                       = 6.

      IF sy-subrc <> 0.
        zcx_trm_exception=>raise( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_objects.
    CALL FUNCTION 'TRINT_REQUEST_CHOICE'
      EXPORTING
        iv_suppress_dialog   = 'X'
        iv_request_types     = 'FTCOK'
        iv_lock_objects      = iv_lock
        iv_with_error_log    = 'X'
        iv_request           = iv_trkorr
      IMPORTING
        et_log               = et_log
      TABLES
        it_e071              = it_e071
      EXCEPTIONS
        invalid_request      = 1
        invalid_request_type = 2
        user_not_owner       = 3
        no_objects_appended  = 4
        enqueue_error        = 5
        cancelled_by_user    = 6
        recursive_call       = 7.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD create_workbench.
    create(
      EXPORTING
        iv_text   = iv_text
        iv_target = iv_target
        iv_type   = 'K'
      IMPORTING
        ev_trkorr = ev_trkorr
    ).
  ENDMETHOD.

  METHOD create_transport_of_copies.
    create(
      EXPORTING
        iv_text   = iv_text
        iv_target = iv_target
        iv_type   = 'T'
      IMPORTING
        ev_trkorr = ev_trkorr
    ).
  ENDMETHOD.

  METHOD create.
    DATA ls_header TYPE trwbo_request_header.
    CALL FUNCTION 'TR_INSERT_REQUEST_WITH_TASKS'
      EXPORTING
        iv_text           = iv_text
        iv_type           = iv_type
        iv_target         = iv_target
      IMPORTING
        es_request_header = ls_header
      EXCEPTIONS
        insert_failed     = 1
        enqueue_failed    = 2.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
    ev_trkorr = ls_header-trkorr.
  ENDMETHOD.

  METHOD delete.
    CALL FUNCTION 'TR_DELETE_COMM'
      EXPORTING
        wi_dialog = ' '
        wi_trkorr = iv_trkorr
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

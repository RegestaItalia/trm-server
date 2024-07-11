CLASS zcl_trm_transport DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tyt_lxe_packg TYPE STANDARD TABLE OF lxe_tt_packg_line.

    CLASS-METHODS add_translations_to_transport
      IMPORTING iv_trkorr   TYPE trkorr
                it_devclass TYPE tyt_lxe_packg
      RAISING   zcx_trm_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_transport IMPLEMENTATION.

  METHOD add_translations_to_transport.
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
        zcx_trm_exception=>raise(  ).
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
        zcx_trm_exception=>raise(  ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

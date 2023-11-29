FUNCTION ZTRM_ADD_LANG_TR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"  TABLES
*"      IT_DEVCLASS STRUCTURE  LXE_TT_PACKG_LINE
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      EMPTY_DEVCLASS
*"      LANGUAGE_NOT_IN_CP
*"      CREATE_REQUEST_FAILED
*"      WRITE_TO_REQUEST_FAILED
*"      RELEASE_REQUEST_FAILED
*"      INVALID_REQUEST
*"      NO_CORRECT_OBJECTS_AVAILABLE
*"      UNKNOWN
*"----------------------------------------------------------------------
  DATA: it_langs   TYPE TABLE OF lxeisolang,
        wa_langs   TYPE lxeisolang,
        lo_explang TYPE REF TO cl_lxe_log_export,
        lv_ret_cnt TYPE i,
        lt_objct   TYPE lxe_tt_objct,
        lt_user    TYPE lxe_tt_user,
        lt_date    TYPE lxe_tt_date,
        lt_comp    TYPE lxe_tt_comp.

  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

  IF it_devclass[] IS INITIAL.
    RAISE empty_devclass.
  ENDIF.

  SELECT DISTINCT targlng FROM lxe_log INTO TABLE it_langs WHERE custmnr EQ cl_lxe_constants=>c_trl_area_local.

  LOOP AT it_langs INTO wa_langs.

    CALL FUNCTION 'LXE_T002_CHECK_LANGUAGE'
      EXPORTING
        language           = wa_langs
      EXCEPTIONS
        language_not_in_cp = 1
        unknown            = 2
        OTHERS             = 3.

    IF sy-subrc EQ 1.
      RAISE language_not_in_cp.
    ELSEIF sy-subrc EQ 2.
      RAISE unknown.
    ELSEIF sy-subrc EQ 3.
      RAISE unknown.
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

    IF sy-subrc EQ 1.
      RAISE create_request_failed.
    ELSEIF sy-subrc EQ 2.
      RAISE write_to_request_failed.
    ELSEIF sy-subrc EQ 3.
      RAISE release_request_failed.
    ELSEIF sy-subrc EQ 4.
      RAISE invalid_request.
    ELSEIF sy-subrc EQ 5.
      RAISE no_correct_objects_available.
    ELSEIF sy-subrc EQ 6.
      RAISE unknown.
    ENDIF.
  ENDLOOP.




ENDFUNCTION.

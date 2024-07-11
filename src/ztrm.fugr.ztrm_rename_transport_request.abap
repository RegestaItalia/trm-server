FUNCTION ztrm_rename_transport_request.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"     VALUE(IV_AS4TEXT) TYPE  AS4TEXT
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      UPDATE_FAILED
*"----------------------------------------------------------------------
  PERFORM check_auth.


  CALL FUNCTION 'ENQUEUE_E_TRKORR'
    EXPORTING
      trkorr = iv_trkorr.
  IF sy-subrc <> 0.
    RAISE update_failed.
  ENDIF.

  "LSTR6F02 - e070_update
  DATA: ls_e070  LIKE e070,
        ls_e070c LIKE e070c,
        ls_e07t  LIKE e07t.
  DATA: lv_msgtext1 LIKE sy-msgv3,
        lv_msgtext2 LIKE sy-msgv3,
        lv_msgtext3 LIKE sy-msgv3.

  SELECT SINGLE * INTO ls_e070
         FROM e070
         WHERE trkorr = iv_trkorr.
  SELECT SINGLE * INTO ls_e070c "read additional fields - note 2231381
         FROM e070c
         WHERE trkorr = iv_trkorr.

  ls_e07t-trkorr = iv_trkorr.
  ls_e07t-langu = sy-langu.
  ls_e07t-as4text = iv_as4text.


  CALL FUNCTION 'TRINT_UPDATE_COMM_HEADER'
    EXPORTING
      wi_e070            = ls_e070
      wi_e07t            = ls_e07t
      wi_save_user       = ' '
      wi_sel_e070        = 'X'
      wi_sel_e07t        = 'X'
      wi_user            = sy-uname
      wi_e070c           = ls_e070c
      wi_sel_e070c       = 'X'
    IMPORTING
      we_e070            = ls_e070
      we_e070c           = ls_e070c
    EXCEPTIONS
      e070_update_error  = 01
      e07t_update_error  = 02
      e070c_update_error = 03.

  IF sy-subrc <> 0.
    CALL FUNCTION 'DEQUEUE_E_TRKORR'
      EXPORTING
        trkorr = iv_trkorr.
    RAISE update_failed.
  ENDIF.

  lv_msgtext1 = ls_e070-trkorr.
  lv_msgtext2 = sy-uname.
  lv_msgtext3 = ' '.

  CALL FUNCTION 'TRINT_APPEND_COMM_SYSLOG_ENTRY'
    EXPORTING
      wi_msgid      = 'TR'
      wi_msgno      = '018'
      wi_msgv2      = lv_msgtext1
      wi_msgv3      = lv_msgtext2
      wi_msgv4      = lv_msgtext3
      wi_new_order  = ' '
      wi_trfunction = ls_e070-trfunction
      wi_trkorr     = ls_e070-trkorr.


ENDFUNCTION.

CLASS zcl_trm_transport DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tyt_lxe_packg TYPE STANDARD TABLE OF lxe_tt_packg_line WITH DEFAULT KEY,
           tyt_e071      TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
           tyt_tline     TYPE STANDARD TABLE OF tline WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING iv_trkorr TYPE trkorr
      RAISING   zcx_trm_exception.

    CLASS-METHODS create_workbench
      IMPORTING iv_text             TYPE as4text
                iv_target           TYPE tr_target
      RETURNING VALUE(ro_transport) TYPE REF TO zcl_trm_transport
      RAISING   zcx_trm_exception.

    CLASS-METHODS create_transport_of_copies
      IMPORTING iv_text             TYPE as4text
                iv_target           TYPE tr_target
      RETURNING VALUE(ro_transport) TYPE REF TO zcl_trm_transport
      RAISING   zcx_trm_exception.

    CLASS-METHODS find_object_lock
      IMPORTING iv_pgmid            TYPE pgmid
                iv_object           TYPE trobjtype
                iv_obj_name         TYPE trobj_name
      RETURNING VALUE(ro_transport) TYPE REF TO zcl_trm_transport
      RAISING   zcx_trm_exception.

    CLASS-METHODS read_queue
      IMPORTING iv_target   TYPE tmssysnam
      EXPORTING et_requests TYPE tmsiqreqs
      RAISING   zcx_trm_exception.

    METHODS get_trkorr
      RETURNING VALUE(rv_trkorr) TYPE trkorr.

    METHODS add_translations
      IMPORTING it_devclass TYPE tyt_lxe_packg
      RAISING   zcx_trm_exception.

    METHODS add_objects
      IMPORTING iv_lock TYPE flag
                it_e071 TYPE tyt_e071
      EXPORTING et_log  TYPE sprot_u_tab
      RAISING   zcx_trm_exception.

    METHODS delete
      RAISING zcx_trm_exception.

    METHODS enqueue
      RAISING zcx_trm_exception.

    METHODS dequeue
      RAISING zcx_trm_exception.

    METHODS forward
      IMPORTING iv_target       TYPE tmssysnam
                iv_source       TYPE tmssysnam
                iv_import_again TYPE flag
      RAISING   zcx_trm_exception.

    METHODS import
      IMPORTING iv_system TYPE tmssysnam
      RAISING   zcx_trm_exception.

    METHODS release
      IMPORTING iv_lock     TYPE flag
      EXPORTING et_messages TYPE ctsgerrmsgs
      RAISING   zcx_trm_exception.

    METHODS rename
      IMPORTING iv_as4text TYPE as4text
      RAISING   zcx_trm_exception.

    METHODS set_documentation
      IMPORTING it_doc TYPE tyt_tline
      RAISING   zcx_trm_exception.

    METHODS copy
      IMPORTING iv_trkorr TYPE trkorr
                iv_doc    TYPE trparflag
      RAISING   zcx_trm_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS create
      IMPORTING iv_text             TYPE as4text
                iv_target           TYPE tr_target
                iv_type             TYPE trfunction
      RETURNING VALUE(ro_transport) TYPE REF TO zcl_trm_transport
      RAISING   zcx_trm_exception.

    DATA: gv_trkorr TYPE trkorr.
ENDCLASS.



CLASS zcl_trm_transport IMPLEMENTATION.

  METHOD constructor.
    gv_trkorr = iv_trkorr.
  ENDMETHOD.

  METHOD get_trkorr.
    rv_trkorr = gv_trkorr.
  ENDMETHOD.

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
          tr_request                   = gv_trkorr
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
    DATA lt_e071 LIKE it_e071.
    MOVE it_e071[] TO lt_e071[].
    CALL FUNCTION 'TRINT_REQUEST_CHOICE'
      EXPORTING
        iv_suppress_dialog   = 'X'
        iv_request_types     = 'FTCOK'
        iv_lock_objects      = iv_lock
        iv_with_error_log    = 'X'
        iv_request           = gv_trkorr
      IMPORTING
        et_log               = et_log
      TABLES
        it_e071              = lt_e071
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
        iv_text      = iv_text
        iv_target    = iv_target
        iv_type      = 'K'
      RECEIVING
        ro_transport = ro_transport
    ).
  ENDMETHOD.

  METHOD create_transport_of_copies.
    create(
      EXPORTING
        iv_text      = iv_text
        iv_target    = iv_target
        iv_type      = 'T'
      RECEIVING
        ro_transport = ro_transport
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
    CREATE OBJECT ro_transport EXPORTING iv_trkorr = ls_header-trkorr.
  ENDMETHOD.

  METHOD delete.
    CALL FUNCTION 'TR_DELETE_COMM'
      EXPORTING
        wi_dialog = ' '
        wi_trkorr = gv_trkorr
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD enqueue.
    CALL FUNCTION 'ENQUEUE_E_TRKORR'
      EXPORTING
        trkorr = gv_trkorr
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( iv_reason  = zcx_trm_exception=>c_reason-enqueue_error ).
    ENDIF.
  ENDMETHOD.

  METHOD dequeue.
    CALL FUNCTION 'DEQUEUE_E_TRKORR'
      EXPORTING
        trkorr = gv_trkorr
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( iv_reason  = zcx_trm_exception=>c_reason-dequeue_error ).
    ENDIF.
  ENDMETHOD.

  METHOD forward.
    DATA: lt_stdout    TYPE STANDARD TABLE OF tpstdout,
          ls_stdout    LIKE LINE OF lt_stdout,
          lt_log       TYPE zcx_trm_exception=>tyt_log,
          ls_exception TYPE stmscalert.
    CALL FUNCTION 'TMS_MGR_FORWARD_TR_REQUEST'
      EXPORTING
        iv_request      = gv_trkorr
        iv_target       = iv_target
        iv_source       = iv_source
        iv_import_again = iv_import_again
        iv_monitor      = space
      IMPORTING
        es_exception    = ls_exception
      TABLES
        tt_stdout       = lt_stdout
      EXCEPTIONS
        OTHERS          = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ELSEIF ls_exception-msgty EQ 'E' OR ls_exception-msgty EQ 'A'.
      LOOP AT lt_stdout INTO ls_stdout.
        APPEND ls_stdout-line TO lt_log.
      ENDLOOP.
      syst-msgid = ls_exception-msgid.
      syst-msgno = ls_exception-msgno.
      syst-msgty = ls_exception-msgty.
      syst-msgv1 = ls_exception-msgv1.
      syst-msgv2 = ls_exception-msgv2.
      syst-msgv3 = ls_exception-msgv3.
      syst-msgv4 = ls_exception-msgv4.
      zcx_trm_exception=>raise( it_log = lt_log ).
    ENDIF.
  ENDMETHOD.

  METHOD find_object_lock.
    DATA: ls_e070   TYPE e070,
          lv_trkorr TYPE trkorr.
    SELECT SINGLE e070~trkorr e070~strkorr
    FROM e071
    INNER JOIN e070 ON e070~trkorr = e071~trkorr
    INTO CORRESPONDING FIELDS OF ls_e070
    WHERE e071~pgmid EQ iv_pgmid AND e071~object EQ iv_object AND e071~obj_name EQ iv_obj_name
          AND ( e070~trfunction EQ 'K' OR e070~trfunction EQ 'S' OR e070~trfunction EQ 'R' )
          AND e070~trstatus EQ 'D'.

    IF ls_e070-strkorr IS NOT INITIAL.
      lv_trkorr = ls_e070-strkorr.
    ELSE.
      lv_trkorr = ls_e070-trkorr.
    ENDIF.

    IF lv_trkorr IS NOT INITIAL.
      CREATE OBJECT ro_transport EXPORTING iv_trkorr = lv_trkorr.
    ENDIF.
  ENDMETHOD.

  METHOD import.
    CALL FUNCTION 'TMS_MGR_IMPORT_TR_REQUEST'
      EXPORTING
        iv_system             = iv_system
        iv_request            = gv_trkorr
        iv_ctc_active         = ' '
        iv_overtake           = 'X'
        iv_import_again       = 'X'
        iv_ignore_originality = 'X'
        iv_ignore_repairs     = 'X'
        iv_ignore_transtype   = 'X'
        iv_ignore_tabletype   = 'X'
        iv_ignore_predec      = 'X'
        iv_ignore_cvers       = 'X'
        iv_test_import        = ' '
        iv_subset             = 'X'
        iv_offline            = 'X'
        iv_monitor            = 'X'
        iv_verbose            = ' '
      EXCEPTIONS
        OTHERS                = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD read_queue.
    DATA: ls_bufcnt TYPE tmsbufcnt,
          ls_alog   TYPE tmsalog,
          lv_dummy  TYPE string.
    " 03072024 avoid display alert
    sy-batch = 'X'.

    CALL FUNCTION 'TMS_UIQ_IQD_READ_QUEUE'
      EXPORTING
        iv_system      = iv_target
        iv_collect     = 'X'
        iv_read_shadow = 'X'
      IMPORTING
        et_requests    = et_requests
        es_bufcnt      = ls_bufcnt
      EXCEPTIONS
        OTHERS         = 1.

    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.

    IF ls_bufcnt-alertid IS NOT INITIAL.
      CALL FUNCTION 'TMS_ALT_ANALYSE_ALERT'
        EXPORTING
          iv_alert_id   = ls_bufcnt-alertid
          iv_no_display = 'X'
        IMPORTING
          es_alog       = ls_alog
        EXCEPTIONS
          OTHERS        = 1.
      IF ls_alog-msgty EQ 'E' OR ls_alog-msgty EQ 'A' OR sy-subrc <> 0.
        MESSAGE ID ls_alog-msgid
        TYPE ls_alog-msgty
        NUMBER ls_alog-msgno
        INTO lv_dummy
        WITH ls_alog-msgv1 ls_alog-msgv2 ls_alog-msgv3 ls_alog-msgv4.
        zcx_trm_exception=>raise( iv_reason  = zcx_trm_exception=>c_reason-tms_alert ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD release.
    DATA lv_without_lock TYPE flag.
    IF iv_lock EQ 'X'.
      lv_without_lock = ' '.
    ELSE.
      lv_without_lock = 'X'.
    ENDIF.

    CALL FUNCTION 'TRINT_RELEASE_REQUEST'
      EXPORTING
        iv_trkorr                = gv_trkorr
        iv_dialog                = ' '
        iv_success_message       = ' '
        iv_display_export_log    = ' '
        iv_without_objects_check = 'X'
        iv_without_locking       = lv_without_lock
      EXCEPTIONS
        OTHERS                   = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD rename.
    enqueue( ).
    "LSTR6F02 - e070_update
    DATA: ls_e070  TYPE e070,
          ls_e070c TYPE e070c,
          ls_e07t  TYPE e07t.
    DATA: lv_msgtext1 LIKE sy-msgv3,
          lv_msgtext2 LIKE sy-msgv3,
          lv_msgtext3 LIKE sy-msgv3.

    SELECT SINGLE * INTO ls_e070
           FROM e070
           WHERE trkorr = gv_trkorr.
    SELECT SINGLE * INTO ls_e070c "read additional fields - note 2231381
           FROM e070c
           WHERE trkorr = gv_trkorr.

    ls_e07t-trkorr = gv_trkorr.
    ls_e07t-langu = sy-langu.
    ls_e07t-as4text = iv_as4text.

    CALL FUNCTION 'TRINT_UPDATE_COMM_HEADER'
      EXPORTING
        wi_e070      = ls_e070
        wi_e07t      = ls_e07t
        wi_save_user = ' '
        wi_sel_e070  = 'X'
        wi_sel_e07t  = 'X'
        wi_user      = sy-uname
        wi_e070c     = ls_e070c
        wi_sel_e070c = 'X'
      IMPORTING
        we_e070      = ls_e070
        we_e070c     = ls_e070c
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.

    dequeue( ).

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
  ENDMETHOD.

  METHOD set_documentation.
    DATA lt_doc LIKE it_doc.
    MOVE it_doc[] TO lt_doc[].
    CALL FUNCTION 'TRINT_DOCU_INTERFACE'
      EXPORTING
        iv_object           = gv_trkorr
        iv_action           = 'M'
        iv_modify_appending = ''
      TABLES
        tt_line             = lt_doc
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD copy.
    CALL FUNCTION 'TR_COPY_COMM'
      EXPORTING
        wi_dialog                = ' '
        wi_trkorr_from           = iv_trkorr
        wi_trkorr_to             = gv_trkorr
        wi_without_documentation = iv_doc
      EXCEPTIONS
        OTHERS                   = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

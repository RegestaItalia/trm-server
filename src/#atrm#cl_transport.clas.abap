"! Transport API
CLASS /atrm/cl_transport DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: tyt_lxe_packg TYPE STANDARD TABLE OF lxe_tt_packg_line WITH DEFAULT KEY,
           tyt_e071      TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
           tyt_e071k     TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY,
           tyt_tline     TYPE STANDARD TABLE OF tline WITH DEFAULT KEY.
    CONSTANTS:
      c_migrate_nr_range_nr TYPE nrnr   VALUE '00',
      c_migrate_object      TYPE nrobj  VALUE '/ATRM/TRNR',
      c_migrate_subobj      TYPE nrsobj VALUE space,
      c_migrate_toyear      TYPE nryear VALUE '0000'.

    "! Constructor
    "! @parameter trkorr | Transport request number
    METHODS constructor
      IMPORTING trkorr TYPE trkorr.

    "! Create a new workbench request
    "! @parameter text | Description of the transport
    "! @parameter target | Target system
    "! @parameter transport | Created transport object
    "! @raising /atrm/cx_exception | Raised if creation fails
    CLASS-METHODS create_workbench
      IMPORTING text             TYPE as4text
                target           TYPE tr_target
      RETURNING VALUE(transport) TYPE REF TO /atrm/cl_transport
      RAISING   /atrm/cx_exception.

    "! Create a new customizing request
    "! @parameter text | Description of the transport
    "! @parameter target | Target system
    "! @parameter transport | Created transport object
    "! @raising /atrm/cx_exception | Raised if creation fails
    CLASS-METHODS create_customizing
      IMPORTING text             TYPE as4text
                target           TYPE tr_target
      RETURNING VALUE(transport) TYPE REF TO /atrm/cl_transport
      RAISING   /atrm/cx_exception.

    "! Create a transport of copies
    "! @parameter text | Description of the transport
    "! @parameter target | Target system
    "! @parameter transport | Created transport object
    "! @raising /atrm/cx_exception | Raised if creation fails
    CLASS-METHODS create_transport_of_copies
      IMPORTING text             TYPE as4text
                target           TYPE tr_target
      RETURNING VALUE(transport) TYPE REF TO /atrm/cl_transport
      RAISING   /atrm/cx_exception.

    "! Read the TMS import queue for a system
    "! @parameter target | Target system name
    "! @parameter requests | Returned queue requests
    "! @raising /atrm/cx_exception | Raised if queue cannot be read
    CLASS-METHODS read_queue
      IMPORTING target   TYPE tmssysnam
      EXPORTING requests TYPE tmsiqreqs
      RAISING   /atrm/cx_exception.

    "! Get transport targets
    "! @parameter targets  | Transport targets
    "! @raising /atrm/cx_exception | Raised if not initialized
    CLASS-METHODS get_targets
      RETURNING VALUE(targets) TYPE tarsystems
      RAISING   /atrm/cx_exception.

    "! Get the current transport request number
    "! @parameter trkorr | Transport request number
    METHODS get_trkorr
      RETURNING VALUE(trkorr) TYPE trkorr.

    "! Add translation-related objects to the transport
    "! @parameter devclass | Table of development packages to include
    "! @raising /atrm/cx_exception | Raised if translations cannot be added
    METHODS add_translations
      IMPORTING devclass TYPE tyt_lxe_packg
      RAISING   /atrm/cx_exception.

    "! Add E071 objects to the transport
    "! @parameter lock | Lock the objects during addition
    "! @parameter e071 | Table of E071 entries to add
    "! @parameter log | Exported log of results
    "! @raising /atrm/cx_exception | Raised if addition fails
    METHODS add_objects
      IMPORTING lock TYPE flag
                e071 TYPE tyt_e071
      EXPORTING log  TYPE sprot_u_tab
      RAISING   /atrm/cx_exception.

    "! Remove comments for a specific object type
    "! @parameter object | Object type to clean
    "! @raising /atrm/cx_exception | Raised if deletion fails
    METHODS remove_comments
      IMPORTING object TYPE trobjtype
      RAISING   /atrm/cx_exception.

    "! Delete the transport request
    "! @raising /atrm/cx_exception | Raised if deletion fails
    METHODS delete
      RAISING /atrm/cx_exception.

    "! Enqueue the transport for editing
    "! @raising /atrm/cx_exception | Raised if enqueue fails
    METHODS enqueue
      RAISING /atrm/cx_exception.

    "! Dequeue the transport
    "! @raising /atrm/cx_exception | Raised if dequeue fails
    METHODS dequeue
      RAISING /atrm/cx_exception.

    "! Forward the transport to a target system
    "! @parameter target | Target system
    "! @parameter source | Source system
    "! @parameter import_again | Import even if already imported
    "! @raising /atrm/cx_exception | Raised if forwarding fails
    METHODS forward
      IMPORTING target       TYPE tmssysnam
                source       TYPE tmssysnam
                import_again TYPE flag
      RAISING   /atrm/cx_exception.

    "! Import the transport into a system
    "! @parameter system | Target system name
    "! @raising /atrm/cx_exception | Raised if import fails
    METHODS import
      IMPORTING system TYPE tmssysnam
      RAISING   /atrm/cx_exception.

    "! Release the transport
    "! @parameter lock | Whether to lock objects before release
    "! @parameter messages | Messages from the release operation
    "! @raising /atrm/cx_exception | Raised if release fails
    METHODS release
      IMPORTING lock     TYPE flag
      EXPORTING messages TYPE ctsgerrmsgs
      RAISING   /atrm/cx_exception.

    "! Rename the transport (change text)
    "! @parameter as4text | New description
    "! @raising /atrm/cx_exception | Raised if rename fails
    METHODS rename
      IMPORTING as4text TYPE as4text
      RAISING   /atrm/cx_exception.

    "! Set documentation text for the transport
    "! @parameter doc | Documentation lines
    "! @raising /atrm/cx_exception | Raised if update fails
    METHODS set_documentation
      IMPORTING doc TYPE tyt_tline
      RAISING   /atrm/cx_exception.

    "! Copy another transport’s contents into this one
    "! @parameter trkorr | Source transport
    "! @parameter doc | Whether to skip documentation
    "! @raising /atrm/cx_exception | Raised if copy fails
    METHODS copy
      IMPORTING trkorr TYPE trkorr
                doc    TYPE trparflag
      RAISING   /atrm/cx_exception.

    "! Delete transport from the TMS import queue
    "! @parameter system | Target system name
    "! @raising /atrm/cx_exception | Raised if removal fails
    METHODS delete_from_tms_queue
      IMPORTING system TYPE tmssysnam
      RAISING   /atrm/cx_exception.

    "! Update transport description in TMS buffer
    "! @raising /atrm/cx_exception | Raised if refresh fails
    METHODS refresh_tms_txt
      RAISING /atrm/cx_exception.

    "! Get list of E071 objects
    "! @parameter e071 | E071 entries in the transport
    METHODS get_e071
      RETURNING VALUE(e071) TYPE tyt_e071.

    "! Get list of E071K entries
    "! @parameter e071k | E071K entries in the transport
    METHODS get_e071k
      RETURNING VALUE(e071k) TYPE tyt_e071k.

    "! Change transport owner
    "! @parameter user         | New owner
    "! @raising /atrm/cx_exception | Raised if owner change fails
    METHODS set_owner
      IMPORTING user TYPE tr_as4user
      RAISING   /atrm/cx_exception.

    "! Get import status
    "! @parameter system       | Target system name
    "! @parameter stat         | Status
    "! @raising /atrm/cx_exception | Raised if status is not found
    METHODS get_import_status
      IMPORTING system      TYPE tmssysnam
      RETURNING VALUE(stat) TYPE tpstat
      RAISING   /atrm/cx_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS create
      IMPORTING text             TYPE as4text
                target           TYPE tr_target
                type             TYPE trfunction
      RETURNING VALUE(transport) TYPE REF TO /atrm/cl_transport
      RAISING   /atrm/cx_exception.

    DATA: gv_trkorr TYPE trkorr,
          gt_e071   TYPE tyt_e071,
          gt_e071k  TYPE tyt_e071k.
ENDCLASS.



CLASS /atrm/cl_transport IMPLEMENTATION.

  METHOD constructor.
    gv_trkorr = trkorr.
  ENDMETHOD.

  METHOD get_trkorr.
    trkorr = gv_trkorr.
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

    IF devclass[] IS INITIAL.
      /atrm/cx_exception=>raise( iv_message = 'No input packages defined'
                                iv_reason  = /atrm/cx_exception=>c_reason-invalid_input ).
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
        /atrm/cx_exception=>raise( ).
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
      lv_ret_cnt = lo_explang->apply_filters( st_packg  = devclass[]
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
        /atrm/cx_exception=>raise( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_objects.
    DATA: lt_e071    LIKE e071,
          ls_log     LIKE LINE OF log,
          lv_message TYPE string.
    MOVE e071[] TO lt_e071[].
    CALL FUNCTION 'TRINT_REQUEST_CHOICE'
      EXPORTING
        iv_suppress_dialog   = 'X'
        iv_request_types     = 'FTCOK'
        iv_lock_objects      = lock
        iv_with_error_log    = 'X'
        iv_request           = gv_trkorr
      IMPORTING
        et_log               = log
      TABLES
        it_e071              = lt_e071
      EXCEPTIONS
        no_objects_appended  = 0
        invalid_request      = 1
        invalid_request_type = 2
        user_not_owner       = 3
        enqueue_error        = 4
        cancelled_by_user    = 5
        recursive_call       = 6
        OTHERS               = 7.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.
    READ TABLE log INTO ls_log WITH KEY severity = 'E'.
    IF sy-subrc <> 0.
      READ TABLE log INTO ls_log WITH KEY severity = 'A'.
    ENDIF.
    IF ls_log IS NOT INITIAL.
      IF ls_log-ag IS NOT INITIAL.
        MESSAGE ID ls_log-ag TYPE 'I' NUMBER ls_log-msgnr WITH ls_log-var1 ls_log-var2 ls_log-var3 ls_log-var4 INTO lv_message.
        /atrm/cx_exception=>raise( iv_message = lv_message ).
      ELSE.
        /atrm/cx_exception=>raise( iv_message = 'Unknown error, check logs.' ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD remove_comments.
    DATA: lt_e071    TYPE STANDARD TABLE OF e071,
          ls_e071    LIKE LINE OF lt_e071,
          ls_request TYPE trwbo_request.
    ls_request-h-trkorr = gv_trkorr.
    SELECT * FROM e071 INTO TABLE lt_e071 WHERE pgmid EQ '*' AND object EQ object AND trkorr EQ gv_trkorr.
    CHECK lt_e071[] IS NOT INITIAL.
    LOOP AT lt_e071 INTO ls_e071.
      CALL FUNCTION 'TR_DELETE_COMM_OBJECT_KEYS'
        EXPORTING
          is_e071_delete              = ls_e071
          iv_dialog_flag              = ' '
        CHANGING
          cs_request                  = ls_request
        EXCEPTIONS
          e_database_access_error     = 1
          e_empty_lockkey             = 2
          e_bad_target_request        = 3
          e_wrong_source_client       = 4
          n_no_deletion_of_c_objects  = 5
          n_no_deletion_of_corr_entry = 6
          n_object_entry_doesnt_exist = 7
          n_request_already_released  = 8
          n_request_from_other_system = 9
          r_action_aborted_by_user    = 10
          r_foreign_lock              = 11
          w_bigger_lock_in_same_order = 12
          w_duplicate_entry           = 13
          w_no_authorization          = 14
          w_user_not_owner            = 15
          OTHERS                      = 16.
      IF sy-subrc <> 0.
        /atrm/cx_exception=>raise( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_workbench.
    create(
      EXPORTING
        text      = text
        target    = target
        type      = 'K'
      RECEIVING
        transport = transport
    ).
  ENDMETHOD.

  METHOD create_customizing.
    create(
      EXPORTING
        text      = text
        target    = target
        type      = 'W'
      RECEIVING
        transport = transport
    ).
  ENDMETHOD.

  METHOD create_transport_of_copies.
    create(
      EXPORTING
        text      = text
        target    = target
        type      = 'T'
      RECEIVING
        transport = transport
    ).
  ENDMETHOD.

  METHOD create.
    DATA ls_header TYPE trwbo_request_header.
    CALL FUNCTION 'TR_INSERT_REQUEST_WITH_TASKS'
      EXPORTING
        iv_text           = text
        iv_type           = type
        iv_target         = target
      IMPORTING
        es_request_header = ls_header
      EXCEPTIONS
        insert_failed     = 1
        enqueue_failed    = 2.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.
    CREATE OBJECT transport EXPORTING trkorr = ls_header-trkorr.
  ENDMETHOD.

  METHOD delete.
    CALL FUNCTION 'TR_DELETE_COMM'
      EXPORTING
        wi_dialog = ' '
        wi_trkorr = gv_trkorr
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD enqueue.
    CALL FUNCTION 'ENQUEUE_E_TRKORR'
      EXPORTING
        trkorr = gv_trkorr
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( iv_reason = /atrm/cx_exception=>c_reason-enqueue_error ).
    ENDIF.
  ENDMETHOD.

  METHOD dequeue.
    CALL FUNCTION 'DEQUEUE_E_TRKORR'
      EXPORTING
        trkorr = gv_trkorr
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( iv_reason = /atrm/cx_exception=>c_reason-dequeue_error ).
    ENDIF.
  ENDMETHOD.

  METHOD forward.
    DATA: lt_stdout    TYPE STANDARD TABLE OF tpstdout,
          ls_stdout    LIKE LINE OF lt_stdout,
          lt_log       TYPE /atrm/cx_exception=>tyt_log,
          ls_exception TYPE stmscalert.
    CALL FUNCTION 'TMS_MGR_FORWARD_TR_REQUEST'
      EXPORTING
        iv_request      = gv_trkorr
        iv_target       = target
        iv_source       = source
        iv_import_again = import_again
        iv_monitor      = space
      IMPORTING
        es_exception    = ls_exception
      TABLES
        tt_stdout       = lt_stdout
      EXCEPTIONS
        OTHERS          = 1.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
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
      /atrm/cx_exception=>raise( it_log = lt_log ).
    ENDIF.
  ENDMETHOD.

  METHOD import.
    CALL FUNCTION 'TMS_MGR_IMPORT_TR_REQUEST'
      EXPORTING
        iv_system             = system
        iv_request            = gv_trkorr
        iv_client             = sy-mandt
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
      /atrm/cx_exception=>raise( ).
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
        iv_system      = target
        iv_collect     = 'X'
        iv_read_shadow = 'X'
      IMPORTING
        et_requests    = requests
        es_bufcnt      = ls_bufcnt
      EXCEPTIONS
        OTHERS         = 1.

    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
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
        /atrm/cx_exception=>raise( iv_reason = /atrm/cx_exception=>c_reason-tms_alert ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD release.
    DATA lv_without_lock TYPE flag.
    IF lock EQ 'X'.
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
      /atrm/cx_exception=>raise( ).
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
    ls_e07t-as4text = as4text.

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
      /atrm/cx_exception=>raise( ).
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
    DATA lt_doc LIKE doc.
    MOVE doc[] TO lt_doc[].
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
      /atrm/cx_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD copy.
    CALL FUNCTION 'TR_COPY_COMM'
      EXPORTING
        wi_dialog                = ' '
        wi_trkorr_from           = trkorr
        wi_trkorr_to             = gv_trkorr
        wi_without_documentation = doc
      EXCEPTIONS
        OTHERS                   = 1.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD delete_from_tms_queue.
    DATA: ls_tmsbuffer    TYPE tmsbuffer,
          lt_tp_maintains TYPE stms_tp_maintains,
          ls_tp_maintains LIKE LINE OF lt_tp_maintains,
          ls_tpstdout     TYPE tpstdout,
          lt_log          TYPE /atrm/cx_exception=>tyt_log,
          ls_exception    TYPE stmscalert.

    SELECT SINGLE * FROM tmsbuffer INTO ls_tmsbuffer WHERE sysnam EQ system AND trkorr EQ gv_trkorr.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'TMS_MGR_MAINTAIN_TR_QUEUE'
      EXPORTING
        iv_command                 = 'DELFROMBUFFER'
        iv_system                  = ls_tmsbuffer-sysnam
        iv_domain                  = ls_tmsbuffer-domnam
        iv_request                 = ls_tmsbuffer-trkorr
        iv_tarcli                  = ls_tmsbuffer-tarcli
        iv_monitor                 = ' '
        iv_verbose                 = ' '
      IMPORTING
        et_tp_maintains            = lt_tp_maintains
        es_exception               = ls_exception
      EXCEPTIONS
        read_config_failed         = 1
        table_of_requests_is_empty = 2
        OTHERS                     = 3.

    IF sy-subrc <> 0 OR ( ls_exception-error <> 'OK' AND ls_exception-error <> space ).
      IF lt_tp_maintains[] IS NOT INITIAL.
        READ TABLE lt_tp_maintains INTO ls_tp_maintains INDEX 1.
        LOOP AT ls_tp_maintains-tp_stdout INTO ls_tpstdout.
          APPEND ls_tpstdout-line TO lt_log.
        ENDLOOP.
        /atrm/cx_exception=>raise( it_log = lt_log ).
      ELSE.
        /atrm/cx_exception=>raise( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD refresh_tms_txt.
    DATA ls_tmsbuftxt TYPE tmsbuftxt.
    SELECT SINGLE * FROM tmsbuftxt INTO ls_tmsbuftxt WHERE trkorr EQ gv_trkorr.
    CHECK sy-subrc EQ 0.
    SELECT SINGLE as4text FROM e07t INTO ls_tmsbuftxt-text WHERE trkorr EQ gv_trkorr.
    SELECT SINGLE as4user FROM e070 INTO ls_tmsbuftxt-owner WHERE trkorr EQ gv_trkorr.
    SELECT SINGLE client FROM e070c INTO ls_tmsbuftxt-srccli WHERE trkorr EQ gv_trkorr.
    " there's no standard way to update buffer text table other than clearing the buffer as a whole?
    MODIFY tmsbuftxt FROM ls_tmsbuftxt.
    COMMIT WORK AND WAIT.
    " don't raise exception if it fails!!
  ENDMETHOD.

  METHOD get_e071.
    IF gt_e071[] IS INITIAL.
      SELECT * FROM e071 INTO CORRESPONDING FIELDS OF TABLE gt_e071 WHERE trkorr EQ gv_trkorr.
    ENDIF.
    e071[] = gt_e071[].
  ENDMETHOD.

  METHOD get_e071k.
    IF gt_e071k[] IS INITIAL.
      SELECT * FROM e071k INTO CORRESPONDING FIELDS OF TABLE gt_e071k WHERE trkorr EQ gv_trkorr.
    ENDIF.
    e071k[] = gt_e071k[].
  ENDMETHOD.

  METHOD set_owner.
    CALL FUNCTION 'TR_CHANGE_USERNAME'
      EXPORTING
        wi_dialog           = ' '
        wi_trkorr           = gv_trkorr
        wi_user             = user
      EXCEPTIONS
        already_released    = 1
        e070_update_error   = 2
        file_access_error   = 3
        not_exist_e070      = 4
        user_does_not_exist = 5
        tr_enqueue_failed   = 6
        no_authorization    = 7
        wrong_client        = 8
        unallowed_user      = 9
        OTHERS              = 10.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_import_status.
    DATA: ls_tmsbuffer TYPE tmsbuffer,
          lt_stats     TYPE tpstats.

    SELECT SINGLE * FROM tmsbuffer INTO ls_tmsbuffer WHERE sysnam EQ system AND trkorr EQ gv_trkorr.
    CHECK sy-subrc EQ 0.
    CALL FUNCTION 'TMS_IMU_FILTER_TPSTAT'
      EXPORTING
        iv_system   = ls_tmsbuffer-sysnam
        iv_domain   = ls_tmsbuffer-domnam
        iv_request  = ls_tmsbuffer-trkorr
        iv_project  = ls_tmsbuffer-project
        iv_tarcli   = ls_tmsbuffer-tarcli
        iv_tpstatid = ls_tmsbuffer-tpstatid
        iv_jobid    = ls_tmsbuffer-jobid
        iv_verbose  = 'X'
      IMPORTING
        et_tpstat   = lt_stats.
    IF lt_stats[] IS INITIAL.
      /atrm/cx_exception=>raise(
        iv_message = 'No import status found for this transport in the target system'
        iv_reason = /atrm/cx_exception=>c_reason-not_found
      ).
    ENDIF.
    SORT lt_stats BY timestamp DESCENDING.
    stat = lt_stats[ 1 ].
  ENDMETHOD.

  METHOD get_targets.
    DATA: consolidations LIKE targets,
          consolidation  LIKE LINE OF consolidations,
          order          TYPE STANDARD TABLE OF tmscsys,
          order_line     LIKE LINE OF order.
    FIELD-SYMBOLS <tarsystem> TYPE tarsystem.
    CALL FUNCTION 'TR_GET_CONSOLIDATION_TARGETS'
      IMPORTING
        et_targets             = consolidations
      EXCEPTIONS
        system_not_initialized = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.
    CHECK consolidations[] IS NOT INITIAL.
    SELECT sysnam
      FROM tmscsys
      INTO CORRESPONDING FIELDS OF TABLE order
      FOR ALL ENTRIES IN consolidations
      WHERE sysnam EQ consolidations-table_line and systyp EQ 'V'.
    LOOP AT order INTO order_line.
      APPEND INITIAL LINE TO targets ASSIGNING <tarsystem>.
      <tarsystem> = order_line-sysnam.
    ENDLOOP.
    LOOP AT consolidations INTO consolidation.
      READ TABLE targets TRANSPORTING NO FIELDS WITH KEY table_line = consolidation.
      CHECK sy-subrc <> 0.
      APPEND INITIAL LINE TO targets ASSIGNING <tarsystem>.
      <tarsystem> = consolidation.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

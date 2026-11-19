"! Transport API
"! Transport API
CLASS /atrm/cl_transport DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPE-POOLS: abap, trwbo.

    TYPES: tyt_lxe_packg TYPE STANDARD TABLE OF /atrm/devclass_range WITH DEFAULT KEY,
           tyt_e071      TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
           tyt_e071k     TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY,
           tyt_tline     TYPE STANDARD TABLE OF tline WITH DEFAULT KEY.

    "! Result of a TMS tp call (import, forward, queue maintenance).
    "! Same component names as type group STMS's STMS_TP_IMPORT, which is not
    "! on every release: trm-core reads it as XML (/ATRM/IMPORT_TR test_result)
    TYPES: BEGIN OF ty_tr_request,
             trkorr  TYPE trkorr,
             tarcli  TYPE trtarcli,
             project TYPE trkorr_p,
             preflg  TYPE trtppreflg,
             owner   TYPE tmsowner,
           END OF ty_tr_request,
           tyt_tp_stdout TYPE STANDARD TABLE OF tpstdout WITH DEFAULT KEY,
           tyt_tp_logptr TYPE STANDARD TABLE OF tplogptr WITH DEFAULT KEY,
           BEGIN OF ty_tp_result,
             request     TYPE ty_tr_request,
             tp_ret_code TYPE trretcode,
             tp_alog     TYPE trfile,
             tp_slog     TYPE trfile,
             tp_pid      TYPE wppid,
             tp_stdout   TYPE tyt_tp_stdout,
             tp_logptr   TYPE tyt_tp_logptr,
             alert       TYPE stmscalert,
           END OF ty_tp_result.

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
                with_task        TYPE flag OPTIONAL
                task_type        TYPE trfunction OPTIONAL
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

    "! Get the transport layers with a consolidation route from this system to a target
    "! @parameter target | Transport target
    "! @parameter layers | Transport layers, sorted
    "! @raising /atrm/cx_exception | Raised if target is missing or no transport configuration is active
    CLASS-METHODS get_transport_layers
      IMPORTING target        TYPE tarsystem
      RETURNING VALUE(layers) TYPE /atrm/devlayer_t
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

    "! Import the transport into a system asynchronously
    "! @parameter system | Target system name
    "! @parameter test   | Test import
    "! @raising /atrm/cx_exception | Raised if import fails
    METHODS import
      IMPORTING system        TYPE tmssysnam
                test          TYPE flag
      RETURNING VALUE(import) TYPE ty_tp_result
      RAISING   /atrm/cx_exception.

    "! Release the transport and wait until the update task has completed
    "! the release (status O, R or N); the export may still be running.
    "! @parameter lock | Whether to lock objects before release
    "! @parameter messages | Messages from the release operation
    "! @raising /atrm/cx_exception | Raised if release fails or the update task does not complete
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

    "! Sort and compress transport objects
    "! @raising /atrm/cx_exception | Raised on error
    METHODS sort_and_compress
      RAISING /atrm/cx_exception.

    "! Lock all objects in the transport
    "! @raising /atrm/cx_exception | Raised on error
    METHODS lock
      RAISING /atrm/cx_exception.

    "! Import multiple transports (in batch)
    "! @parameter system       | Target system name
    "! @parameter transports   | List of transport requests
    "! @parameter test         | Test import
    "! @raising /atrm/cx_exception | Raised if import fails
    CLASS-METHODS import_multiple
      IMPORTING system        TYPE tmssysnam
                transports    TYPE /atrm/trkorr_t
                test          TYPE flag
      RETURNING VALUE(import) TYPE ty_tp_result
      RAISING   /atrm/cx_exception.

    METHODS get_entries
      EXPORTING e071   TYPE tyt_e071
                tadir  TYPE scts_tadir
                tdevc  TYPE /atrm/cl_core=>tyt_tdevc
                tdevct TYPE /atrm/cl_core=>tyt_tdevct.
    "! Wait until the release is really completed.
    "! TRINT_RELEASE_REQUEST returns once the request is handed to the update
    "! task; versioning and export run afterwards and can still fail.
    "! @parameter timeout | Maximum wait in seconds
    "! @parameter exported | Wait for the export too (R/N); otherwise return once the update task is done (O)
    "! @parameter status | Final request status
    "! @raising /atrm/cx_exception | Raised if not completed within timeout
    METHODS wait_release
      IMPORTING timeout       TYPE i DEFAULT 600
                exported      TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(status) TYPE trstatus
      RAISING   /atrm/cx_exception.
    "! Remove every object entry of the request and of its tasks, with its object lock
    "! @raising /atrm/cx_exception | First entry that couldn't be removed (the others are attempted)
    METHODS delete_entries
      RAISING /atrm/cx_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS execute_import
      IMPORTING system        TYPE tmssysnam
                transport     TYPE trkorr
                test          TYPE flag
                transports    TYPE /atrm/trkorr_t OPTIONAL
      RETURNING VALUE(import) TYPE ty_tp_result
      RAISING   /atrm/cx_exception.

    CLASS-METHODS create
      IMPORTING text             TYPE as4text
                target           TYPE tr_target
                type             TYPE trfunction
                attributes       TYPE scts_attrs OPTIONAL
                users            TYPE scts_users OPTIONAL
      RETURNING VALUE(transport) TYPE REF TO /atrm/cl_transport
      RAISING   /atrm/cx_exception.
METHODS complete_shi3_entries
      CHANGING e071  TYPE tyt_e071
               e071k TYPE tyt_e071k
      RAISING  /atrm/cx_exception.
METHODS record_fdt0_entries
      IMPORTING e071 TYPE tyt_e071
      RAISING   /atrm/cx_exception.
CLASS-METHODS add_fm_value
      IMPORTING name   TYPE abap_parmname
                value  TYPE any
      CHANGING  params TYPE abap_func_parmbind_tab.
CLASS-METHODS fm_has_parameter
      IMPORTING funcname     TYPE rs38l_fnam
                parameter    TYPE rs38l_par_
      RETURNING VALUE(found) TYPE flag.
CLASS-METHODS create_fm_param_data
      IMPORTING funcname    TYPE rs38l_fnam
                parameter   TYPE rs38l_par_
      RETURNING VALUE(data) TYPE REF TO data.
CLASS-METHODS copy_tp_result
      IMPORTING source TYPE any
      CHANGING  result TYPE ty_tp_result.
CLASS-METHODS bind_fm_result
      IMPORTING funcname TYPE rs38l_fnam
                name     TYPE abap_parmname
                kind     TYPE i
      CHANGING  value    TYPE any
                params   TYPE abap_func_parmbind_tab.
CLASS-METHODS call_tms
      IMPORTING funcname       TYPE rs38l_fnam
                result_param   TYPE abap_parmname
                params         TYPE abap_func_parmbind_tab
                not_found_text TYPE string
      RETURNING VALUE(result)  TYPE ty_tp_result
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
    CONSTANTS lc_trl_area_local TYPE lxe_log-custmnr VALUE '999999'.
    DATA: lo_lock_error TYPE REF TO /atrm/cx_exception,
          lr_date       TYPE REF TO data,
          lr_objct      TYPE REF TO data,
          lr_user       TYPE REF TO data,
          lr_comp       TYPE REF TO data.
    FIELD-SYMBOLS:
      <lt_date>  TYPE STANDARD TABLE,
      <lt_objct> TYPE STANDARD TABLE,
      <lt_user>  TYPE STANDARD TABLE,
      <lt_comp>  TYPE STANDARD TABLE,
      <ls_date>  TYPE any,
      <lv_value> TYPE any.

    enqueue( ).
    TRY.
        DATA: lt_langs   TYPE TABLE OF lxeisolang,
              wa_langs   LIKE LINE OF lt_langs,
              lo_explang TYPE REF TO object,
              lv_ret_cnt TYPE i.

        IF devclass[] IS INITIAL.
          /atrm/cx_exception=>raise( iv_message = 'No input packages defined' "#EC NOTEXT
                                    iv_reason  = /atrm/cx_exception=>c_reason-invalid_input ).
        ENDIF.

        TRY.
            CREATE DATA lr_date TYPE ('LXE_TT_DATE').
            CREATE DATA lr_objct TYPE ('LXE_TT_OBJCT').
            CREATE DATA lr_user TYPE ('LXE_TT_USER').
            CREATE DATA lr_comp TYPE ('LXE_TT_COMP').
            ASSIGN lr_date->* TO <lt_date>.
            ASSIGN lr_objct->* TO <lt_objct>.
            ASSIGN lr_user->* TO <lt_user>.
            ASSIGN lr_comp->* TO <lt_comp>.
          CATCH cx_root.
            " Translation log API is not available on this SAP release
            sort_and_compress( ).
            dequeue( ).
            RETURN.
        ENDTRY.

        SELECT DISTINCT targlng FROM lxe_log INTO TABLE lt_langs WHERE custmnr EQ lc_trl_area_local.

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
          TRY.
              CREATE OBJECT lo_explang TYPE ('CL_LXE_LOG_EXPORT_LOCAL')
                EXPORTING
                  custmnr = lc_trl_area_local
                  isolang = wa_langs.
            CATCH cx_root.
              " Translation export class is not available on this SAP release
              CONTINUE.
          ENDTRY.

          APPEND INITIAL LINE TO <lt_date> ASSIGNING <ls_date>.
          ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_date> TO <lv_value>.
          IF sy-subrc = 0.
            <lv_value> = 'I'.
          ENDIF.
          UNASSIGN <lv_value>.
          ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_date> TO <lv_value>.
          IF sy-subrc = 0.
            <lv_value> = 'LE'.
          ENDIF.
          UNASSIGN <lv_value>.
          ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_date> TO <lv_value>.
          IF sy-subrc = 0.
            <lv_value> = sy-datum.
          ENDIF.

          CLEAR lv_ret_cnt.
          CALL METHOD lo_explang->('LOG_SELECT')
            EXPORTING
              st_objct = <lt_objct>
              st_user  = <lt_user>
              st_date  = <lt_date>
            RECEIVING
              ret_cnt  = lv_ret_cnt.
          CHECK lv_ret_cnt NE 0.

          CLEAR lv_ret_cnt.
          CALL METHOD lo_explang->('APPLY_FILTERS')
            EXPORTING
              st_packg  = devclass[]
              st_comp   = <lt_comp>
              piecelist = ' '
              check_ex  = 'X'
              chckorlg  = ' '
              clientdp  = ' '
            RECEIVING
              ret_cnt   = lv_ret_cnt.

          CHECK lv_ret_cnt NE 0.

          CALL METHOD lo_explang->('EXPORT_OBJECTS')
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
        sort_and_compress( ).
      CATCH /atrm/cx_exception INTO lo_lock_error.
        TRY.
            dequeue( ).
          CATCH /atrm/cx_exception.
        ENDTRY.
        RAISE EXCEPTION lo_lock_error.
    ENDTRY.
    dequeue( ).
  ENDMETHOD.

  METHOD add_objects.
    DATA: lo_lock_error TYPE REF TO /atrm/cx_exception,
          lt_e071       LIKE e071,
          lt_e071k      TYPE tyt_e071k,
          ls_log        LIKE LINE OF log.
    MOVE e071[] TO lt_e071[].
    DELETE lt_e071 WHERE pgmid EQ 'CORR'. " no CORR allowed
    CHECK lt_e071[] IS NOT INITIAL. " silently exit
    complete_shi3_entries( CHANGING e071 = lt_e071 e071k = lt_e071k ).
    enqueue( ).
    TRY.
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
            it_e071k             = lt_e071k
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
            sy-msgid = ls_log-ag.
            sy-msgty = 'I'.
            sy-msgno = ls_log-msgnr.
            sy-msgv1 = ls_log-var1.
            sy-msgv2 = ls_log-var2.
            sy-msgv3 = ls_log-var3.
            sy-msgv4 = ls_log-var4.
            /atrm/cx_exception=>raise( ).
          ELSE.
            /atrm/cx_exception=>raise( iv_message = 'Unknown error, check logs.' ). "#EC NOTEXT
          ENDIF.
        ENDIF.
        sort_and_compress( ).
      CATCH /atrm/cx_exception INTO lo_lock_error.
        TRY.
            dequeue( ).
          CATCH /atrm/cx_exception.
        ENDTRY.
        RAISE EXCEPTION lo_lock_error.
    ENDTRY.
    dequeue( ).
    " BRF+ takes its own locks: record FDT0 applications after dequeue
    record_fdt0_entries( lt_e071 ).
  ENDMETHOD.

  METHOD remove_comments.
    DATA lo_lock_error TYPE REF TO /atrm/cx_exception.
    enqueue( ).
    TRY.
        DATA: lt_e071    TYPE STANDARD TABLE OF e071,
              ls_e071    LIKE LINE OF lt_e071,
              ls_request TYPE trwbo_request.
        CALL FUNCTION 'TR_SORT_AND_COMPRESS_COMM'
          EXPORTING
            iv_trkorr                      = gv_trkorr
            iv_dialog                      = ' '
          EXCEPTIONS
            trkorr_not_found               = 1
            order_released                 = 2
            error_while_modifying_obj_list = 3
            tr_enqueue_failed              = 4
            no_authorization               = 5
            OTHERS                         = 6.
        IF sy-subrc <> 0.
          /atrm/cx_exception=>raise( ).
        ENDIF.
        ls_request-h-trkorr = gv_trkorr.
        SELECT * FROM e071 INTO TABLE lt_e071 WHERE pgmid EQ '*' AND object EQ object AND trkorr EQ gv_trkorr.
        IF lt_e071[] IS NOT INITIAL.
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
          COMMIT WORK AND WAIT.
        ENDIF.
      CATCH /atrm/cx_exception INTO lo_lock_error.
        TRY.
            dequeue( ).
          CATCH /atrm/cx_exception.
        ENDTRY.
        RAISE EXCEPTION lo_lock_error.
    ENDTRY.
    dequeue( ).
  ENDMETHOD.

  METHOD create_workbench.
    DATA: lt_users TYPE scts_users,
          ls_user  LIKE LINE OF lt_users.
    "a task of the current user, for objects recorded in the request (e.g. by RS_CORR_INSERT)
    IF with_task = 'X'.
      ls_user-user = sy-uname.
      "development/correction task, unless the objects need another kind (e.g. R repair)
      IF task_type IS INITIAL.
        ls_user-type = 'S'.
      ELSE.
        ls_user-type = task_type.
      ENDIF.
      APPEND ls_user TO lt_users.
    ENDIF.
    create(
      EXPORTING
        text      = text
        target    = target
        type      = 'K'
        users     = lt_users
      RECEIVING
        transport = transport
    ).
  ENDMETHOD.

  METHOD create_customizing.
    DATA: attributes TYPE scts_attrs,
          attribute  LIKE LINE OF attributes.

    CLEAR attribute.
    attribute-attribute = 'SAP_ATO_TRANSPORT_TYPE'.
    attribute-value = 'BC'.
    APPEND attribute TO attributes.

    CLEAR attribute.
    attribute-attribute = 'SAP_CUS_TRANSPORT_CATEGORY'.
    attribute-value = 'MANUAL_CUST'.
    APPEND attribute TO attributes.

    create(
      EXPORTING
        text       = text
        target     = target
        type       = 'W'
        attributes = attributes
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
        it_attributes     = attributes
        it_users          = users
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
    DATA: lo_lock_error TYPE REF TO /atrm/cx_exception,
          ls_request    TYPE trwbo_request.
    enqueue( ).
    TRY.
        " Object locks of the request (e.g. a locked TRM landscape transport) block TR_DELETE_COMM
        CALL FUNCTION 'TR_READ_REQUEST'
          EXPORTING
            iv_trkorr         = gv_trkorr
            iv_read_e070      = 'X'
            iv_read_objs_keys = 'X'
          CHANGING
            cs_request        = ls_request
          EXCEPTIONS
            OTHERS            = 1.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'TRINT_UNLOCK_REQUEST'
            CHANGING
              cs_request = ls_request
            EXCEPTIONS
              OTHERS     = 1.
        ENDIF.
        CALL FUNCTION 'TR_DELETE_COMM'
          EXPORTING
            wi_dialog = ' '
            wi_trkorr = gv_trkorr
          EXCEPTIONS
            OTHERS    = 1.
        IF sy-subrc <> 0.
          /atrm/cx_exception=>raise( ).
        ENDIF.
      CATCH /atrm/cx_exception INTO lo_lock_error.
        TRY.
            dequeue( ).
          CATCH /atrm/cx_exception.
        ENDTRY.
        RAISE EXCEPTION lo_lock_error.
    ENDTRY.
    dequeue( ).
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
    DATA lt_params TYPE abap_func_parmbind_tab.

    CALL METHOD add_fm_value EXPORTING name = 'IV_REQUEST' value = gv_trkorr CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_TARGET' value = target CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_SOURCE' value = source CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_IMPORT_AGAIN' value = import_again CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_MONITOR' value = ' ' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_VERBOSE' value = 'X' CHANGING params = lt_params.

    call_tms(
      funcname       = 'TMS_MGR_FORWARD_TR_REQUEST'
      result_param   = 'ET_TP_FORWARDS'
      params         = lt_params
      not_found_text = 'No forward status found'            "#EC NOTEXT
    ).
  ENDMETHOD.

  METHOD import.
    import = execute_import(
      system    = system
      transport = gv_trkorr
      test      = test
    ).
  ENDMETHOD.

  METHOD read_queue.
    DATA: bufcnt      TYPE tmsbufcnt,
          alog        TYPE tmsalog,
          batch       TYPE sy-batch,
          alert_subrc TYPE sy-subrc,
          subrc       TYPE sy-subrc,
          msgid       TYPE sy-msgid,
          msgno       TYPE sy-msgno,
          msgty       TYPE sy-msgty,
          msgv1       TYPE sy-msgv1,
          msgv2       TYPE sy-msgv2,
          msgv3       TYPE sy-msgv3,
          msgv4       TYPE sy-msgv4.

    batch = sy-batch.
    sy-batch = 'X'.
    CALL FUNCTION 'TMS_UIQ_IQD_READ_QUEUE'
      EXPORTING
        iv_system         = target
        iv_collect        = 'X'
        iv_read_shadow    = 'X'
        iv_monitor        = ' '
        iv_verbose        = 'X'
      IMPORTING
        et_requests       = requests
        es_bufcnt         = bufcnt
      EXCEPTIONS
        read_queue_failed = 1
        error_message     = 2
        OTHERS            = 3.

    subrc = sy-subrc.
    msgid = sy-msgid.
    msgno = sy-msgno.
    msgty = sy-msgty.
    msgv1 = sy-msgv1.
    msgv2 = sy-msgv2.
    msgv3 = sy-msgv3.
    msgv4 = sy-msgv4.
    sy-batch = batch.

    IF bufcnt-alertid IS NOT INITIAL.
      CALL FUNCTION 'TMS_ALT_ANALYSE_ALERT'
        EXPORTING
          iv_alert_id   = bufcnt-alertid
          iv_no_display = 'X'
        IMPORTING
          es_alog       = alog
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.
      alert_subrc = sy-subrc.
      IF alert_subrc = 0
        AND ( alog-msgty = 'E' OR alog-msgty = 'A' ).
        sy-msgid = alog-msgid.
        sy-msgno = alog-msgno.
        sy-msgty = alog-msgty.
        sy-msgv1 = alog-msgv1.
        sy-msgv2 = alog-msgv2.
        sy-msgv3 = alog-msgv3.
        sy-msgv4 = alog-msgv4.
        /atrm/cx_exception=>raise(
          iv_reason = /atrm/cx_exception=>c_reason-tms_alert
        ).
      ENDIF.
    ENDIF.

    IF subrc <> 0.
      sy-msgid = msgid.
      sy-msgno = msgno.
      sy-msgty = msgty.
      sy-msgv1 = msgv1.
      sy-msgv2 = msgv2.
      sy-msgv3 = msgv3.
      sy-msgv4 = msgv4.
      /atrm/cx_exception=>raise( ).
    ENDIF.

    IF alert_subrc <> 0.
      /atrm/cx_exception=>raise(
        iv_reason = /atrm/cx_exception=>c_reason-tms_alert
      ).
    ENDIF.
  ENDMETHOD.

  METHOD release.
    DATA lo_lock_error TYPE REF TO /atrm/cx_exception.
    enqueue( ).
    TRY.
        DATA lv_without_lock TYPE flag.
        IF lock EQ 'X'.
          lv_without_lock = ' '.
        ELSE.
          lv_without_lock = 'X'.
        ENDIF.

        " IV_IGNORE_WARNINGS does not exist on old releases (e.g. 7.00): passing it dumps
        IF fm_has_parameter( funcname = 'TRINT_RELEASE_REQUEST' parameter = 'IV_IGNORE_WARNINGS' ) = 'X'.
          CALL FUNCTION 'TRINT_RELEASE_REQUEST'
            EXPORTING
              iv_trkorr                = gv_trkorr
              iv_dialog                = ' '
              iv_success_message       = ' '
              iv_display_export_log    = ' '
              iv_without_objects_check = 'X'
              iv_without_locking       = lv_without_lock
              " objects deleted by an already released request have no TADIR entry:
              " skip their lock (TK313) instead of failing the release with TR776
              iv_ignore_warnings       = lock
            EXCEPTIONS
              OTHERS                   = 1.
        ELSE.
          " Old releases cannot skip TK313 while locking, so a request holding objects
          " deleted by an already released request fails with TR776 (OBJECT_LOCK_ERROR).
          " The lock pass has already locked and committed every other object by then:
          " when TK313 is the only lock error, release again without locking, which only
          " skips the objects without TADIR entry - the same outcome as IV_IGNORE_WARNINGS
          DATA: lt_messages   TYPE ctsgerrmsgs,
                ls_message    TYPE ctsgerrmsg,
                lv_subrc      TYPE sy-subrc,
                lv_only_tk313 TYPE flag.
          CALL FUNCTION 'TRINT_RELEASE_REQUEST'
            EXPORTING
              iv_trkorr                = gv_trkorr
              iv_dialog                = ' '
              iv_success_message       = ' '
              iv_display_export_log    = ' '
              iv_without_objects_check = 'X'
              iv_without_locking       = lv_without_lock
            IMPORTING
              et_messages              = lt_messages
            EXCEPTIONS
              object_lock_error        = 1
              OTHERS                   = 2.
          lv_subrc = sy-subrc.
          IF lv_subrc = 1 AND lock = 'X'.
            LOOP AT lt_messages INTO ls_message WHERE msgty CA 'EAX'.
              IF ls_message-msgid = 'TK' AND ls_message-msgno = '313'.
                lv_only_tk313 = 'X'.
              ELSE.
                CLEAR lv_only_tk313.
                EXIT.
              ENDIF.
            ENDLOOP.
            IF lv_only_tk313 = 'X'.
              CALL FUNCTION 'TRINT_RELEASE_REQUEST'
                EXPORTING
                  iv_trkorr                = gv_trkorr
                  iv_dialog                = ' '
                  iv_success_message       = ' '
                  iv_display_export_log    = ' '
                  iv_without_objects_check = 'X'
                  iv_without_locking       = 'X'
                EXCEPTIONS
                  OTHERS                   = 1.
              lv_subrc = sy-subrc.
            ENDIF.
          ENDIF.
          sy-subrc = lv_subrc.
        ENDIF.
        IF sy-subrc <> 0.
          /atrm/cx_exception=>raise( ).
        ENDIF.
      CATCH /atrm/cx_exception INTO lo_lock_error.
        TRY.
            dequeue( ).
          CATCH /atrm/cx_exception.
        ENDTRY.
        RAISE EXCEPTION lo_lock_error.
    ENDTRY.
    dequeue( ).
    " the release is completed asynchronously by the update task (versioning):
    " a failure there leaves the request modifiable and its locks held, so
    " don't report success before it is done (the export is not awaited)
    wait_release( exported = abap_false ).
  ENDMETHOD.

  METHOD rename.
    DATA lo_lock_error TYPE REF TO /atrm/cx_exception.
    enqueue( ).
    TRY.
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
      CATCH /atrm/cx_exception INTO lo_lock_error.
        TRY.
            dequeue( ).
          CATCH /atrm/cx_exception.
        ENDTRY.
        RAISE EXCEPTION lo_lock_error.
    ENDTRY.
    dequeue( ).
  ENDMETHOD.

  METHOD set_documentation.
    DATA lo_lock_error TYPE REF TO /atrm/cx_exception.
    enqueue( ).
    TRY.
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
      CATCH /atrm/cx_exception INTO lo_lock_error.
        TRY.
            dequeue( ).
          CATCH /atrm/cx_exception.
        ENDTRY.
        RAISE EXCEPTION lo_lock_error.
    ENDTRY.
    dequeue( ).
  ENDMETHOD.

  METHOD copy.
    DATA lo_lock_error TYPE REF TO /atrm/cx_exception.
    enqueue( ).
    TRY.
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
        sort_and_compress( ).
      CATCH /atrm/cx_exception INTO lo_lock_error.
        TRY.
            dequeue( ).
          CATCH /atrm/cx_exception.
        ENDTRY.
        RAISE EXCEPTION lo_lock_error.
    ENDTRY.
    dequeue( ).
  ENDMETHOD.

  METHOD delete_from_tms_queue.
    DATA: tmsbuffer  TYPE tmsbuffer,
          lt_params  TYPE abap_func_parmbind_tab,
          " typed like IV_COMMAND: 7.00 rejects a shorter literal in a dynamic call
          lv_command TYPE stpa-command VALUE 'DELFROMBUFFER'.

    SELECT SINGLE domnam sysnam trkorr tarcli FROM tmsbuffer
      INTO CORRESPONDING FIELDS OF tmsbuffer
      WHERE sysnam EQ system
        AND trkorr EQ gv_trkorr.
    CHECK sy-subrc EQ 0.

    CALL METHOD add_fm_value EXPORTING name = 'IV_COMMAND' value = lv_command CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_SYSTEM' value = tmsbuffer-sysnam CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_DOMAIN' value = tmsbuffer-domnam CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_REQUEST' value = tmsbuffer-trkorr CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_TARCLI' value = tmsbuffer-tarcli CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_MONITOR' value = ' ' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_VERBOSE' value = 'X' CHANGING params = lt_params.

    call_tms(
      funcname       = 'TMS_MGR_MAINTAIN_TR_QUEUE'
      result_param   = 'ET_TP_MAINTAINS'
      params         = lt_params
      not_found_text = 'No queue maintenance status found'  "#EC NOTEXT
    ).
  ENDMETHOD.

  METHOD refresh_tms_txt.
    DATA ls_tmsbuftxt TYPE tmsbuftxt.
    SELECT SINGLE * FROM tmsbuftxt INTO ls_tmsbuftxt WHERE trkorr EQ gv_trkorr.
    CHECK sy-subrc EQ 0.
    SELECT SINGLE as4text FROM e07t INTO ls_tmsbuftxt-text
      WHERE trkorr EQ gv_trkorr
        AND langu EQ sy-langu.
    SELECT SINGLE as4user FROM e070 INTO ls_tmsbuftxt-owner WHERE trkorr EQ gv_trkorr.
    SELECT SINGLE client FROM e070c INTO ls_tmsbuftxt-srccli
      WHERE trkorr EQ gv_trkorr.
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
    DATA lo_lock_error TYPE REF TO /atrm/cx_exception.
    enqueue( ).
    TRY.
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
      CATCH /atrm/cx_exception INTO lo_lock_error.
        TRY.
            dequeue( ).
          CATCH /atrm/cx_exception.
        ENDTRY.
        RAISE EXCEPTION lo_lock_error.
    ENDTRY.
    dequeue( ).
  ENDMETHOD.

  METHOD get_import_status.
    DATA: ls_tmsbuffer TYPE tmsbuffer,
          lt_stats     TYPE tpstats.

    SELECT SINGLE * FROM tmsbuffer INTO ls_tmsbuffer
      WHERE sysnam EQ system
        AND trkorr EQ gv_trkorr.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise(
        iv_message = 'Transport was not found in the target system buffer' "#EC NOTEXT
        iv_reason  = /atrm/cx_exception=>c_reason-not_found
      ).
    ENDIF.
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
        iv_message = 'No import status found for this transport in the target system' "#EC NOTEXT
        iv_reason = /atrm/cx_exception=>c_reason-not_found
      ).
    ENDIF.
    SORT lt_stats BY timestamp DESCENDING.
    " In a batch import (IMPORT SUBSET) every request shares the same timestamp:
    " return the latest row of the requested transport, else the latest row.
    READ TABLE lt_stats INTO stat WITH KEY trkorr = gv_trkorr.
    IF sy-subrc <> 0.
      READ TABLE lt_stats INTO stat INDEX 1.
    ENDIF.
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
      WHERE sysnam EQ consolidations-table_line AND systyp EQ 'V'.
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

  METHOD import_multiple.
    DATA: lt_transports TYPE /atrm/trkorr_t,
          trkorr        TYPE trkorr.

    IF transports[] IS INITIAL.
      /atrm/cx_exception=>raise(
        iv_message = 'No transports supplied for batch import' "#EC NOTEXT
        iv_reason  = /atrm/cx_exception=>c_reason-invalid_input
      ).
    ENDIF.

    LOOP AT transports INTO trkorr.
      CHECK trkorr IS NOT INITIAL.
      READ TABLE lt_transports TRANSPORTING NO FIELDS
        WITH KEY table_line = trkorr.
      CHECK sy-subrc <> 0.
      APPEND trkorr TO lt_transports.
    ENDLOOP.

    IF lt_transports[] IS INITIAL.
      /atrm/cx_exception=>raise(
        iv_message = 'No valid transports supplied for batch import' "#EC NOTEXT
        iv_reason  = /atrm/cx_exception=>c_reason-invalid_input
      ).
    ENDIF.

    import = execute_import(
      system     = system
      transport  = 'SOME'
      test       = test
      transports = lt_transports
    ).
  ENDMETHOD.


  METHOD execute_import.
    DATA: subset      TYPE flag,
          offline     TYPE flag,
          trkorr      TYPE trkorr,
          lr_requests TYPE REF TO data,
          lr_request  TYPE REF TO data,
          lt_params   TYPE abap_func_parmbind_tab.
    FIELD-SYMBOLS: <lt_requests> TYPE STANDARD TABLE,
                   <ls_request>  TYPE any,
                   <lv_field>    TYPE any.

    IF transport = 'SOME'.
      subset = 'X'.
    ENDIF.
    IF test IS INITIAL.
      offline = 'X'.
    ENDIF.

    " Parameters are bound dynamically because the signature differs by release:
    " IV_IGNORE_CVERS does not exist on 7.00, and a static call passing it dumps
    CALL METHOD add_fm_value EXPORTING name = 'IV_SYSTEM' value = system CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_REQUEST' value = transport CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_CLIENT' value = sy-mandt CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_CTC_ACTIVE' value = ' ' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_OVERTAKE' value = ' ' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_IMPORT_AGAIN' value = 'X' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_IGNORE_ORIGINALITY' value = 'X' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_IGNORE_REPAIRS' value = ' ' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_IGNORE_TRANSTYPE' value = ' ' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_IGNORE_TABLETYPE' value = ' ' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_IGNORE_PREDEC' value = 'X' CHANGING params = lt_params.
    IF fm_has_parameter( funcname = 'TMS_MGR_IMPORT_TR_REQUEST' parameter = 'IV_IGNORE_CVERS' ) = 'X'.
      CALL METHOD add_fm_value EXPORTING name = 'IV_IGNORE_CVERS' value = 'X' CHANGING params = lt_params.
    ENDIF.
    CALL METHOD add_fm_value EXPORTING name = 'IV_TEST_IMPORT' value = test CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_SUBSET' value = subset CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_OFFLINE' value = offline CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_MONITOR' value = ' ' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_VERBOSE' value = 'X' CHANGING params = lt_params.

    " IT_REQUESTS is typed with type group STMS: build it with the type the
    " function module declares on this release
    IF transports[] IS NOT INITIAL.
      lr_requests = create_fm_param_data( funcname = 'TMS_MGR_IMPORT_TR_REQUEST' parameter = 'IT_REQUESTS' ).
      IF lr_requests IS NOT BOUND.
        /atrm/cx_exception=>raise(
          iv_message = 'Batch import is not supported on this release' "#EC NOTEXT
          iv_reason  = /atrm/cx_exception=>c_reason-generic
        ).
      ENDIF.
      ASSIGN lr_requests->* TO <lt_requests>.
      CREATE DATA lr_request LIKE LINE OF <lt_requests>.
      ASSIGN lr_request->* TO <ls_request>.
      LOOP AT transports INTO trkorr.
        CLEAR <ls_request>.
        ASSIGN COMPONENT 'TRKORR' OF STRUCTURE <ls_request> TO <lv_field>.
        IF sy-subrc = 0.
          <lv_field> = trkorr.
        ENDIF.
        ASSIGN COMPONENT 'TARCLI' OF STRUCTURE <ls_request> TO <lv_field>.
        IF sy-subrc = 0.
          <lv_field> = sy-mandt.
        ENDIF.
        APPEND <ls_request> TO <lt_requests>.
      ENDLOOP.
      CALL METHOD add_fm_value EXPORTING name = 'IT_REQUESTS' value = <lt_requests> CHANGING params = lt_params.
    ENDIF.

    import = call_tms(
      funcname       = 'TMS_MGR_IMPORT_TR_REQUEST'
      result_param   = 'ET_TP_IMPORTS'
      params         = lt_params
      not_found_text = 'No import status found'             "#EC NOTEXT
    ).
  ENDMETHOD.

  METHOD sort_and_compress.
    CALL FUNCTION 'TRINT_SORT_AND_COMPRESS_COMM'
      EXPORTING
        iv_trkorr           = gv_trkorr
        iv_dialog           = ' '
        iv_called_by_editor = ' '
      EXCEPTIONS
        error_message       = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD lock.
    DATA: error    TYPE trboolean,
          messages TYPE ctsgerrmsgs,
          message  LIKE LINE OF messages,
          subrc    TYPE sy-subrc,
          msgid    TYPE sy-msgid,
          msgno    TYPE sy-msgno,
          msgty    TYPE sy-msgty,
          msgv1    TYPE sy-msgv1,
          msgv2    TYPE sy-msgv2,
          msgv3    TYPE sy-msgv3,
          msgv4    TYPE sy-msgv4.
    CALL FUNCTION 'TR_LOCK_REQUEST'
      EXPORTING
        iv_trkorr          = gv_trkorr
        iv_dialog          = ' '
        iv_success_message = ' '
      IMPORTING
        ev_object_error    = error
        et_messages        = messages
      EXCEPTIONS
        error_message      = 1
        OTHERS             = 2.
    subrc = sy-subrc.
    msgty = sy-msgty.
    msgid = sy-msgid.
    msgno = sy-msgno.
    msgv1 = sy-msgv1.
    msgv2 = sy-msgv2.
    msgv3 = sy-msgv3.
    msgv4 = sy-msgv4.

    READ TABLE messages INTO message WITH KEY msgty = 'E'.
    IF sy-subrc <> 0.
      READ TABLE messages INTO message WITH KEY msgty = 'A'.
    ENDIF.
    IF message-msgid IS NOT INITIAL AND message-msgno IS NOT INITIAL.
      sy-msgid = message-msgid.
      sy-msgty = message-msgty.
      sy-msgno = message-msgno.
      sy-msgv1 = message-msgv1.
      sy-msgv2 = message-msgv2.
      sy-msgv3 = message-msgv3.
      sy-msgv4 = message-msgv4.
      /atrm/cx_exception=>raise( ).
    ENDIF.

    IF subrc <> 0.
      sy-msgid = msgid.
      sy-msgty = msgty.
      sy-msgno = msgno.
      sy-msgv1 = msgv1.
      sy-msgv2 = msgv2.
      sy-msgv3 = msgv3.
      sy-msgv4 = msgv4.
      /atrm/cx_exception=>raise( ).
    ENDIF.

    IF error EQ 'X'.
      /atrm/cx_exception=>raise(
        iv_message = 'Generic error return from lock function.' "#EC NOTEXT
        iv_reason  = /atrm/cx_exception=>c_reason-generic
      ).
    ENDIF.
  ENDMETHOD.

  METHOD get_entries.
    DATA lt_packages TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY.
    DATA lt_keys TYPE scts_tadir.
    DATA ls_key LIKE LINE OF lt_keys.
    FIELD-SYMBOLS <ls_e071> TYPE e071.

    e071 = get_e071( ).

    LOOP AT e071 ASSIGNING <ls_e071> WHERE pgmid = 'R3TR'.
      CLEAR ls_key.
      ls_key-pgmid = <ls_e071>-pgmid.
      ls_key-object = <ls_e071>-object.
      ls_key-obj_name = <ls_e071>-obj_name.
      APPEND ls_key TO lt_keys.
      IF <ls_e071>-object = 'DEVC'.
        APPEND <ls_e071>-obj_name TO lt_packages.
      ENDIF.
    ENDLOOP.

    IF lt_keys IS NOT INITIAL.
      SELECT * FROM tadir
        INTO CORRESPONDING FIELDS OF TABLE tadir
        FOR ALL ENTRIES IN lt_keys
        WHERE pgmid = lt_keys-pgmid
          AND object = lt_keys-object
          AND obj_name = lt_keys-obj_name.
    ENDIF.

    IF lt_packages IS NOT INITIAL.
      SORT lt_packages.
      DELETE ADJACENT DUPLICATES FROM lt_packages.
      SELECT * FROM tdevc
        INTO CORRESPONDING FIELDS OF TABLE tdevc
        FOR ALL ENTRIES IN lt_packages
        WHERE devclass = lt_packages-table_line.
      SELECT * FROM tdevct
        INTO CORRESPONDING FIELDS OF TABLE tdevct
        FOR ALL ENTRIES IN lt_packages
        WHERE devclass = lt_packages-table_line.
    ENDIF.
  ENDMETHOD.
  METHOD complete_shi3_entries.
    DATA: lt_shi3    TYPE tyt_e071,
          ls_e071    TYPE e071,
          lv_id      TYPE ttree-id,
          ls_message TYPE hier_mess,
          lt_objects TYPE tyt_e071,
          lt_keys    TYPE tyt_e071k.
    " STREE_BEFORE_EXPORT fails the release when a hierarchy header is recorded
    " without its nodes (R3TR TABU keys): build the complete piece list, as SE43 does
    lt_shi3 = e071.
    DELETE lt_shi3 WHERE pgmid <> 'R3TR' OR object <> 'SHI3'.
    CHECK lt_shi3 IS NOT INITIAL.
    LOOP AT lt_shi3 INTO ls_e071.
      lv_id = ls_e071-obj_name.
      CLEAR: ls_message, lt_objects, lt_keys.
      CALL FUNCTION 'STREE_INSERT_ALL_IN_TRANSPORT'
        EXPORTING
          structure_id               = lv_id
          iv_return_objects_and_keys = 'X'
        IMPORTING
          message                    = ls_message
        TABLES
          et_objects                 = lt_objects
          et_keys                    = lt_keys
        EXCEPTIONS
          error_message              = 1
          OTHERS                     = 2.
      IF sy-subrc <> 0.
        /atrm/cx_exception=>raise( ).
      ENDIF.
      IF ls_message-msgid IS NOT INITIAL.
        sy-msgid = ls_message-msgid.
        sy-msgty = 'I'.
        sy-msgno = ls_message-msgno.
        sy-msgv1 = ls_message-msgv1.
        sy-msgv2 = ls_message-msgv2.
        sy-msgv3 = ls_message-msgv3.
        sy-msgv4 = ls_message-msgv4.
        /atrm/cx_exception=>raise( ).
      ENDIF.
      APPEND LINES OF lt_objects TO e071.
      APPEND LINES OF lt_keys TO e071k.
    ENDLOOP.
    SORT e071 BY pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM e071 COMPARING pgmid object obj_name.
    SORT e071k BY pgmid object objname mastertype mastername tabkey.
    DELETE ADJACENT DUPLICATES FROM e071k COMPARING pgmid object objname mastertype mastername tabkey.
  ENDMETHOD.
  METHOD record_fdt0_entries.
    DATA: ls_e071     TYPE e071,
          lr_row      TYPE REF TO data,
          lr_instance TYPE REF TO data,
          lo_instance TYPE REF TO object,
          lv_name     TYPE sobj_name,
          lv_where    TYPE string,
          lx_root     TYPE REF TO cx_root.
    FIELD-SYMBOLS: <ls_row>      TYPE any,
                   <lv_id>       TYPE any,
                   <lo_instance> TYPE any.
    " FDT0_BEFORE_EXPORT fails the release when the application is not also
    " recorded under FDT0001/FDT0002: let BRF+ record it.
    " BRF+ may not exist in the system, so everything is dynamic
    LOOP AT e071 INTO ls_e071 WHERE pgmid = 'R3TR' AND object = 'FDT0'.
      TRY.
          CREATE DATA lr_row TYPE ('FDT_APPL_TADIR').
          CREATE DATA lr_instance TYPE REF TO ('IF_FDT_ADMIN_DATA').
        CATCH cx_sy_create_data_error.
          RETURN. " BRF+ not available
      ENDTRY.
      ASSIGN lr_row->* TO <ls_row>.
      ASSIGN lr_instance->* TO <lo_instance>.
      lv_name = ls_e071-obj_name.
      REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
      CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
      SELECT SINGLE * FROM ('FDT_APPL_TADIR') INTO <ls_row> WHERE (lv_where).
      CHECK sy-subrc = 0.
      ASSIGN COMPONENT 'ID' OF STRUCTURE <ls_row> TO <lv_id>.
      CHECK sy-subrc = 0.
      TRY.
          CALL METHOD ('CL_FDT_FACTORY')=>('GET_INSTANCE_GENERIC')
            EXPORTING
              iv_id       = <lv_id>
            IMPORTING
              eo_instance = <lo_instance>.
          lo_instance = <lo_instance>.
          CALL METHOD lo_instance->('IF_FDT_TRANSACTION~TRANSPORT')
            EXPORTING
              iv_deep              = abap_true
              iv_transport_request = gv_trkorr.
        CATCH cx_root INTO lx_root.
          /atrm/cx_exception=>raise( io_root = lx_root ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
  METHOD wait_release.
    DATA: lv_waited  TYPE i,
          lv_message TYPE string.
    DO.
      SELECT SINGLE trstatus FROM e070 INTO status WHERE trkorr = gv_trkorr.
      IF sy-subrc <> 0.
        /atrm/cx_exception=>raise( iv_message = 'Transport not found' "#EC NOTEXT
                                   iv_reason  = /atrm/cx_exception=>c_reason-not_found ).
      ENDIF.
      IF status = 'R' OR status = 'N'.
        RETURN.
      ENDIF.
      " O: the update task is done, tp export still running
      IF status = 'O' AND exported = abap_false.
        RETURN.
      ENDIF.
      IF lv_waited >= timeout.
        EXIT.
      ENDIF.
      " WAIT also ends the DB LUW, so the next SELECT sees the update task's commit
      WAIT UP TO 5 SECONDS.
      lv_waited = lv_waited + 5.
    ENDDO.
    IF status = 'O'.
      lv_message = 'Release started but export not finished, check tp logs'. "#EC NOTEXT
    ELSE.
      lv_message = 'Release not completed, check update task (SM13)'. "#EC NOTEXT
    ENDIF.
    /atrm/cx_exception=>raise( iv_message = lv_message
                               iv_reason  = /atrm/cx_exception=>c_reason-generic ).
  ENDMETHOD.

  METHOD get_transport_layers.
    DATA: version TYPE tceversion.
    IF target IS INITIAL.
      /atrm/cx_exception=>raise( iv_message = 'No transport target supplied' "#EC NOTEXT
                                iv_reason  = /atrm/cx_exception=>c_reason-invalid_input ).
    ENDIF.
    SELECT SINGLE version FROM tcevers INTO version WHERE active EQ 'A'.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( iv_message = 'Transport configuration has no active version' ). "#EC NOTEXT
    ENDIF.
    SELECT DISTINCT translayer
      FROM tcerele
      INTO TABLE layers
      WHERE version EQ version AND intsys EQ sy-sysid AND consys EQ target.
    SORT layers.
  ENDMETHOD.

  METHOD delete_entries.
    DATA: lt_requests TYPE trwbo_requests,
          ls_request  TYPE trwbo_request,
          lt_e071     TYPE trwbo_t_e071,
          ls_e071     TYPE e071,
          lx_first    TYPE REF TO /atrm/cx_exception,
          lx_entry    TYPE REF TO /atrm/cx_exception.

    "the request and its tasks, with their entries
    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING
        iv_trkorr   = gv_trkorr
      IMPORTING
        et_requests = lt_requests
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.

    "every entry is attempted, the first failure is raised afterwards
    LOOP AT lt_requests INTO ls_request.
      lt_e071 = ls_request-objects.
      LOOP AT lt_e071 INTO ls_e071.
        TRY.
            CALL FUNCTION 'TR_DELETE_COMM_OBJECT_KEYS'
              EXPORTING
                is_e071_delete = ls_e071
                iv_dialog_flag = ' '
              CHANGING
                cs_request     = ls_request
              EXCEPTIONS
                OTHERS         = 1.
            IF sy-subrc <> 0.
              /atrm/cx_exception=>raise( ).
            ENDIF.
          CATCH /atrm/cx_exception INTO lx_entry.
            IF lx_first IS NOT BOUND.
              lx_first = lx_entry.
            ENDIF.
        ENDTRY.
      ENDLOOP.
    ENDLOOP.

    IF lx_first IS BOUND.
      RAISE EXCEPTION lx_first.
    ENDIF.
  ENDMETHOD.

  METHOD add_fm_value.
    " Bind a by-value input of a dynamic function call (copied, so literals are safe)
    CONSTANTS lc_kind_exporting TYPE i VALUE 10. " = abap_func_exporting
    DATA ls_param TYPE abap_func_parmbind.
    FIELD-SYMBOLS <value> TYPE any.
    ls_param-name = name.
    ls_param-kind = lc_kind_exporting.
    CREATE DATA ls_param-value LIKE value.
    ASSIGN ls_param-value->* TO <value>.
    <value> = value.
    INSERT ls_param INTO TABLE params.
  ENDMETHOD.

  METHOD fm_has_parameter.
    DATA lv_parameter TYPE fupararef-parameter.
    SELECT SINGLE parameter FROM fupararef INTO lv_parameter
      WHERE funcname  = funcname
        AND r3state   = 'A'
        AND parameter = parameter.
    IF sy-subrc = 0.
      found = 'X'.
    ENDIF.
  ENDMETHOD.

  METHOD create_fm_param_data.
    " Data object typed like a function module parameter, or unbound when the
    " parameter (or its type) does not exist on this release. Type group types
    " (e.g. STMS_TP_IMPORTS) are resolved by absolute name.
    DATA: lv_type     TYPE fupararef-structure,
          lv_group    TYPE string,
          lv_rest     TYPE string,
          lv_absolute TYPE string.

    SELECT SINGLE structure FROM fupararef INTO lv_type
      WHERE funcname  = funcname
        AND r3state   = 'A'
        AND parameter = parameter.
    IF sy-subrc <> 0 OR lv_type IS INITIAL.
      RETURN.
    ENDIF.
    TRY.
        CREATE DATA data TYPE (lv_type).
      CATCH cx_sy_create_data_error.
        SPLIT lv_type AT '_' INTO lv_group lv_rest.
        CONCATENATE '\TYPE-POOL=' lv_group '\TYPE=' lv_type INTO lv_absolute.
        TRY.
            CREATE DATA data TYPE (lv_absolute).
          CATCH cx_sy_create_data_error.
            CLEAR data.
        ENDTRY.
    ENDTRY.
  ENDMETHOD.

  METHOD copy_tp_result.
    " Copy one row of a STMS_TP_* table (type group STMS) by component name
    DATA: ls_stdout TYPE tpstdout,
          ls_logptr TYPE tplogptr.
    FIELD-SYMBOLS: <lv_value> TYPE any,
                   <lt_rows>  TYPE ANY TABLE,
                   <ls_row>   TYPE any.

    ASSIGN COMPONENT 'REQUEST' OF STRUCTURE source TO <lv_value>.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING <lv_value> TO result-request.
    ENDIF.
    ASSIGN COMPONENT 'ALERT' OF STRUCTURE source TO <lv_value>.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING <lv_value> TO result-alert.
    ENDIF.
    ASSIGN COMPONENT 'TP_RET_CODE' OF STRUCTURE source TO <lv_value>.
    IF sy-subrc = 0.
      result-tp_ret_code = <lv_value>.
    ENDIF.
    ASSIGN COMPONENT 'TP_ALOG' OF STRUCTURE source TO <lv_value>.
    IF sy-subrc = 0.
      result-tp_alog = <lv_value>.
    ENDIF.
    ASSIGN COMPONENT 'TP_SLOG' OF STRUCTURE source TO <lv_value>.
    IF sy-subrc = 0.
      result-tp_slog = <lv_value>.
    ENDIF.
    ASSIGN COMPONENT 'TP_PID' OF STRUCTURE source TO <lv_value>.
    IF sy-subrc = 0.
      result-tp_pid = <lv_value>.
    ENDIF.
    ASSIGN COMPONENT 'TP_STDOUT' OF STRUCTURE source TO <lt_rows>.
    IF sy-subrc = 0.
      LOOP AT <lt_rows> ASSIGNING <ls_row>.
        CLEAR ls_stdout.
        MOVE-CORRESPONDING <ls_row> TO ls_stdout.
        APPEND ls_stdout TO result-tp_stdout.
      ENDLOOP.
    ENDIF.
    ASSIGN COMPONENT 'TP_LOGPTR' OF STRUCTURE source TO <lt_rows>.
    IF sy-subrc = 0.
      LOOP AT <lt_rows> ASSIGNING <ls_row>.
        CLEAR ls_logptr.
        MOVE-CORRESPONDING <ls_row> TO ls_logptr.
        APPEND ls_logptr TO result-tp_logptr.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD bind_fm_result.
    " Bind an output of a dynamic function call, only if the function has it
    DATA ls_param TYPE abap_func_parmbind.
    DATA lv_parameter TYPE rs38l_par_.
    lv_parameter = name.
    IF fm_has_parameter( funcname = funcname parameter = lv_parameter ) IS INITIAL.
      RETURN.
    ENDIF.
    ls_param-name = name.
    ls_param-kind = kind.
    GET REFERENCE OF value INTO ls_param-value.
    INSERT ls_param INTO TABLE params.
  ENDMETHOD.

  METHOD call_tms.
    " Call a TMS_MGR_* function and evaluate its tp result. The ET_TP_* result
    " tables are typed with type group STMS and are missing on old releases
    " (SAP_BASIS 700): there the classic single-request outputs are read.
    CONSTANTS: lc_kind_importing TYPE i VALUE 20, " = abap_func_importing
               lc_kind_tables    TYPE i VALUE 30. " = abap_func_tables
    DATA: lt_params   TYPE abap_func_parmbind_tab,
          ls_param    TYPE abap_func_parmbind,
          lt_excps    TYPE abap_func_excpbind_tab,
          ls_excp     TYPE abap_func_excpbind,
          lr_results  TYPE REF TO data,
          lv_funcname TYPE rs38l_fnam,
          stdout      TYPE /atrm/cx_exception=>tyt_log,
          tpstdout    TYPE tpstdout,
          batch       TYPE sy-batch,
          found       TYPE flag,
          subrc       TYPE sy-subrc,
          msgid       TYPE sy-msgid,
          msgno       TYPE sy-msgno,
          msgty       TYPE sy-msgty,
          msgv1       TYPE sy-msgv1,
          msgv2       TYPE sy-msgv2,
          msgv3       TYPE sy-msgv3,
          msgv4       TYPE sy-msgv4.
    FIELD-SYMBOLS: <lt_results> TYPE ANY TABLE,
                   <ls_result>  TYPE any.

    lt_params = params.
    lv_funcname = funcname.
    lr_results = create_fm_param_data( funcname = lv_funcname parameter = result_param ).
    IF lr_results IS BOUND.
      ls_param-name = result_param.
      ls_param-kind = lc_kind_importing.
      ls_param-value = lr_results.
      INSERT ls_param INTO TABLE lt_params.
    ELSE.
      CALL METHOD bind_fm_result
        EXPORTING funcname = lv_funcname name = 'EV_TP_RET_CODE' kind = lc_kind_importing
        CHANGING  value = result-tp_ret_code params = lt_params.
      CALL METHOD bind_fm_result
        EXPORTING funcname = lv_funcname name = 'EV_TP_ALOG' kind = lc_kind_importing
        CHANGING  value = result-tp_alog params = lt_params.
      CALL METHOD bind_fm_result
        EXPORTING funcname = lv_funcname name = 'EV_TP_SLOG' kind = lc_kind_importing
        CHANGING  value = result-tp_slog params = lt_params.
      CALL METHOD bind_fm_result
        EXPORTING funcname = lv_funcname name = 'EV_TP_PID' kind = lc_kind_importing
        CHANGING  value = result-tp_pid params = lt_params.
      CALL METHOD bind_fm_result
        EXPORTING funcname = lv_funcname name = 'ES_EXCEPTION' kind = lc_kind_importing
        CHANGING  value = result-alert params = lt_params.
      CALL METHOD bind_fm_result
        EXPORTING funcname = lv_funcname name = 'TT_STDOUT' kind = lc_kind_tables
        CHANGING  value = result-tp_stdout params = lt_params.
      CALL METHOD bind_fm_result
        EXPORTING funcname = lv_funcname name = 'TT_LOGPTR' kind = lc_kind_tables
        CHANGING  value = result-tp_logptr params = lt_params.
    ENDIF.

    ls_excp-name = 'READ_CONFIG_FAILED'.
    ls_excp-value = 1.
    INSERT ls_excp INTO TABLE lt_excps.
    ls_excp-name = 'TABLE_OF_REQUESTS_IS_EMPTY'.
    ls_excp-value = 2.
    INSERT ls_excp INTO TABLE lt_excps.
    ls_excp-name = 'ERROR_MESSAGE'.
    ls_excp-value = 3.
    INSERT ls_excp INTO TABLE lt_excps.
    ls_excp-name = 'OTHERS'.
    ls_excp-value = 4.
    INSERT ls_excp INTO TABLE lt_excps.

    batch = sy-batch.
    sy-batch = 'X'.
    CALL FUNCTION lv_funcname
      PARAMETER-TABLE lt_params
      EXCEPTION-TABLE lt_excps.

    subrc = sy-subrc.
    msgty = sy-msgty.
    msgid = sy-msgid.
    msgno = sy-msgno.
    msgv1 = sy-msgv1.
    msgv2 = sy-msgv2.
    msgv3 = sy-msgv3.
    msgv4 = sy-msgv4.
    sy-batch = batch.

    IF lr_results IS BOUND.
      ASSIGN lr_results->* TO <lt_results>.
      LOOP AT <lt_results> ASSIGNING <ls_result>.
        found = 'X'.
        CALL METHOD copy_tp_result
          EXPORTING source = <ls_result>
          CHANGING  result = result.
        EXIT.
      ENDLOOP.
    ELSEIF subrc = 0.
      found = 'X'.
    ENDIF.

    IF found = 'X'.
      LOOP AT result-tp_stdout INTO tpstdout.
        APPEND tpstdout-line TO stdout.
      ENDLOOP.
      IF result-alert-severity = 'E'
        OR result-alert-msgty = 'E'
        OR result-alert-msgty = 'A'.
        IF result-alert-msgid IS NOT INITIAL
          AND result-alert-msgno IS NOT INITIAL.
          sy-msgid = result-alert-msgid.
          sy-msgty = result-alert-msgty.
          sy-msgno = result-alert-msgno.
          sy-msgv1 = result-alert-msgv1.
          sy-msgv2 = result-alert-msgv2.
          sy-msgv3 = result-alert-msgv3.
          sy-msgv4 = result-alert-msgv4.
        ENDIF.
        /atrm/cx_exception=>raise( it_log = stdout ).
      ENDIF.
    ENDIF.

    IF subrc <> 0.
      sy-msgid = msgid.
      sy-msgty = msgty.
      sy-msgno = msgno.
      sy-msgv1 = msgv1.
      sy-msgv2 = msgv2.
      sy-msgv3 = msgv3.
      sy-msgv4 = msgv4.
      /atrm/cx_exception=>raise( ).
    ENDIF.

    IF found IS INITIAL.
      /atrm/cx_exception=>raise(
        iv_message = not_found_text
        iv_reason  = /atrm/cx_exception=>c_reason-generic
      ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

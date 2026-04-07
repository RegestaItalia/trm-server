REPORT ztrm_installer.

*******************************************************************
*              TRM - Transport Request Manager                    *
*                  https://trmregistry.com                        *
*******************************************************************
*                                                                 *
* MIT License                                                     *
*                                                                 *
* Copyright (c) 2023 RegestaItalia                                *
*                                                                 *
* Permission is hereby granted, free of charge, to any person     *
* obtaining a copy of this software and associated documentation  *
* files (the "Software"), to deal in the Software without         *
* restriction, including without limitation the rights to use,    *
* copy, modify, merge, publish, distribute, sublicense, and/or    *
* sell copies of the Software, and to permit persons to whom the  *
* Software is furnished to do so, subject to the following        *
* conditions:                                                     *
*                                                                 *
* The above copyright notice and this permission notice shall be  *
* included in all copies or substantial portions of the Software. *
*                                                                 *
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, *
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES *
* OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND        *
* NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT     *
* HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,    *
* WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR   *
* OTHER DEALINGS IN THE SOFTWARE.                                 *
*                                                                 *
*******************************************************************

SELECTION-SCREEN BEGIN OF BLOCK sc_header WITH FRAME TITLE sc_titl1.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN COMMENT 1(77) sc_txt1.
  SELECTION-SCREEN COMMENT /1(77) sc_txt2.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN COMMENT /1(77) sc_txt3.
  SELECTION-SCREEN COMMENT /1(77) sc_txt4.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN COMMENT /1(77) sc_txt5.
  SELECTION-SCREEN COMMENT /1(77) sc_txt6.
SELECTION-SCREEN END OF BLOCK sc_header.

SELECTION-SCREEN SKIP.

PARAMETERS:
  p_rest  TYPE c AS CHECKBOX DEFAULT 'X' USER-COMMAND rest.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN: BEGIN OF TABBED BLOCK psel FOR 10 LINES,
TAB (20) online USER-COMMAND tab1 DEFAULT SCREEN 100,
TAB (20) offline USER-COMMAND tab2 DEFAULT SCREEN 200,
END OF BLOCK psel.

SELECTION-SCREEN BEGIN OF SCREEN 100 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK sc_serv WITH FRAME TITLE sc_titl2.
    PARAMETERS:
      p_id TYPE strustssl-applic DEFAULT 'ANONYM'.
  SELECTION-SCREEN END OF BLOCK sc_serv.

  SELECTION-SCREEN SKIP.

  SELECTION-SCREEN BEGIN OF BLOCK sc_proxy WITH FRAME TITLE sc_titl3.
    PARAMETERS:
      p_proxy TYPE string LOWER CASE,
      p_pport TYPE string LOWER CASE,
      p_puser TYPE string LOWER CASE,
      p_ppwd  TYPE string LOWER CASE.
  SELECTION-SCREEN END OF BLOCK sc_proxy.
SELECTION-SCREEN END OF SCREEN 100.

SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK sc_other WITH FRAME TITLE TEXT-001.
    PARAMETERS:
      p_lserv TYPE rlgrap-filename LOWER CASE,
      p_lrest TYPE rlgrap-filename LOWER CASE.
  SELECTION-SCREEN END OF BLOCK sc_other.
SELECTION-SCREEN END OF SCREEN 200.

CLASS lcl_report DEFINITION.

  PUBLIC SECTION.
    CONSTANTS: base_url      TYPE string VALUE 'https://trmregistry.com/registry',
               server_trkorr TYPE trkorr VALUE 'A4HK900115',
               rest_trkorr   TYPE trkorr VALUE 'A4HK900115'.
    METHODS run.
    CLASS-METHODS get_versions
      EXPORTING server TYPE string
                rest   TYPE string.
  PRIVATE SECTION.
    TYPES: ty_package_name TYPE devclass,
           ty_package_tab  TYPE STANDARD TABLE OF ty_package_name WITH EMPTY KEY,
           BEGIN OF ty_pkg_node,
             original TYPE devclass,
             package  TYPE devclass,
             parent   TYPE devclass,
             level    TYPE i,
           END OF ty_pkg_node,
           ty_pkg_node_tab TYPE STANDARD TABLE OF ty_pkg_node WITH EMPTY KEY.
    METHODS get_client
      IMPORTING
        with_base_url TYPE c DEFAULT 'X'
        url           TYPE string OPTIONAL
          PREFERRED PARAMETER url
      RETURNING
        VALUE(client) TYPE REF TO if_http_client.
    METHODS display_error
      IMPORTING
        iv_text TYPE string.
    METHODS display_messages
      IMPORTING
        iv_response TYPE string.
    METHODS run_offline.
    METHODS run_online.
    METHODS handle_release
      IMPORTING
        release  TYPE xstring
        trkorr   TYPE trkorr
        checksum TYPE string OPTIONAL.

    CLASS-METHODS get_dir_trans
      EXPORTING dir_trans TYPE pfevalue.
    CLASS-METHODS write_binary_file
      IMPORTING file_path TYPE string
                file      TYPE xstring.
    CLASS-METHODS get_file_sys
      EXPORTING file_sys TYPE filesys.
    CLASS-METHODS delete_from_tms_queue
      IMPORTING trkorr TYPE trkorr
                system TYPE tmssysnam
      EXPORTING subrc  TYPE i.
    CLASS-METHODS forward
      IMPORTING trkorr       TYPE trkorr
                target       TYPE tmssysnam
                source       TYPE tmssysnam
                import_again TYPE flag
      EXPORTING subrc        TYPE i.
    CLASS-METHODS import
      IMPORTING trkorr TYPE trkorr
                system TYPE tmssysnam
      EXPORTING subrc  TYPE i.
    CLASS-METHODS read_queue
      IMPORTING target   TYPE tmssysnam
      EXPORTING requests TYPE tmsiqreqs
                subrc    TYPE i.
    CLASS-METHODS refresh_tms_txt
      IMPORTING trkorr TYPE trkorr.
    CLASS-METHODS rebuild_hierarchy
      IMPORTING
        it_packages         TYPE ty_package_tab
      RETURNING
        VALUE(rt_hierarchy) TYPE ty_pkg_node_tab.
    CLASS-METHODS get_parent
      IMPORTING
        iv_package       TYPE devclass
        it_packages      TYPE ty_package_tab
      RETURNING
        VALUE(rv_parent) TYPE string.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.

  METHOD get_versions.
    FIELD-SYMBOLS: <trm_server_version>  TYPE string,
                   <fs_trm_rest_version> TYPE string.
    ASSIGN ('/ATRM/IF_SERVER')=>('VERSION') TO <trm_server_version>.
    IF <trm_server_version> IS ASSIGNED.
      server = <trm_server_version>.
    ENDIF.
    ASSIGN ('/ATRM/IF_REST')=>('VERSION') TO <fs_trm_rest_version>.
    IF <fs_trm_rest_version> IS ASSIGNED.
      rest = <fs_trm_rest_version>.
    ENDIF.
  ENDMETHOD.

  METHOD get_client.
    DATA client_url TYPE string.
    IF with_base_url EQ 'X'.
      client_url = base_url.
    ENDIF.
    IF url IS NOT INITIAL.
      CONCATENATE client_url url INTO client_url.
    ENDIF.
    cl_http_client=>create_by_url(
      EXPORTING
        url                 = client_url
        ssl_id              = p_id
        proxy_host          = p_proxy
        proxy_service       = p_pport
      IMPORTING
        client              = client
      EXCEPTIONS
        argument_not_found  = 1
        plugin_not_active   = 2
        internal_error      = 3
        OTHERS              = 4 ).

    IF sy-subrc <> 0.
      display_error( 'Error in HTTP Client Create' ).
      RETURN.
    ENDIF.

    IF p_puser IS NOT INITIAL.
      client->authenticate(
        proxy_authentication = abap_true
        username             = p_puser
        password             = p_ppwd ).
    ENDIF.

    client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
  ENDMETHOD.

  METHOD run.
    DATA:
      confirm_message  TYPE string,
      confirm_answer   TYPE c,
      server_version   TYPE string,
      rest_version     TYPE string,
      install_versions TYPE string.
    SELECT COUNT( * ) FROM e070 WHERE trkorr EQ server_trkorr.
    IF sy-subrc EQ 0.
      CONCATENATE 'Transport' server_trkorr '(trm-server transport number)' 'already exists in' sy-sysid 'Do you want to overwrite?' INTO confirm_message SEPARATED BY space.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question         = confirm_message
          text_button_1         = 'Continue'
          icon_button_1         = '@0V@'
          text_button_2         = 'Cancel'
          icon_button_2         = '@0W@'
          display_cancel_button = ' '
        IMPORTING
          answer                = confirm_answer.
      IF confirm_answer = '2'.
        WRITE / 'Installation cancelled by user.'.
        RETURN.
      ENDIF.
    ENDIF.
    IF p_rest EQ 'X'.
      SELECT COUNT( * ) FROM e070 WHERE trkorr EQ rest_trkorr.
      IF sy-subrc EQ 0.
        CONCATENATE 'Transport' rest_trkorr '(trm-rest transport number)' 'already exists in' sy-sysid 'Do you want to overwrite?' INTO confirm_message SEPARATED BY space.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question         = confirm_message
            text_button_1         = 'Continue'
            icon_button_1         = '@0V@'
            text_button_2         = 'Cancel'
            icon_button_2         = '@0W@'
            display_cancel_button = ' '
          IMPORTING
            answer                = confirm_answer.
        IF confirm_answer = '2'.
          WRITE / 'Installation cancelled by user.'.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.
    IF psel-activetab EQ 'TAB1'.
      run_online( ).
    ENDIF.
    IF psel-activetab EQ 'TAB2'.
      run_offline( ).
    ENDIF.
    get_versions( IMPORTING server = server_version rest = rest_version ).
    IF server_version IS NOT INITIAL OR rest_version IS NOT INITIAL.
      install_versions = 'Successfully installed'.
      IF server_version IS NOT INITIAL.
        CONCATENATE install_versions 'trm-server' server_version INTO install_versions SEPARATED BY space.
      ENDIF.
      IF p_rest EQ 'X' AND rest_version IS NOT INITIAL.
        CONCATENATE install_versions 'trm-rest' rest_version INTO install_versions SEPARATED BY space.
      ENDIF.
      MESSAGE install_versions TYPE 'I'.
      WRITE: /, install_versions.
    ENDIF.
  ENDMETHOD.

  METHOD display_error.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Error'
        txt1  = iv_text
        txt2  = ''.
  ENDMETHOD.

  METHOD display_messages.
    DATA:
      lt_lines TYPE TABLE OF string,
      lv_line  TYPE string.

    SPLIT iv_response AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    LOOP AT lt_lines INTO lv_line.
      WRITE / lv_line.
    ENDLOOP.
    SKIP.
  ENDMETHOD.

  METHOD run_offline.
    DATA: filename TYPE string,
          bin      TYPE STANDARD TABLE OF x255,
          filelen  TYPE i,
          file     TYPE xstring.

    IF p_lserv IS INITIAL.
      MESSAGE 'Missing trm-server release file!' TYPE 'E'.
    ENDIF.
    IF p_rest EQ 'X' AND p_lrest IS INITIAL.
      MESSAGE 'Missing trm-rest release file!' TYPE 'E'.
    ENDIF.
    filename = p_lserv.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = filename
        filetype                = 'BIN'
      IMPORTING
        filelength              = filelen
      TABLES
        data_tab                = bin
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.

    IF sy-subrc <> 0.
      MESSAGE |Error during binary upload.| TYPE 'E'.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = filelen
      IMPORTING
        buffer       = file
      TABLES
        binary_tab   = bin
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      MESSAGE 'Error converting binary table' TYPE 'E'.
    ENDIF.

    WRITE / 'Starting installation of trm-server...'.
    handle_release(
      EXPORTING
        release  = file
        trkorr   = server_trkorr
    ).

    IF p_rest EQ 'X'.
      filename = p_lrest.

      CALL FUNCTION 'GUI_UPLOAD'
        EXPORTING
          filename                = filename
          filetype                = 'BIN'
        IMPORTING
          filelength              = filelen
        TABLES
          data_tab                = bin
        EXCEPTIONS
          file_open_error         = 1
          file_read_error         = 2
          no_batch                = 3
          gui_refuse_filetransfer = 4
          invalid_type            = 5
          no_authority            = 6
          unknown_error           = 7
          bad_data_format         = 8
          header_not_allowed      = 9
          separator_not_allowed   = 10
          header_too_long         = 11
          unknown_dp_error        = 12
          access_denied           = 13
          dp_out_of_memory        = 14
          disk_full               = 15
          dp_timeout              = 16
          OTHERS                  = 17.

      IF sy-subrc <> 0.
        MESSAGE |Error during binary upload.| TYPE 'E'.
      ENDIF.

      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = filelen
        IMPORTING
          buffer       = file
        TABLES
          binary_tab   = bin
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.

      IF sy-subrc <> 0.
        MESSAGE 'Error converting binary table' TYPE 'E'.
      ENDIF.

      WRITE / 'Starting installation of trm-rest...'.
      handle_release(
        EXPORTING
          release  = file
          trkorr   = rest_trkorr
      ).
    ENDIF.
  ENDMETHOD.

  METHOD run_online.
    TYPES: BEGIN OF release,
             download_link TYPE string,
             checksum      TYPE string,
           END OF release.
    DATA:
      code            TYPE i,
      client          TYPE REF TO if_http_client,
      reason          TYPE string,
      response        TYPE string,
      confirm_message TYPE string,
      confirm_answer  TYPE c,
      release         TYPE release,
      file            TYPE xstring.

    cl_progress_indicator=>progress_indicate(
        i_text = 'Checking connection to TRM Registry...'
        i_output_immediately = 'X' ).

    client = get_client( ).
    client->send( ).
    client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      display_error( 'Error in HTTP Client Receive' ).

      client->get_last_error(
        IMPORTING
          message = response ).

      display_messages( response ).

      WRITE / 'Also check transaction SMICM -> Goto -> Trace File -> Display End'.
      RETURN.
    ENDIF.
* if SSL Handshake fails, make sure to also check https://launchpad.support.sap.com/#/notes/510007
    client->response->get_status(
      IMPORTING
        code   = code
        reason = reason ).
    IF code <> 200.
      display_error( 'TRM Registry is unreachable!' ).
    ENDIF.


    WRITE / 'Successfully connected to TRM Registry.'.
    IF p_rest EQ 'X'.
      confirm_message = 'trm-server and trm-rest'.
    ELSE.
      confirm_message = 'trm-server'.
    ENDIF.
    CONCATENATE confirm_message 'will be downloaded from the registry and imported. Do you want to proceed?' INTO confirm_message SEPARATED BY space.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = confirm_message
        text_button_1         = 'Continue'
        icon_button_1         = '@0V@'
        text_button_2         = 'Cancel'
        icon_button_2         = '@0W@'
        display_cancel_button = ' '
      IMPORTING
        answer                = confirm_answer.

    IF confirm_answer = '2'.
      WRITE / 'Installation cancelled by user.'.
      RETURN.
    ENDIF.

    WRITE / 'Starting installation of trm-server...'.

    WRITE / 'Downloading trm-server latest release from TRM Registry...'.
    cl_progress_indicator=>progress_indicate(
        i_text = 'Downloading trm-server latest release from TRM Registry...'
        i_output_immediately = 'X' ).

    " trm-server
    client = get_client( '/package/trm-server' ).
    client->request->set_method( if_http_request=>co_request_method_get ).
    client->send( ).
    client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      display_error( 'Error in HTTP Client Receive' ).
      client->get_last_error(
        IMPORTING
          message = response ).
      display_messages( response ).
      RETURN.
    ENDIF.
    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = client->response->get_cdata( )
      CHANGING
        data             = release
    ).
    client = get_client(
      with_base_url = ' '
      url           = release-download_link
    ).
    client->request->set_method( if_http_request=>co_request_method_get ).
    client->request->set_header_field( name = 'Accept' value = 'application/octet-stream' ).
    client->send( ).
    client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      display_error( 'Error in HTTP Client Receive' ).
      client->get_last_error(
        IMPORTING
          message = response ).
      display_messages( response ).
      RETURN.
    ENDIF.
    file = client->response->get_data(
*             virus_scan_profile = '/SIHTTP/HTTP_UPLOAD'
*             vscan_scan_always  = if_http_entity=>co_content_check_profile
    ).
    handle_release(
      EXPORTING
        release  = file
        trkorr   = server_trkorr
        checksum = release-checksum
    ).

    IF p_rest EQ 'X'.
      WRITE / 'Starting installation of trm-rest...'.

      WRITE / 'Downloading trm-rest latest release from TRM Registry...'.
      cl_progress_indicator=>progress_indicate(
          i_text = 'Downloading trm-rest latest release from TRM Registry...'
          i_output_immediately = 'X' ).

      " trm-server
      client = get_client( '/package/trm-rest' ).
      client->request->set_method( if_http_request=>co_request_method_get ).
      client->send( ).
      client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
      IF sy-subrc <> 0.
        display_error( 'Error in HTTP Client Receive' ).
        client->get_last_error(
          IMPORTING
            message = response ).
        display_messages( response ).
        RETURN.
      ENDIF.
      /ui2/cl_json=>deserialize(
        EXPORTING
          json             = client->response->get_cdata( )
        CHANGING
          data             = release
      ).
      client = get_client(
        with_base_url = ' '
        url           = release-download_link
      ).
      client->request->set_method( if_http_request=>co_request_method_get ).
      client->request->set_header_field( name = 'Accept' value = 'application/octet-stream' ).
      client->send( ).
      client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
      IF sy-subrc <> 0.
        display_error( 'Error in HTTP Client Receive' ).
        client->get_last_error(
          IMPORTING
            message = response ).
        display_messages( response ).
        RETURN.
      ENDIF.
      file = client->response->get_data(
*             virus_scan_profile = '/SIHTTP/HTTP_UPLOAD'
*             vscan_scan_always  = if_http_entity=>co_content_check_profile
      ).
      handle_release(
        EXPORTING
          release  = file
          trkorr   = rest_trkorr
          checksum = release-checksum
      ).
    ENDIF.
  ENDMETHOD.

  METHOD get_dir_trans.
    DATA lv_param_name TYPE pfeparname.
    lv_param_name = 'DIR_TRANS'.
    CALL FUNCTION 'SXPG_PROFILE_PARAMETER_GET'
      EXPORTING
        parameter_name  = lv_param_name
      IMPORTING
        parameter_value = dir_trans
      EXCEPTIONS
        OTHERS          = 1.
  ENDMETHOD.
  METHOD write_binary_file.
    OPEN DATASET file_path FOR OUTPUT IN BINARY MODE.
    TRANSFER file TO file_path.
    CLOSE DATASET file_path.
  ENDMETHOD.
  METHOD get_file_sys.
    SELECT SINGLE filesys INTO file_sys FROM opsystem WHERE opsys = sy-opsys.
  ENDMETHOD.
  METHOD delete_from_tms_queue.
    DATA: ls_tmsbuffer    TYPE tmsbuffer,
          lt_tp_maintains TYPE stms_tp_maintains,
          ls_tp_maintains LIKE LINE OF lt_tp_maintains,
          ls_tpstdout     TYPE tpstdout,
          ls_exception    TYPE stmscalert.

    SELECT SINGLE * FROM tmsbuffer INTO ls_tmsbuffer WHERE sysnam EQ system AND trkorr EQ trkorr.
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
          WRITE: /, ls_tpstdout-line.
        ENDLOOP.
      ENDIF.
      subrc = 1.
    ELSE.
      subrc = 0.
    ENDIF.
  ENDMETHOD.
  METHOD forward.
    DATA: lt_stdout    TYPE STANDARD TABLE OF tpstdout,
          ls_stdout    LIKE LINE OF lt_stdout,
          ls_exception TYPE stmscalert.
    CALL FUNCTION 'TMS_MGR_FORWARD_TR_REQUEST'
      EXPORTING
        iv_request      = trkorr
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
      subrc = 1.
    ELSEIF ls_exception-msgty EQ 'E' OR ls_exception-msgty EQ 'A'.
      subrc = 1.
      LOOP AT lt_stdout INTO ls_stdout.
        WRITE: /, ls_stdout-line.
      ENDLOOP.
    ELSE.
      subrc = 0.
    ENDIF.
  ENDMETHOD.
  METHOD import.
    CALL FUNCTION 'TMS_MGR_IMPORT_TR_REQUEST'
      EXPORTING
        iv_system             = system
        iv_request            = trkorr
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
      subrc = 1.
    ELSE.
      subrc = 0.
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
      subrc = 1.
      RETURN.
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
        WRITE: /, lv_dummy.
        subrc = 1.
        RETURN.
      ENDIF.
    ENDIF.
    subrc = 0.
  ENDMETHOD.
  METHOD refresh_tms_txt.
    DATA ls_tmsbuftxt TYPE tmsbuftxt.
    SELECT SINGLE * FROM tmsbuftxt INTO ls_tmsbuftxt WHERE trkorr EQ trkorr.
    CHECK sy-subrc EQ 0.
    SELECT SINGLE as4text FROM e07t INTO ls_tmsbuftxt-text WHERE trkorr EQ trkorr.
    SELECT SINGLE as4user FROM e070 INTO ls_tmsbuftxt-owner WHERE trkorr EQ trkorr.
    SELECT SINGLE client FROM e070c INTO ls_tmsbuftxt-srccli WHERE trkorr EQ trkorr.
    " there's no standard way to update buffer text table other than clearing the buffer as a whole?
    MODIFY tmsbuftxt FROM ls_tmsbuftxt.
    COMMIT WORK AND WAIT.
    " don't raise exception if it fails!!
  ENDMETHOD.
  METHOD rebuild_hierarchy.
    DATA: lt_packages    TYPE ty_package_tab,
          lv_package     TYPE ty_package_name,
          lv_parent      TYPE devclass,
          lv_level       TYPE i,
          lv_walk_parent TYPE devclass.
    FIELD-SYMBOLS <hierarchy> TYPE ty_pkg_node.
    lt_packages = it_packages.

    LOOP AT lt_packages INTO lv_package.
      lv_parent = get_parent(
        iv_package  = lv_package
        it_packages = lt_packages ).

      lv_level = 0.
      lv_walk_parent = lv_parent.

      " Calculate hierarchy depth
      WHILE lv_walk_parent IS NOT INITIAL.
        lv_level += 1.
        lv_walk_parent = get_parent(
          iv_package  = lv_walk_parent
          it_packages = lt_packages ).
      ENDWHILE.

      APPEND INITIAL LINE TO rt_hierarchy ASSIGNING <hierarchy>.
      <hierarchy>-package = lv_package.
      <hierarchy>-original = lv_package.
      <hierarchy>-parent = lv_parent.
      <hierarchy>-level = lv_level.

      REPLACE FIRST OCCURRENCE OF '/ATRM/SERVER' IN <hierarchy>-package WITH '$TRM'.
      REPLACE FIRST OCCURRENCE OF '/ATRM/REST' IN <hierarchy>-package WITH '$TRM_REST'.
      REPLACE FIRST OCCURRENCE OF '/ATRM/SERVER' IN <hierarchy>-parent WITH '$TRM'.
      REPLACE FIRST OCCURRENCE OF '/ATRM/REST' IN <hierarchy>-parent WITH '$TRM_REST'.
    ENDLOOP.

    SORT rt_hierarchy BY level package.
  ENDMETHOD.
  METHOD get_parent.
    DATA: lv_best_len      TYPE i VALUE -1,
          lv_candidate     TYPE ty_package_name,
          lv_candidate_len TYPE i,
          lv_package_len   TYPE i.

    LOOP AT it_packages INTO lv_candidate.
      IF lv_candidate = iv_package.
        CONTINUE.
      ENDIF.

      lv_candidate_len = strlen( lv_candidate ).
      lv_package_len   = strlen( iv_package ).

      " Candidate must be shorter
      IF lv_candidate_len >= lv_package_len.
        CONTINUE.
      ENDIF.

      " Candidate must be a prefix of the package
      IF iv_package+0(lv_candidate_len) <> lv_candidate.
        CONTINUE.
      ENDIF.

      " require '_' after the parent name to avoid false positives
      IF iv_package+lv_candidate_len(1) <> '_'.
        CONTINUE.
      ENDIF.

      " Keep the longest matching prefix = immediate parent
      IF lv_candidate_len > lv_best_len.
        lv_best_len = lv_candidate_len.
        rv_parent = lv_candidate.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD handle_release.
    TYPES: BEGIN OF manifest,
             name     TYPE string,
             registry TYPE string,
           END OF manifest.
    DATA:
      release_manifest_json TYPE xstring,
      release_manifest      TYPE manifest,
      release_entry         TYPE string,
      data_file_name        TYPE string,
      header_file_name      TYPE string,
      data_file_path        TYPE string,
      header_file_path      TYPE string,
      data_file             TYPE xstring,
      header_file           TYPE xstring,
      file_hash             TYPE string,
      zip                   TYPE REF TO cl_abap_zip,
      transport_files       TYPE xstring,
      filesys               TYPE filesys,
      file_path_separator   TYPE c,
      dirtrans              TYPE pfevalue,
      tmssysnam             TYPE tmssysnam,
      subrc                 TYPE i,
      tmsrequests           TYPE tmsiqreqs,
      in_queue              TYPE c,
      transport_rc          TYPE i,
      e071                  TYPE STANDARD TABLE OF tadir,
      tadir                 TYPE STANDARD TABLE OF tadir,
      tadir_line            LIKE LINE OF tadir,
      devclass              TYPE STANDARD TABLE OF tadir,
      devclass_line         LIKE LINE OF devclass,
      devclass_exists       TYPE STANDARD TABLE OF tdevc,
      packages              TYPE ty_package_tab,
      hierarchy             TYPE ty_pkg_node_tab,
      node                  LIKE LINE OF hierarchy,
      sap_package           TYPE scompkdtln,
      sap_package_instance  TYPE REF TO if_package.
    IF subrc <> 0.
      display_error( 'Error getting default transport layer' ).
      RETURN.
    ENDIF.
    get_file_sys(
      IMPORTING
        file_sys = filesys
    ).
    WRITE: /, 'System: ', filesys-filesys.
    IF filesys-filesys EQ 'WINDOWS NT' OR filesys EQ 'DOS'.
      file_path_separator = '\'.
    ELSEIF filesys-filesys EQ 'UNIX' OR filesys-filesys EQ 'AS\400' OR filesys-filesys EQ 'MACINTOSH' OR filesys-filesys EQ 'MPE' OR filesys-filesys EQ 'VMS'.
      file_path_separator = '/'.
    ELSE.
      display_error( 'Cannot get system file path separator' ).
      RETURN.
    ENDIF.
    get_dir_trans( IMPORTING dir_trans = dirtrans ).
    CONCATENATE 'dist/' trkorr INTO release_entry.
    data_file_name = 'R' && trkorr+4(6) && '.' && trkorr(3).
    header_file_name =  'K' && trkorr+4(6) && '.' && trkorr(3).
    data_file_path = dirtrans && file_path_separator && 'data' && file_path_separator && data_file_name.
    header_file_path =  dirtrans && file_path_separator && 'cofiles' && file_path_separator && header_file_name.
    WRITE: /, 'Release data: ', data_file_name, ' Release header: ', header_file_name.
    IF checksum IS NOT INITIAL.
      WRITE: /, 'Verifying release integrity, to match', / checksum.
      TRY.
          cl_abap_message_digest=>calculate_hash_for_raw(
            EXPORTING
              if_algorithm     = 'SHA512'
              if_data          = release
            IMPORTING
              ef_hashb64string = file_hash
          ).
        CATCH cx_abap_message_digest.
          display_error( 'Error in release checksum' ).
          RETURN.
      ENDTRY.
      IF file_hash <> checksum.
        display_error( 'Release checksum does not match!' ).
        RETURN.
      ENDIF.
      WRITE / 'Release integrity verified.'.
    ENDIF.
    CREATE OBJECT zip.
    zip->load(
      EXPORTING
        zip = release
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2
    ).
    IF sy-subrc <> 0.
      display_error( 'Error in release content' ).
      RETURN.
    ENDIF.
    zip->get(
      EXPORTING
        name = 'manifest.json'
      IMPORTING
        content = release_manifest_json
      EXCEPTIONS
        zip_decompression_error = 1
        zip_index_error         = 2
        OTHERS                  = 3
    ).
    IF release_manifest_json IS INITIAL OR sy-subrc <> 0.
      display_error( 'Error in release content: manifest not found' ).
      RETURN.
    ENDIF.
    /ui2/cl_json=>deserialize(
      EXPORTING
        jsonx            = release_manifest_json
      CHANGING
        data             = release_manifest
    ).
    IF release_manifest-name <> 'trm-server' AND release_manifest-name <> 'trm-rest'.
      display_error( 'Error in release content: invalid manifest' ).
      WRITE: /, 'Expected manifest name to be trm-server or trm-rest but found', release_manifest-name.
      RETURN.
    ENDIF.
    IF release_manifest-registry IS NOT INITIAL.
      display_error( 'Error in release content: registry field should be empty' ).
      WRITE: /, 'Expected manifest registry field to be empty but found', release_manifest-registry.
      RETURN.
    ENDIF.
    zip->get(
      EXPORTING
        name = release_entry
      IMPORTING
        content = transport_files
      EXCEPTIONS
        zip_decompression_error = 1
        zip_index_error         = 2
        OTHERS                  = 3
    ).
    IF transport_files IS INITIAL OR sy-subrc <> 0.
      display_error( 'Error in release content: transport files not found' ).
      RETURN.
    ENDIF.
    zip->load(
      EXPORTING
        zip = transport_files
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2
    ).
    IF sy-subrc <> 0.
      display_error( 'Error in release content' ).
      RETURN.
    ENDIF.
    zip->get(
      EXPORTING
        name = header_file_name
      IMPORTING
        content = header_file
      EXCEPTIONS
        zip_decompression_error = 1
        zip_index_error         = 2
        OTHERS                  = 3
    ).
    IF header_file IS INITIAL OR sy-subrc <> 0.
      display_error( 'Error in release content: header file not found' ).
      RETURN.
    ENDIF.
    zip->get(
      EXPORTING
        name = data_file_name
      IMPORTING
        content = data_file
      EXCEPTIONS
        zip_decompression_error = 1
        zip_index_error         = 2
        OTHERS                  = 3
    ).
    IF data_file IS INITIAL OR sy-subrc <> 0.
      display_error( 'Error in release content: data file not found' ).
      RETURN.
    ENDIF.
    tmssysnam = sy-sysid.

    WRITE: /, 'Deleting', trkorr, 'from TMS queue (if exists)...'.
    delete_from_tms_queue(
      EXPORTING
        trkorr  = trkorr
        system  = tmssysnam
      IMPORTING
        subrc   = subrc
    ).
    IF subrc <> 0.
      display_error( 'Error deleting transport from TMS queue' ).
      RETURN.
    ENDIF.

    WRITE / 'All files extracted from release, ready to import into application server.'.
    WRITE: /, 'Release data: ', data_file_path, ' Release header: ', header_file_path.
    cl_progress_indicator=>progress_indicate(
      i_text = 'Copying release files into application server...'
      i_output_immediately = 'X' ).
    write_binary_file( EXPORTING file_path = header_file_path file = header_file ).
    write_binary_file( EXPORTING file_path = data_file_path file = data_file ).

    WRITE: /, 'Forwarding', trkorr, '...'.
    cl_progress_indicator=>progress_indicate(
      i_text = 'Forwarding...'
      i_output_immediately = 'X' ).
    forward(
      EXPORTING
        trkorr       = trkorr
        target       = tmssysnam
        source       = tmssysnam
        import_again = 'X'
      IMPORTING
        subrc         = subrc
    ).
    IF subrc <> 0.
      display_error( 'Error forwarding transport' ).
      RETURN.
    ENDIF.

    WRITE: /, 'Importing', trkorr, '...'.
    cl_progress_indicator=>progress_indicate(
      i_text = 'Importing...'
      i_output_immediately = 'X' ).
    import(
      EXPORTING
        trkorr = trkorr
        system = tmssysnam
      IMPORTING
         subrc  = subrc
    ).
    IF subrc <> 0.
      display_error( 'Error importing transport' ).
      RETURN.
    ENDIF.

    WRITE: /, 'Waiting for import status update in TMS queue...'.
    cl_progress_indicator=>progress_indicate(
      i_text = 'Installing...'
      i_output_immediately = 'X' ).
    CLEAR in_queue.
    WHILE in_queue <> 'X'.
      WAIT UP TO 3 SECONDS.
      read_queue(
        EXPORTING
          target = tmssysnam
        IMPORTING
          requests = tmsrequests
          subrc  = subrc
      ).
      IF subrc <> 0.
        display_error( 'Error reading queue' ).
        RETURN.
      ENDIF.
      DELETE tmsrequests WHERE trkorr NE trkorr.
      SORT tmsrequests BY bufpos DESCENDING.
      IF tmsrequests IS NOT INITIAL.
        IF tmsrequests[ 1 ]-impsing <> 'X'.
          in_queue = 'X'.
        ENDIF.
        IF tmsrequests[ 1 ]-maxrc IS INITIAL.
          transport_rc = -1.
        ELSE.
        ENDIF.
        transport_rc = tmsrequests[ 1 ]-maxrc.
      ENDIF.
    ENDWHILE.
    WRITE: /, 'Import of transport', trkorr, 'ended with return code', transport_rc.
    refresh_tms_txt( EXPORTING trkorr = trkorr ).


    cl_progress_indicator=>progress_indicate(
      i_text = 'Generating SAP packages...'
      i_output_immediately = 'X' ).
    SELECT pgmid object obj_name FROM e071 INTO CORRESPONDING FIELDS OF TABLE e071 WHERE trkorr EQ trkorr AND pgmid EQ 'R3TR'.
    IF e071[] IS NOT INITIAL.
      SELECT pgmid object obj_name devclass
        FROM tadir
        INTO CORRESPONDING FIELDS OF TABLE tadir
        FOR ALL ENTRIES IN e071
        WHERE pgmid EQ e071-pgmid
          AND object EQ e071-object
          AND obj_name EQ e071-obj_name.
      MOVE tadir[] TO devclass[].
      SORT devclass BY devclass ASCENDING.
      DELETE ADJACENT DUPLICATES FROM devclass COMPARING devclass.
      LOOP AT devclass INTO devclass_line.
        APPEND devclass_line-devclass TO packages.
      ENDLOOP.
      hierarchy = rebuild_hierarchy( packages ).
      SELECT devclass
        FROM tdevc
        INTO TABLE devclass_exists
        FOR ALL ENTRIES IN hierarchy
        WHERE devclass EQ hierarchy-package.
      LOOP AT hierarchy INTO node.
        READ TABLE devclass_exists TRANSPORTING NO FIELDS WITH KEY devclass = node-package.
        CHECK sy-subrc <> 0.
        WRITE: /, 'Creating package', node-package.
        CLEAR sap_package.
        CLEAR sap_package_instance.
        sap_package-devclass = node-package.
        sap_package-parentcl = node-parent.
        sap_package-ctext = 'TRM (Transport Request Manager)'.
        sap_package-as4user = sy-uname.
        sap_package-dlvunit = 'LOCAL'.
        sap_package-masterlang = sy-langu.
        cl_package_factory=>create_new_package(
          EXPORTING
            i_reuse_deleted_object       = abap_true
          IMPORTING
            e_package                    = sap_package_instance
          CHANGING
            c_package_data               = sap_package
          EXCEPTIONS
            object_already_existing      = 1
            object_just_created          = 2
            not_authorized               = 3
            wrong_name_prefix            = 4
            undefined_name               = 5
            reserved_local_name          = 6
            invalid_package_name         = 7
            short_text_missing           = 8
            software_component_invalid   = 9
            layer_invalid                = 10
            author_not_existing          = 11
            component_not_existing       = 12
            component_missing            = 13
            prefix_in_use                = 14
            unexpected_error             = 15
            intern_err                   = 16
            no_access                    = 17
            invalid_translation_depth    = 18
            wrong_mainpack_value         = 19
            superpackage_invalid         = 20
            error_in_cts_checks          = 21
            OTHERS                       = 22
        ).
        IF sy-subrc <> 0.
          display_error( 'Error in package create' ).
          RETURN.
        ENDIF.
        sap_package_instance->save( ).
        sap_package_instance->set_changeable( abap_false ).
      ENDLOOP.
      cl_progress_indicator=>progress_indicate(
        i_text = 'Adjusting tadir entries...'
        i_output_immediately = 'X' ).
      LOOP AT tadir INTO tadir_line.
        READ TABLE hierarchy INTO node WITH KEY original = tadir_line-devclass.
        CALL FUNCTION 'TR_TADIR_INTERFACE'
          EXPORTING
            wi_test_modus      = ' '
            wi_tadir_pgmid     = tadir_line-pgmid
            wi_tadir_object    = tadir_line-object
            wi_tadir_obj_name  = tadir_line-obj_name
            wi_tadir_devclass  = node-package
            wi_tadir_srcsystem = 'TRM'
            wi_set_genflag     = 'X'
*           iv_no_pak_check    = 'X'
          EXCEPTIONS
            OTHERS             = 1.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

DATA report TYPE REF TO lcl_report.

INITIALIZATION.
  sc_titl1               = 'Description'.
  sc_txt1                = 'This report can be used to perform the first installs of trm-server'.
  sc_txt2                = 'and trm-rest.'.
  sc_txt3                = 'You can either let the report download the latest release from the TRM'.
  sc_txt4                = 'registry or provide a release yourself via file upload.'.
  sc_txt5                = '@X1@ TRM Installer v1.0.0 - RegestaItalia'.
  sc_txt6                = 'Visit trmregistry.com'.
  sc_titl2               = 'Registry connection settings'.
  sc_titl3               = 'Proxy settings (Optional)'.
  %_p_rest_%_app_%-text  = 'Install trm-rest'.
  online                 = 'Online install'.
  offline                = 'Offline install'.

  psel-prog      = sy-repid.
  psel-dynnr     = 100.
  psel-activetab = 'TAB1'.

  CREATE OBJECT report.
  DATA:
    confirm_message TYPE string,
    confirm_answer  TYPE c,
    server_version  TYPE string,
    rest_version    TYPE string.
  lcl_report=>get_versions( IMPORTING server = server_version rest = rest_version ).
  IF server_version IS NOT INITIAL OR rest_version IS NOT INITIAL.
    confirm_message = 'Already installed'.
    IF server_version IS NOT INITIAL.
      CONCATENATE confirm_message 'trm-server' server_version INTO confirm_message SEPARATED BY space.
    ENDIF.
    IF p_rest EQ 'X' AND rest_version IS NOT INITIAL.
      CONCATENATE confirm_message 'trm-rest' rest_version INTO confirm_message SEPARATED BY space.
    ENDIF.
    CONCATENATE confirm_message 'This report should be used for first installs only! Consider upgrading via TRM. Do you want to continue anyway?' INTO confirm_message SEPARATED BY space.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = confirm_message
        text_button_1         = 'Continue'
        icon_button_1         = '@0V@'
        text_button_2         = 'Cancel'
        icon_button_2         = '@0W@'
        display_cancel_button = ' '
      IMPORTING
        answer                = confirm_answer.
    IF confirm_answer = '2'.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN.
  p_proxy = replace(
    val   = p_proxy
    regex = 'http(s?)://'
    with  = ''
    occ   = 1 ).
  CASE sy-ucomm.
    WHEN 'TAB1'.
      psel-dynnr = 100.
      psel-activetab = 'TAB1'.
    WHEN 'TAB2'.
      psel-dynnr = 200.
      psel-activetab = 'TAB2'.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  %_p_id_%_app_%-text    = 'SSL Client Identity'.
  %_p_proxy_%_app_%-text = 'Hostname/IP'.
  %_p_pport_%_app_%-text = 'Port'.
  %_p_puser_%_app_%-text = 'Username'.
  %_p_ppwd_%_app_%-text  = 'Password'.
  %_p_lserv_%_app_%-text  = 'trm-server package'.
  %_p_lrest_%_app_%-text  = 'trm-rest package'.
  LOOP AT SCREEN.
    IF screen-name EQ 'P_PPWD'.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF p_rest EQ 'X'.
      IF screen-name EQ 'P_LREST'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ '%_P_LREST_%_APP_%-TEXT'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-name EQ 'P_LREST'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ '%_P_LREST_%_APP_%-TEXT'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.
  report->run( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lserv.
  PERFORM choose_file CHANGING p_lserv.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lrest.
  PERFORM choose_file CHANGING p_lrest.

FORM choose_file CHANGING file TYPE rlgrap-filename.
  DATA: lt_filetable TYPE filetable,
        lv_rc        TYPE i,
        lv_action    TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select a file'
      multiselection          = abap_false
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
      user_action             = lv_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc <> 0.
    MESSAGE 'Error opening file chooser' TYPE 'E'.
  ENDIF.

  IF lv_action = cl_gui_frontend_services=>action_ok AND lv_rc > 0.
    READ TABLE lt_filetable INDEX 1 INTO DATA(ls_file).
    IF sy-subrc = 0.
      file = ls_file-filename.
    ENDIF.
  ENDIF.
ENDFORM.

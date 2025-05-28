CLASS zcl_trm_utility DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: tyt_ko100               TYPE STANDARD TABLE OF ko100 WITH DEFAULT KEY,
           tyt_installdevc         TYPE STANDARD TABLE OF ztrm_installdevc WITH DEFAULT KEY,
           tyt_trnspacett          TYPE STANDARD TABLE OF trnspacett WITH DEFAULT KEY,
           tyt_migration_tmsbuffer TYPE STANDARD TABLE OF ztrm_tmsbuffer WITH DEFAULT KEY,
           tyt_migration_doktl     TYPE STANDARD TABLE OF ztrm_doktl WITH DEFAULT KEY,
           tyt_migration_e071      TYPE STANDARD TABLE OF ztrm_e071 WITH DEFAULT KEY,
           tyt_migration_e070      TYPE STANDARD TABLE OF ztrm_e070 WITH DEFAULT KEY.

    "! Check if current user is authorized to execute TRM functions
    "! @parameter rv_authorized | 'X' if authorized
    CLASS-METHODS check_functions_authorization
      RETURNING VALUE(rv_authorized) TYPE flag.

    "! Add a transport to the TRM skip list
    "! @parameter iv_trkorr | Transport request to skip
    CLASS-METHODS add_skip_trkorr
      IMPORTING iv_trkorr TYPE trkorr
      RAISING   zcx_trm_exception.

    "! Remove a transport from the TRM skip list
    "! @parameter iv_trkorr | Transport request to remove from skip list
    CLASS-METHODS remove_skip_trkorr
      IMPORTING iv_trkorr TYPE trkorr
      RAISING   zcx_trm_exception.

    "! Add transport to the TRM source list
    "! @parameter iv_trkorr | Transport request to add to source list
    CLASS-METHODS add_source_trkorr
      IMPORTING iv_trkorr TYPE trkorr
      RAISING   zcx_trm_exception.

    "! Remove a transport from the TRM source list
    "! @parameter iv_trkorr | Transport request to remove from source list
    CLASS-METHODS remove_source_trkorr
      IMPORTING iv_trkorr TYPE trkorr
      RAISING   zcx_trm_exception.

    "! Get a binary file from the application server
    "! @parameter iv_file_path | Absolute path to the binary file
    "! @parameter ev_file | File content as xstring
    CLASS-METHODS get_binary_file
      IMPORTING iv_file_path TYPE string
      EXPORTING ev_file      TYPE xstring
      RAISING   zcx_trm_exception.

    "! Write a binary file to the application server
    "! @parameter iv_file_path | Absolute path to the binary file
    "! @parameter iv_file | File content as xstring
    CLASS-METHODS write_binary_file
      IMPORTING iv_file_path TYPE string
                iv_file      TYPE xstring
      RAISING   zcx_trm_exception.

    "! Get the DIR_TRANS path from system profile
    "! @parameter ev_dir_trans | DIR_TRANS path
    CLASS-METHODS get_dir_trans
      EXPORTING ev_dir_trans TYPE pfevalue
      RAISING   zcx_trm_exception.

    "! Get the current operating system's file system
    "! @parameter ev_file_sys | OS
    CLASS-METHODS get_file_sys
      EXPORTING ev_file_sys TYPE filesys
      RAISING   zcx_trm_exception.

    "! Get the default transport layer
    "! @parameter ev_layer | Transport layer
    CLASS-METHODS get_default_transport_layer
      EXPORTING ev_layer TYPE devlayer
      RAISING   zcx_trm_exception.

    "! Get supported object types from transport configuration
    "! @parameter et_object_text | Supported object types
    CLASS-METHODS get_supported_object_types
      EXPORTING et_object_text TYPE tyt_ko100
      RAISING   zcx_trm_exception.

    "! Add install devclasses into TRM install devclass list
    "! @parameter it_installdevc | Devclasses to add
    CLASS-METHODS add_install_devclass
      IMPORTING it_installdevc TYPE tyt_installdevc
      RAISING   zcx_trm_exception.

    "! Add package integrity
    "! @parameter is_integrity | Integrity
    CLASS-METHODS add_package_integrity
      IMPORTING is_integrity TYPE ztrm_integrity
      RAISING   zcx_trm_exception.

    "! Wrapper for TR_TADIR_INTERFACE to register objects in the TADIR table
    "! @parameter iv_pgmid     | Program ID
    "! @parameter iv_object    | Object type
    "! @parameter iv_objname   | Name of the object
    "! @parameter iv_devclass  | (Optional) Development class the object belongs to
    "! @parameter iv_srcsystem | (Optional) Logical system the object originates from
    "! @parameter iv_author    | (Optional) Author of the object
    "! @parameter iv_genflag   | (Optional) Generation flag (X = generated object)
    CLASS-METHODS tadir_interface
      IMPORTING iv_pgmid     TYPE pgmid
                iv_object    TYPE trobjtype
                iv_objname   TYPE sobj_name
                iv_devclass  TYPE devclass OPTIONAL
                iv_srcsystem TYPE srcsystem OPTIONAL
                iv_author    TYPE responsibl OPTIONAL
                iv_genflag   TYPE genflag OPTIONAL
      RAISING   zcx_trm_exception.

    "! Add and activate a custom namespace entry in the system
    "! @parameter iv_namespace  | Namespace key to be added
    "! @parameter iv_replicense | Repair license key for the namespace
    "! @parameter it_texts      | Text descriptions for the namespace (multilingual)
    CLASS-METHODS add_namespace
      IMPORTING iv_namespace  TYPE namespace
                iv_replicense TYPE trnlicense
                it_texts      TYPE tyt_trnspacett
      RAISING   zcx_trm_exception.

    "! Get R3trans utility version
    "! @parameter rv_r3trans | Version
    CLASS-METHODS get_r3trans_info
      RETURNING VALUE(rv_r3trans) TYPE string
      RAISING   zcx_trm_exception.

    "! Add TMS buffer data to migration tables
    "! @parameter it_data | Data
    CLASS-METHODS add_migration_tmsbuffer
      IMPORTING it_data TYPE tyt_migration_tmsbuffer
      RAISING   zcx_trm_exception.

    "! Add documentation data to migration tables
    "! @parameter it_data | Data
    CLASS-METHODS add_migration_doktl
      IMPORTING it_data TYPE tyt_migration_doktl
      RAISING   zcx_trm_exception.

    "! Add E071 data to migration tables
    "! @parameter it_data | Data
    CLASS-METHODS add_migration_e071
      IMPORTING it_data TYPE tyt_migration_e071
      RAISING   zcx_trm_exception.

    "! Add E070 data to migration tables
    "! @parameter it_data | Data
    CLASS-METHODS add_migration_e070
      IMPORTING it_data TYPE tyt_migration_e070
      RAISING   zcx_trm_exception.

    "! Reads messages from memory (e.g., after SUBMIT) and appends them to the provided message table
    "! @parameter messages | Message table to append entries from memory
    CLASS-METHODS append_messages_from_memory
      CHANGING
        messages TYPE symsg_tab.


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

  METHOD remove_skip_trkorr.
    enqueue( iv_tabname = 'ZTRM_SKIP_TRKORR' ).
    DELETE FROM ztrm_skip_trkorr WHERE trkorr EQ iv_trkorr.
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

  METHOD remove_source_trkorr.
    enqueue( iv_tabname = 'ZTRM_SRC_TRKORR' ).
    DELETE FROM ztrm_src_trkorr WHERE trkorr EQ iv_trkorr.
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
    DATA: lt_lang_objects LIKE et_object_text,
          ls_lang_objects LIKE LINE OF lt_lang_objects.
    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = et_object_text
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.

    " add LANG supported objects
    MOVE et_object_text[] TO lt_lang_objects[].
    DELETE lt_lang_objects WHERE pgmid <> 'R3TR' AND pgmid <> 'LIMU' OR object = 'ADIR'.
    LOOP AT lt_lang_objects INTO ls_lang_objects.
      ls_lang_objects-pgmid = 'LANG'.
      READ TABLE et_object_text TRANSPORTING NO FIELDS WITH KEY pgmid = ls_lang_objects-pgmid object = ls_lang_objects-object.
      CHECK sy-subrc <> 0.
      APPEND ls_lang_objects TO et_object_text.
    ENDLOOP.
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

  METHOD add_namespace.
    DATA: ls_trnspacet  TYPE trnspacet,
          ls_trnspacett TYPE trnspacett.

    ls_trnspacet-namespace = iv_namespace.
    ls_trnspacet-role       = 'C'.
    ls_trnspacet-changeuser = sy-uname.
    ls_trnspacet-changedate = sy-datum.
    ls_trnspacet-replicense = iv_replicense.
    INSERT trnspacet FROM ls_trnspacet.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( iv_reason = zcx_trm_exception=>c_reason-insert_error ).
    ENDIF.

    LOOP AT it_texts INTO ls_trnspacett.
      CHECK ls_trnspacett-spras IS NOT INITIAL.
      ls_trnspacett-namespace = iv_namespace.
      INSERT trnspacett FROM ls_trnspacett.
      IF sy-subrc <> 0.
        zcx_trm_exception=>raise( iv_reason = zcx_trm_exception=>c_reason-insert_error ).
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'TR_ACTIVATE_NAMESPACE'
      EXPORTING
        iv_namespace         = iv_namespace
      EXCEPTIONS
        deletion_not_allowed = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( ).
    ENDIF.

    UPDATE trnspace SET editflag = 'X' WHERE namespace = iv_namespace.

    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD get_r3trans_info.
    TYPES: ty_cmd TYPE c LENGTH 254,
           BEGIN OF ty_result,
             line(255),
           END OF ty_result.
    DATA: cmd    TYPE ty_cmd,
          result TYPE STANDARD TABLE OF ty_result.
    cmd = 'R3trans'.
    CALL 'SYSTEM' ID 'COMMAND' FIELD cmd
                  ID 'TAB'     FIELD result.
    IF sy-subrc <> 12. "Fatal errors have occurred, R3trans sets to 12 when running without options
      zcx_trm_exception=>raise( iv_reason = zcx_trm_exception=>c_reason-r3trans_cmd_error ).
    ENDIF.

    CONCATENATE LINES OF result INTO rv_r3trans SEPARATED BY cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD add_migration_tmsbuffer.
    enqueue( iv_tabname = 'ZTRM_TMSBUFFER' ).
    MODIFY ztrm_tmsbuffer FROM TABLE it_data.
    COMMIT WORK AND WAIT.
    dequeue( iv_tabname = 'ZTRM_TMSBUFFER' ).
  ENDMETHOD.

  METHOD add_migration_doktl.
    enqueue( iv_tabname = 'ZTRM_DOKTL' ).
    MODIFY ztrm_doktl FROM TABLE it_data.
    COMMIT WORK AND WAIT.
    dequeue( iv_tabname = 'ZTRM_DOKTL' ).
  ENDMETHOD.

  METHOD add_migration_e071.
    enqueue( iv_tabname = 'ZTRM_E071' ).
    MODIFY ztrm_e071 FROM TABLE it_data.
    COMMIT WORK AND WAIT.
    dequeue( iv_tabname = 'ZTRM_E071' ).
  ENDMETHOD.

  METHOD add_migration_e070.
    enqueue( iv_tabname = 'ZTRM_E070' ).
    MODIFY ztrm_e070 FROM TABLE it_data.
    COMMIT WORK AND WAIT.
    dequeue( iv_tabname = 'ZTRM_E070' ).
  ENDMETHOD.

  METHOD append_messages_from_memory.
    DATA: lt_list_tab  TYPE TABLE OF abaplist,
          lt_ascii_tab TYPE soli_tab,
          ls_ascii     LIKE LINE OF lt_ascii_tab,
          lv_lines     TYPE i,
          ls_message   LIKE LINE OF messages.
    FIELD-SYMBOLS <fs_msg> TYPE symsg.
    CALL FUNCTION 'LIST_FROM_MEMORY'
      TABLES
        listobject = lt_list_tab
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    CALL FUNCTION 'LIST_FREE_MEMORY'
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.
    CALL FUNCTION 'LIST_TO_ASCI'
      TABLES
        listasci           = lt_ascii_tab
        listobject         = lt_list_tab
      EXCEPTIONS
        empty_list         = 1
        list_index_invalid = 2
        error_message      = 3
        OTHERS             = 4.
    DESCRIBE TABLE lt_ascii_tab LINES lv_lines.
    IF lv_lines GE 3. " remove report header
      READ TABLE lt_ascii_tab INTO ls_ascii INDEX 2.
      IF '-' CO ls_ascii-line.
        DELETE lt_ascii_tab FROM 1 TO 3.
      ENDIF.
    ENDIF.
    CLEAR ls_ascii.
    LOOP AT lt_ascii_tab INTO ls_ascii.
      CHECK ls_ascii-line IS NOT INITIAL.
      CLEAR ls_message.
      CONDENSE ls_ascii-line.
      cl_message_helper=>set_msg_vars_for_clike( ls_ascii-line ).
      MOVE-CORRESPONDING sy TO ls_message.
      ls_message-msgty = 'I'.
      READ TABLE messages TRANSPORTING NO FIELDS WITH KEY table_line = ls_message.
      CHECK sy-subrc <> 0.
      APPEND ls_message TO messages.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

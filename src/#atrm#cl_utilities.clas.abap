CLASS /atrm/cl_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: tyt_ko100       TYPE STANDARD TABLE OF ko100 WITH DEFAULT KEY,
           tyt_tadir       TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY,
           tyt_installdevc TYPE STANDARD TABLE OF /atrm/instdevc WITH DEFAULT KEY,
           tyt_trnspacett  TYPE STANDARD TABLE OF trnspacett WITH DEFAULT KEY.

    "! Check if current user is authorized to execute TRM functions
    "! @parameter authorized | 'X' if authorized
    CLASS-METHODS check_functions_authorization
      RETURNING VALUE(authorized) TYPE flag.

    "! Get a binary file from the application server
    "! @parameter file_path | Absolute path to the binary file
    "! @parameter file | File content as xstring
    CLASS-METHODS get_binary_file
      IMPORTING file_path TYPE string
      EXPORTING file      TYPE xstring
      RAISING   /atrm/cx_exception.

    "! Write a binary file to the application server
    "! @parameter file_path | Absolute path to the binary file
    "! @parameter file | File content as xstring
    CLASS-METHODS write_binary_file
      IMPORTING file_path TYPE string
                file      TYPE xstring
      RAISING   /atrm/cx_exception.

    "! Get the DIR_TRANS path from system profile
    "! @parameter dir_trans | DIR_TRANS path
    CLASS-METHODS get_dir_trans
      EXPORTING dir_trans TYPE pfevalue
      RAISING   /atrm/cx_exception.

    "! Get the current operating system's file system
    "! @parameter file_sys | OS
    CLASS-METHODS get_file_sys
      EXPORTING file_sys TYPE filesys
      RAISING   /atrm/cx_exception.

    "! Get the default transport layer
    "! @parameter layer | Transport layer
    CLASS-METHODS get_default_transport_layer
      EXPORTING layer TYPE devlayer
      RAISING   /atrm/cx_exception.

    "! Get supported object types from transport configuration
    "! @parameter object_text | Supported object types
    CLASS-METHODS get_supported_object_types
      EXPORTING object_text TYPE tyt_ko100
      RAISING   /atrm/cx_exception.

    "! Add install devclasses into TRM install devclass list
    "! @parameter installdevc | Devclasses to add
    CLASS-METHODS add_install_devclass
      IMPORTING installdevc TYPE tyt_installdevc
      RAISING   /atrm/cx_exception.

    "! Wrapper for TR_TADIR_INTERFACE to register objects in the TADIR table
    "! @parameter pgmid     | Program ID
    "! @parameter object    | Object type
    "! @parameter objname   | Name of the object
    "! @parameter devclass  | (Optional) Development class the object belongs to
    "! @parameter srcsystem | (Optional) Logical system the object originates from
    "! @parameter author    | (Optional) Author of the object
    "! @parameter genflag   | (Optional) Generation flag (X = generated object)
    CLASS-METHODS tadir_interface
      IMPORTING pgmid     TYPE pgmid
                object    TYPE trobjtype
                objname   TYPE sobj_name
                devclass  TYPE devclass OPTIONAL
                srcsystem TYPE srcsystem OPTIONAL
                author    TYPE responsibl OPTIONAL
                genflag   TYPE genflag OPTIONAL
      RAISING   /atrm/cx_exception.

    "! Add and activate a custom namespace entry in the system
    "! @parameter namespace  | Namespace key to be added
    "! @parameter replicense | Repair license key for the namespace
    "! @parameter texts      | Text descriptions for the namespace (multilingual)
    CLASS-METHODS add_namespace
      IMPORTING namespace  TYPE namespace
                replicense TYPE trnlicense
                texts      TYPE tyt_trnspacett
      RAISING   /atrm/cx_exception.

    "! Get R3trans utility version
    "! @parameter r3trans | Version
    CLASS-METHODS get_r3trans_info
      RETURNING VALUE(r3trans) TYPE string
      RAISING   /atrm/cx_exception.

    "! Reads messages from memory (e.g., after SUBMIT) and appends them to the provided message table
    "! @parameter messages | Message table to append entries from memory
    CLASS-METHODS append_messages_from_memory
      CHANGING
        messages TYPE symsg_tab.

    "! Returns for all objects the transport request that locks them, if any
    "! @parameter objects               | TADIR key table to check for locks
    "! @parameter locks                 | Table with object keys and corresponding lock
    CLASS-METHODS get_objs_locks
      IMPORTING objects      TYPE tyt_tadir
      RETURNING VALUE(locks) TYPE /atrm/object_lock_t.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS enqueue
      IMPORTING mode_rstable TYPE enqmode DEFAULT 'E'
                tabname      TYPE tabname
      RAISING   /atrm/cx_exception.
    CLASS-METHODS dequeue
      IMPORTING mode_rstable TYPE enqmode DEFAULT 'E'
                tabname      TYPE tabname
      RAISING   /atrm/cx_exception.
ENDCLASS.



CLASS /atrm/cl_utilities IMPLEMENTATION.

  METHOD check_functions_authorization.
    CLEAR authorized.
    SELECT COUNT( * ) FROM /atrm/users WHERE uname = sy-uname.
    IF sy-dbcnt EQ 1.
      authorized = 'X'.
    ENDIF.
  ENDMETHOD.

  METHOD enqueue.
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        mode_rstable   = mode_rstable
        tabname        = tabname
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( iv_reason  = /atrm/cx_exception=>c_reason-enqueue_error ).
    ENDIF.
  ENDMETHOD.

  METHOD dequeue.
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable   = mode_rstable
        tabname        = tabname
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( iv_reason  = /atrm/cx_exception=>c_reason-dequeue_error ).
    ENDIF.
  ENDMETHOD.

  METHOD get_binary_file.
    OPEN DATASET file_path FOR INPUT IN BINARY MODE.
    READ DATASET file_path INTO file.
    CLOSE DATASET file_path.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD write_binary_file.
    OPEN DATASET file_path FOR OUTPUT IN BINARY MODE.
    TRANSFER file TO file_path.
    CLOSE DATASET file_path.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
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
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_file_sys.
    SELECT SINGLE filesys INTO file_sys FROM opsystem WHERE opsys = sy-opsys.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( iv_message = 'File system not found'
                                iv_reason  = /atrm/cx_exception=>c_reason-not_found ).
    ENDIF.
  ENDMETHOD.

  METHOD get_default_transport_layer.
    CALL FUNCTION 'TR_GET_TRANSPORT_TARGET'
      EXPORTING
        iv_use_default    = 'X'
        iv_get_layer_only = 'X'
      IMPORTING
        ev_layer          = layer
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_supported_object_types.
    DATA: lt_lang_objects LIKE object_text,
          ls_lang_objects LIKE LINE OF lt_lang_objects.
    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = object_text
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.

    " add LANG supported objects
    MOVE object_text[] TO lt_lang_objects[].
    DELETE lt_lang_objects WHERE pgmid <> 'R3TR' AND pgmid <> 'LIMU' OR object = 'ADIR'.
    LOOP AT lt_lang_objects INTO ls_lang_objects.
      ls_lang_objects-pgmid = 'LANG'.
      READ TABLE object_text TRANSPORTING NO FIELDS WITH KEY pgmid = ls_lang_objects-pgmid object = ls_lang_objects-object.
      CHECK sy-subrc <> 0.
      APPEND ls_lang_objects TO object_text.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_install_devclass.
    DATA ls_installdevc LIKE LINE OF installdevc.
    enqueue( tabname = '/ATRM/INSTDEVC' ).
    LOOP AT installdevc INTO ls_installdevc.
      MODIFY /atrm/instdevc FROM ls_installdevc.
    ENDLOOP.
    COMMIT WORK AND WAIT.
    dequeue( tabname = '/ATRM/INSTDEVC' ).
  ENDMETHOD.

  METHOD tadir_interface.
    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus      = ' '
        wi_tadir_pgmid     = pgmid
        wi_tadir_object    = object
        wi_tadir_obj_name  = objname
        wi_tadir_devclass  = devclass
        wi_tadir_srcsystem = srcsystem
        wi_tadir_author    = author
        wi_set_genflag     = genflag
*       iv_no_pak_check    = 'X'
      EXCEPTIONS
        OTHERS             = 1.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD add_namespace.
    DATA: ls_trnspacet  TYPE trnspacet,
          ls_trnspacett TYPE trnspacett.

    ls_trnspacet-namespace = namespace.
    ls_trnspacet-role       = 'C'.
    ls_trnspacet-changeuser = sy-uname.
    ls_trnspacet-changedate = sy-datum.
    ls_trnspacet-replicense = replicense.
    INSERT trnspacet FROM ls_trnspacet.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( iv_reason = /atrm/cx_exception=>c_reason-insert_error ).
    ENDIF.

    LOOP AT texts INTO ls_trnspacett.
      CHECK ls_trnspacett-spras IS NOT INITIAL.
      ls_trnspacett-namespace = namespace.
      INSERT trnspacett FROM ls_trnspacett.
      IF sy-subrc <> 0.
        /atrm/cx_exception=>raise( iv_reason = /atrm/cx_exception=>c_reason-insert_error ).
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'TR_ACTIVATE_NAMESPACE'
      EXPORTING
        iv_namespace         = namespace
      EXCEPTIONS
        deletion_not_allowed = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.

    UPDATE trnspace SET editflag = 'X' WHERE namespace = namespace.

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
      /atrm/cx_exception=>raise( iv_reason = /atrm/cx_exception=>c_reason-r3trans_cmd_error ).
    ENDIF.

    CONCATENATE LINES OF result INTO r3trans SEPARATED BY cl_abap_char_utilities=>newline.
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

  METHOD get_objs_locks.
    TYPES: BEGIN OF ty_aux,
             pgmid    TYPE e071-pgmid,
             object   TYPE e071-object,
             obj_name TYPE e071-obj_name,
             trkorr   TYPE e070-trkorr,
             strkorr  TYPE e070-strkorr,
           END OF ty_aux.
    DATA: ls_object      LIKE LINE OF objects,
          lt_objects_aux TYPE STANDARD TABLE OF ty_aux,
          lt_result_aux  TYPE STANDARD TABLE OF ty_aux,
          ls_result_aux  LIKE LINE OF lt_result_aux.
    FIELD-SYMBOLS: <fs_object_aux> TYPE ty_aux,
                   <fs_obj_lock>   TYPE /atrm/object_lock.
    LOOP AT objects INTO ls_object.
      UNASSIGN <fs_object_aux>.
      APPEND INITIAL LINE TO lt_objects_aux ASSIGNING <fs_object_aux>.
      MOVE-CORRESPONDING ls_object TO <fs_object_aux>.
    ENDLOOP.
    CHECK lt_objects_aux[] IS NOT INITIAL.
    SELECT pgmid object obj_name trkorr strkorr
      FROM /atrm/v_obj_lock
      INTO CORRESPONDING FIELDS OF TABLE lt_result_aux
      FOR ALL ENTRIES IN lt_objects_aux
      WHERE pgmid EQ lt_objects_aux-pgmid
        AND object EQ lt_objects_aux-object
        AND obj_name EQ lt_objects_aux-obj_name
        AND ( trstatus EQ 'D' OR trstatus EQ 'L' ).
    LOOP AT lt_result_aux INTO ls_result_aux.
      UNASSIGN <fs_obj_lock>.
      READ TABLE locks TRANSPORTING NO FIELDS WITH KEY pgmid = ls_result_aux-pgmid object = ls_result_aux-object obj_name = ls_result_aux-obj_name.
      CHECK sy-subrc <> 0.
      APPEND INITIAL LINE TO locks ASSIGNING <fs_obj_lock>.
      <fs_obj_lock>-pgmid = ls_result_aux-pgmid.
      <fs_obj_lock>-object = ls_result_aux-object.
      <fs_obj_lock>-obj_name = ls_result_aux-obj_name.
      IF ls_result_aux-strkorr IS NOT INITIAL.
        <fs_obj_lock>-trkorr = ls_result_aux-strkorr.
      ELSE.
        <fs_obj_lock>-trkorr = ls_result_aux-trkorr.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

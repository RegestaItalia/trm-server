"! Execute checks before exporting transport requests
CLASS zcl_trm_before_export DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: tyt_skip TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    "! Constructor
    "! @parameter it_skip | Table of checks to skip
    "! @parameter io_transport | Reference to the transport object
    METHODS constructor
      IMPORTING it_skip      TYPE tyt_skip
                io_transport TYPE REF TO zcl_trm_transport.

    "! Eentry point, executes checks
    "! @raising zcx_trm_exception | Exception
    "! @parameter et_messages | Messages from processing
    METHODS run
      EXPORTING et_messages TYPE symsg_tab
      RAISING   zcx_trm_exception.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: gt_skip      TYPE tyt_skip,
          go_transport TYPE REF TO zcl_trm_transport,
          gt_e071      TYPE zcl_trm_transport=>tyt_e071,
          gt_e071k     TYPE zcl_trm_transport=>tyt_e071k.

    METHODS stree
      CHANGING messages TYPE symsg_tab.

ENDCLASS.



CLASS zcl_trm_before_export IMPLEMENTATION.

  METHOD constructor.
    gt_skip[] = it_skip[].
    gt_e071 = go_transport->get_e071( ).
    gt_e071k = go_transport->get_e071k( ).
  ENDMETHOD.

  METHOD run.
    READ TABLE gt_skip TRANSPORTING NO FIELDS WITH KEY table_line = 'STREE'.
    IF sy-subrc <> 0.
      stree( CHANGING messages = et_messages ).
    ENDIF.
  ENDMETHOD.

  METHOD stree.
    DATA: lt_log       TYPE STANDARD TABLE OF trlogm,
          ls_log       LIKE LINE OF lt_log,
          ls_message   LIKE LINE OF messages,
          lv_run       TYPE flag,
          lt_selection TYPE rsparams_tt,
          ls_selection LIKE LINE OF lt_selection.

    CLEAR lv_run.
    CALL FUNCTION 'STREE_BEFORE_EXPORT'
      EXPORTING
        iv_client = sy-mandt
      TABLES
        tt_e071   = gt_e071
        tt_e071k  = gt_e071k.
    CLEAR lt_log[].
    IMPORT lt_log = lt_log FROM MEMORY  ID 'APPEND_LOG'.

    CLEAR ls_log.
    LOOP AT lt_log INTO ls_log WHERE severity = 'E' OR severity = 'A'.
      CLEAR ls_message.
      ls_message-msgid = ls_log-ag.
      ls_message-msgno = ls_log-msgnr.
      ls_message-msgty = 'W'.
      ls_message-msgv1 = ls_log-var1.
      ls_message-msgv2 = ls_log-var2.
      ls_message-msgv3 = ls_log-var3.
      ls_message-msgv4 = ls_log-var4.
      APPEND ls_message TO messages.
      lv_run = 'X'.
    ENDLOOP.

    CHECK lv_run EQ 'X'.

    CLEAR lt_selection.
    CLEAR ls_selection.
    ls_selection-selname ='P_TRKORR'.
    ls_selection-kind = 'P'.
    ls_selection-low = go_transport->get_trkorr( ).
    APPEND ls_selection TO lt_selection.
    CLEAR ls_selection.
    ls_selection-selname ='P_EXT'.
    ls_selection-kind = 'P'.
    ls_selection-low = 'X'.
    APPEND ls_selection TO lt_selection.

    SUBMIT rs_stree_objects_to_req_get WITH SELECTION-TABLE lt_selection EXPORTING LIST TO MEMORY AND RETURN.
    zcl_trm_utility=>append_messages_from_memory(
      CHANGING
        messages = messages
    ).
  ENDMETHOD.

ENDCLASS.

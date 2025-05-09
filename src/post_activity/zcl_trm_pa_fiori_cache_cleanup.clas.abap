"! Executes a full cleanup of Fiori/UI5-related caches after transport import
"!
"! Runs a series of reports including:
"!
"!  - /UI2/DELETE_CACHE_AFTER_IMP
"!
"!  - /UI2/INVALIDATE_CLIENT_CACHES
"!
"!  - /UI5/APP_INDEX_CALCULATE
"!
"!  - /UI5/DEL_ODATA_METADATA_CACHE
"!
"!  - /IWFND/R_MED_CACHE_CLEANUP
"!
"!  - /IWBEP/R_MGW_MED_CACHE_CLEANUP
CLASS zcl_trm_pa_fiori_cache_cleanup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS trm_pa TYPE flag VALUE 'X' ##NO_TEXT.

    TYPES: ty_ui5_repository_ui   TYPE c LENGTH 30.

    "! @parameter ui5_repository | (Optional) UI5 repository name to restrict calculation to
    "! @parameter messages       | Message table returned from cleanup steps
    "! @raising zcx_trm_exception | Raised if any report fails or invalid selections are detected
    CLASS-METHODS execute
      IMPORTING
        !ui5_repository TYPE ty_ui5_repository_ui OPTIONAL
      EXPORTING
        !messages       TYPE symsg_tab
      RAISING
        zcx_trm_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS submit
      IMPORTING report    TYPE raldb_repo
                selection TYPE rsparams_tt OPTIONAL
      CHANGING  messages  TYPE symsg_tab
      RAISING   zcx_trm_exception.
ENDCLASS.



CLASS zcl_trm_pa_fiori_cache_cleanup IMPLEMENTATION.

  METHOD execute.
    DATA: lt_seltab    TYPE rsparams_tt,
          ls_selection LIKE LINE OF lt_seltab,
          ls_message   LIKE LINE OF messages.

    CLEAR lt_seltab.
    submit(
      EXPORTING
        report    = '/UI2/DELETE_CACHE_AFTER_IMP'
        selection = lt_seltab
      CHANGING
        messages  = messages
    ).

    CLEAR lt_seltab.
    CLEAR ls_selection.
    ls_selection-selname ='GV_ALL'.
    ls_selection-kind = 'P'.
    ls_selection-low = 'X'.
    APPEND ls_selection TO lt_seltab.
    submit(
      EXPORTING
        report    = '/UI2/INVALIDATE_CLIENT_CACHES'
        selection = lt_seltab
      CHANGING
        messages  = messages
    ).

    CLEAR lt_seltab.
    IF ui5_repository IS INITIAL.
      CLEAR ls_message.
      cl_message_helper=>set_msg_vars_for_clike( 'Full UI5 repository calculation!' ).
      MOVE-CORRESPONDING sy TO ls_message.
      ls_message-msgty = 'W'.
      APPEND ls_message TO messages.
      CLEAR ls_selection.
      ls_selection-selname ='P_ALL'.
      ls_selection-kind = 'P'.
      ls_selection-low = 'X'.
      APPEND ls_selection TO lt_seltab.
      CLEAR ls_selection.
      ls_selection-selname ='P_REPO'.
      ls_selection-kind = 'P'.
      APPEND ls_selection TO lt_seltab.
    ELSE.
      CLEAR ls_selection.
      ls_selection-selname ='P_ALL'.
      ls_selection-kind = 'P'.
      ls_selection-low = ' '.
      APPEND ls_selection TO lt_seltab.
      CLEAR ls_selection.
      ls_selection-selname ='P_REPO'.
      ls_selection-kind = 'P'.
      ls_selection-low = ui5_repository.
      APPEND ls_selection TO lt_seltab.
    ENDIF.
    CLEAR ls_selection.
    ls_selection-selname ='P_DISTL'.
    ls_selection-kind = 'P'.
    APPEND ls_selection TO lt_seltab.
    submit(
      EXPORTING
        report    = '/UI5/APP_INDEX_CALCULATE'
        selection = lt_seltab
      CHANGING
        messages  = messages
    ).

    CLEAR lt_seltab.
    submit(
      EXPORTING
        report    = '/UI5/DEL_ODATA_METADATA_CACHE'
        selection = lt_seltab
      CHANGING
        messages  = messages
    ).

    CLEAR lt_seltab.
    CLEAR ls_selection.
    ls_selection-selname ='ALLPROXY'.
    ls_selection-kind = 'P'.
    ls_selection-low = 'X'.
    APPEND ls_selection TO lt_seltab.
    CLEAR ls_selection.
    ls_selection-selname ='ALLMODEL'.
    ls_selection-kind = 'P'.
    ls_selection-low = 'X'.
    APPEND ls_selection TO lt_seltab.
    submit(
      EXPORTING
        report    = '/IWFND/R_MED_CACHE_CLEANUP'
        selection = lt_seltab
      CHANGING
        messages  = messages
    ).

    CLEAR lt_seltab.
    CLEAR ls_selection.
    ls_selection-selname ='ALLMODEL'.
    ls_selection-kind = 'P'.
    ls_selection-low = 'X'.
    APPEND ls_selection TO lt_seltab.
    submit(
      EXPORTING
        report    = '/IWBEP/R_MGW_MED_CACHE_CLEANUP'
        selection = lt_seltab
      CHANGING
        messages  = messages
    ).
  ENDMETHOD.

  METHOD submit.
    DATA: lv_report    LIKE report,
          lv_message   TYPE string,
          ls_selection LIKE LINE OF selection,
          lt_seltab    TYPE rsparams_tt.
    lv_report = report.
    TRANSLATE lv_report TO UPPER CASE.
    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = lv_report
      TABLES
        selection_table = lt_seltab
      EXCEPTIONS
        not_found       = 1
        no_report       = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      zcx_trm_exception=>raise( iv_reason = zcx_trm_exception=>c_reason-pa_exception ).
    ENDIF.
    LOOP AT selection INTO ls_selection.
      READ TABLE lt_seltab TRANSPORTING NO FIELDS WITH KEY selname = ls_selection-selname kind = ls_selection-kind.
      IF sy-subrc <> 0.
        CONCATENATE 'Report' lv_report 'selection' ls_selection-selname 'kind' ls_selection-kind 'not allowed' INTO lv_message SEPARATED BY space.
        zcx_trm_exception=>raise( iv_message = lv_message
                                  iv_reason = zcx_trm_exception=>c_reason-pa_exception ).
      ENDIF.
    ENDLOOP.
    SUBMIT (lv_report) WITH SELECTION-TABLE selection EXPORTING LIST TO MEMORY AND RETURN.
    zcl_trm_utility=>append_messages_from_memory(
      CHANGING
        messages = messages
    ).
  ENDMETHOD.

ENDCLASS.

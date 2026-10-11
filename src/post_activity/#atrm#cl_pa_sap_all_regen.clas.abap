"! Regenerate SAP_ALL for all clients
"!
CLASS /atrm/cl_pa_sap_all_regen DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS trm_pa TYPE flag VALUE 'X' ##NO_TEXT.

    "! @raising /atrm/cx_exception | Raised if activation fails
    CLASS-METHODS execute
      EXPORTING
        !messages TYPE /atrm/symsg_tab
      RAISING
        /atrm/cx_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS submit
      IMPORTING report    TYPE raldb_repo
                selection TYPE rsparams_tt OPTIONAL
      CHANGING  messages  TYPE /atrm/symsg_tab
      RAISING   /atrm/cx_exception.
ENDCLASS.



CLASS /atrm/cl_pa_sap_all_regen IMPLEMENTATION.

  METHOD execute.
    submit(
       EXPORTING
         report    = 'AGR_REGENERATE_SAP_ALL'
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
      /atrm/cx_exception=>raise( iv_reason = /atrm/cx_exception=>c_reason-pa_exception ).
    ENDIF.
    LOOP AT selection INTO ls_selection.
      READ TABLE lt_seltab TRANSPORTING NO FIELDS WITH KEY selname = ls_selection-selname kind = ls_selection-kind.
      IF sy-subrc <> 0.
        CONCATENATE 'Report' lv_report 'selection' ls_selection-selname 'kind' ls_selection-kind 'not allowed' INTO lv_message SEPARATED BY space.
        /atrm/cx_exception=>raise( iv_message = lv_message
                                  iv_reason = /atrm/cx_exception=>c_reason-pa_exception ).
      ENDIF.
    ENDLOOP.
    SUBMIT (lv_report) WITH SELECTION-TABLE selection EXPORTING LIST TO MEMORY AND RETURN.
    /atrm/cl_utilities=>append_messages_from_memory(
      CHANGING
        messages = messages
    ).
  ENDMETHOD.

ENDCLASS.

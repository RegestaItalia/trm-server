CLASS zcl_trm_pa_fiori_cache_cleanup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    CONSTANTS trm_pa TYPE flag VALUE 'X' ##NO_TEXT.

    TYPES: ty_ui5_repository_ui   TYPE c LENGTH 30.

    CLASS-METHODS execute
      IMPORTING
        !ui5_repository TYPE ty_ui5_repository_ui OPTIONAL
      EXPORTING
        !messages       TYPE symsg_tab
      RAISING
        zcx_trm_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS append_messages_from_memory
      CHANGING
        messages TYPE symsg_tab.
ENDCLASS.



CLASS zcl_trm_pa_fiori_cache_cleanup IMPLEMENTATION.

  METHOD execute.
    SUBMIT /ui2/delete_cache_after_imp EXPORTING LIST TO MEMORY AND RETURN.
    append_messages_from_memory(
      CHANGING
        messages = messages
    ).

    SUBMIT /ui2/invalidate_client_caches WITH gv_all = 'X' EXPORTING LIST TO MEMORY AND RETURN.
    append_messages_from_memory(
      CHANGING
        messages = messages
    ).

    IF ui5_repository IS INITIAL.
      SUBMIT /ui5/app_index_calculate WITH p_all = 'X'
                                      WITH p_repo = ''
                                      WITH p_distl = ' ' EXPORTING LIST TO MEMORY AND RETURN.
    ELSE.
      SUBMIT /ui5/app_index_calculate WITH p_all = ' '
                                      WITH p_repo = ui5_repository
                                      WITH p_distl = ' ' EXPORTING LIST TO MEMORY AND RETURN.
    ENDIF.
    append_messages_from_memory(
      CHANGING
        messages = messages
    ).

    SUBMIT /ui5/del_odata_metadata_cache EXPORTING LIST TO MEMORY AND RETURN.
    append_messages_from_memory(
      CHANGING
        messages = messages
    ).

    SUBMIT /iwfnd/r_med_cache_cleanup WITH allproxy = 'X'
                                      WITH allmodel = 'X' EXPORTING LIST TO MEMORY AND RETURN.
    append_messages_from_memory(
      CHANGING
        messages = messages
    ).

    SUBMIT /iwbep/r_mgw_med_cache_cleanup WITH allmodel = 'X' EXPORTING LIST TO MEMORY AND RETURN.
    append_messages_from_memory(
      CHANGING
        messages = messages
    ).
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
      READ TABLE messages TRANSPORTING NO FIELDS WITH KEY table_line = ls_message.
      CHECK sy-subrc <> 0.
      APPEND ls_message TO messages.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.
    execute( ).
  ENDMETHOD.

ENDCLASS.

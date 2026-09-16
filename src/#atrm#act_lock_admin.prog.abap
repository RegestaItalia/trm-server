REPORT /atrm/act_lock_admin.

TYPE-POOLS slis.

TYPES: BEGIN OF ty_row,
         resource_type TYPE /atrm/act_lock-resource_type,
         resource_name TYPE /atrm/act_lock-resource_name,
         resource_hash TYPE /atrm/act_lock-resource_hash,
         action_name TYPE /atrm/act_lock-action_name,
         created_by TYPE /atrm/act_lock-created_by,
         created_at TYPE /atrm/act_lock-created_at,
         owner_token TYPE /atrm/act_lock-owner_token,
         delete_action TYPE c LENGTH 6,
       END OF ty_row.

DATA: gs_lock TYPE /atrm/act_lock,
      gt_locks TYPE STANDARD TABLE OF /atrm/act_lock WITH DEFAULT KEY,
      gs_row TYPE ty_row,
      gt_rows TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY,
      gs_fieldcat TYPE slis_fieldcat_alv,
      gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout TYPE slis_layout_alv.

SELECT-OPTIONS: s_type FOR gs_lock-resource_type,
                s_name FOR gs_lock-resource_name,
                s_hash FOR gs_lock-resource_hash,
                s_action FOR gs_lock-action_name,
                s_owner FOR gs_lock-created_by,
                s_time FOR gs_lock-created_at.

START-OF-SELECTION.
  AUTHORITY-CHECK OBJECT 'ZTRM_AUTH' ID 'ACTVT' FIELD '16'.
  IF sy-subrc <> 0.
    MESSAGE 'Not authorized to view locks' TYPE 'E'.
  ENDIF.

  PERFORM load_rows.
  PERFORM build_fieldcat.
  gs_layout-zebra = 'X'.
  gs_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      i_callback_user_command = 'ALV_USER_COMMAND'
      is_layout               = gs_layout
      it_fieldcat             = gt_fieldcat
      i_save                  = 'A'
    TABLES
      t_outtab                = gt_rows
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Could not display locks' TYPE 'E'.
  ENDIF.

FORM load_rows.
  CLEAR: gt_locks, gt_rows.
  SELECT * FROM /atrm/act_lock INTO TABLE gt_locks
    WHERE resource_type IN s_type
      AND resource_name IN s_name
      AND resource_hash IN s_hash
      AND action_name IN s_action
      AND created_by IN s_owner
      AND created_at IN s_time.
  SORT gt_locks BY created_at resource_type resource_hash.
  LOOP AT gt_locks INTO gs_lock.
    CLEAR gs_row.
    MOVE-CORRESPONDING gs_lock TO gs_row.
    gs_row-delete_action = '@11@'.
    APPEND gs_row TO gt_rows.
  ENDLOOP.
ENDFORM.

FORM add_field USING pv_field TYPE slis_fieldname
                     pv_text TYPE string
                     pv_len TYPE i
                     pv_hotspot TYPE c.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = pv_field.
  gs_fieldcat-seltext_m = pv_text.
  gs_fieldcat-outputlen = pv_len.
  gs_fieldcat-hotspot = pv_hotspot.
  APPEND gs_fieldcat TO gt_fieldcat.
ENDFORM.

FORM build_fieldcat.
  CLEAR gt_fieldcat.
  PERFORM add_field USING 'DELETE_ACTION' 'Delete' 6 'X'.
  PERFORM add_field USING 'RESOURCE_TYPE' 'Type' 16 space.
  PERFORM add_field USING 'RESOURCE_NAME' 'Resource' 60 space.
  PERFORM add_field USING 'ACTION_NAME' 'Action' 40 space.
  PERFORM add_field USING 'CREATED_BY' 'Owner' 12 space.
  PERFORM add_field USING 'CREATED_AT' 'Created at' 20 space.
  PERFORM add_field USING 'RESOURCE_HASH' 'Hash' 64 space.
ENDFORM.

FORM alv_user_command USING pv_ucomm LIKE sy-ucomm
                            ps_selfield TYPE slis_selfield.
  DATA lv_deleted TYPE abap_bool.
  IF pv_ucomm <> '&IC1' OR ps_selfield-fieldname <> 'DELETE_ACTION'.
    RETURN.
  ENDIF.
  READ TABLE gt_rows INTO gs_row INDEX ps_selfield-tabindex.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
  PERFORM delete_row USING gs_row CHANGING lv_deleted.
  IF lv_deleted = abap_true.
    PERFORM load_rows.
    ps_selfield-refresh = 'X'.
  ENDIF.
ENDFORM.

FORM delete_row USING ps_row TYPE ty_row
                CHANGING pv_deleted TYPE abap_bool.
  DATA: lv_answer TYPE c LENGTH 1,
        lv_message TYPE string,
        lo_error TYPE REF TO /atrm/cx_exception.
  pv_deleted = abap_false.
  IF sy-batch = 'X'.
    MESSAGE 'Manual lock deletion is only allowed interactively' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'ZTRM_AUTH' ID 'ACTVT' FIELD '16'.
  IF sy-subrc <> 0.
    MESSAGE 'Not authorized to delete locks' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'S_TABU_NAM'
    ID 'ACTVT' FIELD '02'
    ID 'TABLE' FIELD '/ATRM/ACT_LOCK'.
  IF sy-subrc <> 0.
    MESSAGE 'Not authorized to delete locks' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar = 'Delete TRM action lock'
      text_question = 'At your own risk: deleting an active lock can allow overlapping changes. Delete this row?'
      text_button_1 = 'Delete'
      text_button_2 = 'Cancel'
      default_button = '2'
      display_cancel_button = 'X'
    IMPORTING
      answer = lv_answer.
  IF lv_answer <> '1'.
    RETURN.
  ENDIF.
  TRY.
      /atrm/cl_action_lock=>force_delete(
        iv_resource_type = ps_row-resource_type
        iv_resource_hash = ps_row-resource_hash
        iv_owner_token = ps_row-owner_token ).
    CATCH /atrm/cx_exception INTO lo_error.
      lv_message = lo_error->get_text( ).
      MESSAGE lv_message TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.
  pv_deleted = abap_true.
  MESSAGE 'Lock deleted' TYPE 'S'.
ENDFORM.

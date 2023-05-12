FUNCTION ZTRM_DELETE_MANIFEST_LOCK.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_MANIFEST_TYPE) TYPE  I
*"  EXCEPTIONS
*"      UNSUPPORTED_TYPE
*"----------------------------------------------------------------------
  DATA lt_lock TYPE STANDARD TABLE OF seqg3.
  DATA lv_arg TYPE eqegraarg.

  IF iv_manifest_type EQ 1.
    lv_arg = 'SKTDZAPM*'.
  ELSEIF iv_manifest_type EQ 2.
    lv_arg = 'ZAPM*'.
  ELSE.
    RAISE unsupported_type.
  ENDIF.

  CALL FUNCTION 'ENQUE_READ'
    EXPORTING
      garg = lv_arg
    TABLES
      enq  = lt_lock.

  FIELD-SYMBOLS <fs_lock> TYPE seqg3.
  DATA lv_delete_line TYPE flag.

  "TODO clear lines that don't match regex (Z|Y|/.*/)APM(\d*)
  LOOP AT lt_lock ASSIGNING <fs_lock>.
    lv_delete_line = 'X'.
    IF <fs_lock>-gname EQ 'WBS_ENQUEUE_STRU'.
      IF iv_manifest_type EQ 1.
        CLEAR lv_delete_line.
      ENDIF.
    ELSEIF <fs_lock>-gname EQ 'SEOCLSENQ'.
      IF iv_manifest_type EQ 2.
        CLEAR lv_delete_line.
      ENDIF.
    ENDIF.

    IF lv_delete_line EQ 'X'.
      CLEAR <fs_lock>.
    ENDIF.
  ENDLOOP.
  DELETE lt_lock WHERE table_line IS INITIAL.

  IF lines( lt_lock ) GT 0.
    CALL FUNCTION 'ENQUE_DELETE'
      TABLES
        enq = lt_lock.
  ENDIF.


ENDFUNCTION.

CLASS /atrm/cx_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PROTECTED .

  PUBLIC SECTION.
    INTERFACES if_t100_message.

    TYPES: tyt_log TYPE STANDARD TABLE OF tdline WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF c_reason,
        generic                  TYPE string VALUE 'GENERIC',
        invalid_input            TYPE string VALUE 'INVALID_INPUT',
        enqueue_error            TYPE string VALUE 'ENQUEUE_ERROR',
        dequeue_error            TYPE string VALUE 'DEQUEUE_ERROR',
        dyn_call_param_not_found TYPE string VALUE 'DYN_CALL_PARAM_NOT_FOUND',
        not_found                TYPE string VALUE 'NOT_FOUND',
        tms_alert                TYPE string VALUE 'TMS_ALERT',
        insert_error             TYPE string VALUE 'INSERT_ERROR',
        r3trans_cmd_error        TYPE string VALUE 'R3TRANS_CMD_ERROR',
        snro_interval_not_found  TYPE string VALUE 'SNRO_INTERVAL_NOT_FOUND',
        abapgit_data_error       TYPE string VALUE 'ABAPGIT_DATA_ERROR',
        abapgit_intergration     TYPE string VALUE 'ABAPGIT_INTEGRATION',
        pa_dynamic               TYPE string VALUE 'PA_DYNAMIC',
        pa_not_found             TYPE string VALUE 'PA_NOT_FOUND',
        pa_param_missing         TYPE string VALUE 'PA_PARAM_MISSING',
        pa_unexpected_param      TYPE string VALUE 'PA_UNEXPECTED_PARAM',
        pa_exception             TYPE string VALUE 'PA_EXCEPTION',
        program_not_found        TYPE string VALUE 'PROGRAM_NOT_FOUND',
        package_not_temporary    TYPE string VALUE 'PACKAGE_NOT_TEMPORARY',
      END OF c_reason .

    "! Constructor
    "! Initializes the exception with optional text ID and root cause
    "! @parameter textid | Optional message class ID
    "! @parameter previous | Optional reference to previous (inner) exception
    METHODS constructor
      IMPORTING
        !textid   LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL.

    "! Returns the stored reason code for the exception
    "! @parameter rv_reason | One of the defined constants in `c_reason`, or 'GENERIC' by default
    METHODS reason
      RETURNING VALUE(rv_reason) TYPE string.

    "! Returns the exception log, followed by the exception stack
    "! @parameter rt_log | Table of log lines associated with the exception
    METHODS log
      RETURNING VALUE(rt_log) TYPE tyt_log.

    "! Returns the exception stack only
    "! @parameter rt_stack | Call stack at raise time and chain of wrapped exceptions
    METHODS stack
      RETURNING VALUE(rt_stack) TYPE tyt_log.

    "! Factory method to raise a /atrm/cx_exception with optional context
    "! @parameter iv_message | Optional plain-text message (used if no root exception given)
    "! @parameter io_root    | Optional root exception to wrap (e.g., CX_ROOT or subclass)
    "! @parameter iv_reason  | Optional reason identifier (use `c_reason-*`)
    "! @parameter it_log     | Optional detailed message log for diagnostics
    "! @raising /atrm/cx_exception | Always raises itself
    CLASS-METHODS raise
      IMPORTING iv_message TYPE string OPTIONAL
                io_root    TYPE REF TO cx_root OPTIONAL
                iv_reason  TYPE string OPTIONAL
                it_log     TYPE tyt_log OPTIONAL
      RAISING   /atrm/cx_exception.

    DATA: message TYPE symsg READ-ONLY.
  PROTECTED SECTION.
    DATA: gv_reason TYPE string,
          gt_log    TYPE tyt_log,
          gt_stack  TYPE tyt_log.
  PRIVATE SECTION.
    CLASS-METHODS get_call_stack
      RETURNING VALUE(rt_stack) TYPE tyt_log.
    CLASS-METHODS get_caused_by
      IMPORTING io_root         TYPE REF TO cx_root
      RETURNING VALUE(rt_stack) TYPE tyt_log.
    CLASS-METHODS append_line
      IMPORTING iv_text TYPE string
      CHANGING  ct_log  TYPE tyt_log.

ENDCLASS.



CLASS /atrm/cx_exception IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    MOVE-CORRESPONDING sy TO me->message.
    if_t100_message~t100key-msgid = me->message-msgid.
    if_t100_message~t100key-msgno = me->message-msgno.
    if_t100_message~t100key-attr1 = me->message-msgv1.
    if_t100_message~t100key-attr2 = me->message-msgv2.
    if_t100_message~t100key-attr3 = me->message-msgv3.
    if_t100_message~t100key-attr4 = me->message-msgv4.
  ENDMETHOD.

  METHOD reason.
    IF gv_reason IS INITIAL.
      rv_reason = c_reason-generic.
    ELSE.
      rv_reason = gv_reason.
    ENDIF.
  ENDMETHOD.

  METHOD log.
    rt_log = gt_log.
    IF gt_stack IS NOT INITIAL.
      IF rt_log IS NOT INITIAL.
        APPEND INITIAL LINE TO rt_log.
      ENDIF.
      APPEND LINES OF gt_stack TO rt_log.
    ENDIF.
  ENDMETHOD.

  METHOD stack.
    rt_stack = gt_stack.
  ENDMETHOD.

  METHOD raise.
    DATA: lo_exc      TYPE REF TO /atrm/cx_exception,
          lo_root     TYPE REF TO cx_root,
          lo_trm_root TYPE REF TO /atrm/cx_exception,
          lt_caused   TYPE tyt_log,
          lv_dummy    TYPE string.
    IF io_root IS BOUND.
      lo_root = io_root.
      WHILE lo_root->previous IS BOUND.
        lo_root = lo_root->previous.
      ENDWHILE.
      TRY.
          lo_trm_root ?= lo_root.
        CATCH cx_sy_move_cast_error.
      ENDTRY.
      IF lo_trm_root IS BOUND.
        MESSAGE ID lo_trm_root->message-msgid
          TYPE 'I'
          NUMBER lo_trm_root->message-msgno
          WITH lo_trm_root->message-msgv1
               lo_trm_root->message-msgv2
               lo_trm_root->message-msgv3
               lo_trm_root->message-msgv4
          INTO lv_dummy.
      ELSE.
        cl_message_helper=>set_msg_vars_for_clike( lo_root->get_text( ) ).
      ENDIF.
    ELSEIF iv_message IS SUPPLIED.
      cl_message_helper=>set_msg_vars_for_clike( iv_message ).
    ENDIF.
    CREATE OBJECT lo_exc EXPORTING previous = lo_root.
    lo_exc->gv_reason = iv_reason.
    lo_exc->gt_log = it_log.
    "keep the diagnostics of a wrapped trm exception: its log and the stack
    "of where it was originally raised are more useful than the rethrow point
    IF lo_trm_root IS BOUND.
      IF lo_exc->gt_log IS INITIAL.
        lo_exc->gt_log = lo_trm_root->gt_log.
      ENDIF.
      lo_exc->gt_stack = lo_trm_root->gt_stack.
    ENDIF.
    IF lo_exc->gt_stack IS INITIAL.
      lo_exc->gt_stack = get_call_stack( ).
    ENDIF.
    IF io_root IS BOUND.
      lt_caused = get_caused_by( io_root ).
      APPEND LINES OF lt_caused TO lo_exc->gt_stack.
    ENDIF.
    RAISE EXCEPTION lo_exc.
  ENDMETHOD.

  METHOD get_call_stack.
    DATA: lt_callstack TYPE abap_callstack,
          ls_callstack LIKE LINE OF lt_callstack,
          lv_line      TYPE string,
          lv_lineno    TYPE c LENGTH 10.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = lt_callstack.

    APPEND 'Exception stack:' TO rt_stack.                  "#EC NOTEXT
    LOOP AT lt_callstack INTO ls_callstack.
      "skip the frames of this class (raise, get_call_stack)
      IF ls_callstack-mainprogram CP '/ATRM/CX_EXCEPTION*'.
        CONTINUE.
      ENDIF.
      lv_lineno = ls_callstack-line.
      CONDENSE lv_lineno.
      CONCATENATE '  at' ls_callstack-blocktype ls_callstack-blockname
        INTO lv_line SEPARATED BY space.
      CONCATENATE lv_line ' (' ls_callstack-include ':' lv_lineno ')'
        INTO lv_line.
      append_line( EXPORTING iv_text = lv_line CHANGING ct_log = rt_stack ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_caused_by.
    DATA: lo_exc     TYPE REF TO cx_root,
          lv_class   TYPE string,
          lv_text    TYPE string,
          lv_include TYPE syrepid,
          lv_source  TYPE i,
          lv_lineno  TYPE c LENGTH 10,
          lv_line    TYPE string.

    lo_exc = io_root.
    WHILE lo_exc IS BOUND.
      lv_class = cl_abap_classdescr=>get_class_name( lo_exc ).
      REPLACE FIRST OCCURRENCE OF '\CLASS=' IN lv_class WITH ''.
      lv_text = lo_exc->get_text( ).
      CONCATENATE 'Caused by:' lv_class lv_text
        INTO lv_line SEPARATED BY space.                    "#EC NOTEXT
      append_line( EXPORTING iv_text = lv_line CHANGING ct_log = rt_stack ).
      lo_exc->get_source_position(
        IMPORTING
          include_name = lv_include
          source_line  = lv_source
      ).
      IF lv_include IS NOT INITIAL.
        lv_lineno = lv_source.
        CONDENSE lv_lineno.
        CONCATENATE '  at' lv_include INTO lv_line SEPARATED BY space.
        CONCATENATE lv_line ':' lv_lineno INTO lv_line.
        append_line( EXPORTING iv_text = lv_line CHANGING ct_log = rt_stack ).
      ENDIF.
      lo_exc = lo_exc->previous.
    ENDWHILE.
  ENDMETHOD.

  METHOD append_line.
    "log lines are tdline (132 chars): split longer texts into consecutive lines
    DATA: lv_len    TYPE i,
          lv_off    TYPE i,
          lv_chunk  TYPE i,
          lv_line   TYPE tdline,
          lv_maxlen TYPE i.

    DESCRIBE FIELD lv_line LENGTH lv_maxlen IN CHARACTER MODE.
    lv_len = strlen( iv_text ).
    IF lv_len = 0.
      APPEND INITIAL LINE TO ct_log.
      RETURN.
    ENDIF.
    WHILE lv_off < lv_len.
      lv_chunk = lv_len - lv_off.
      IF lv_chunk > lv_maxlen.
        lv_chunk = lv_maxlen.
      ENDIF.
      lv_line = iv_text+lv_off(lv_chunk).
      APPEND lv_line TO ct_log.
      lv_off = lv_off + lv_chunk.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.

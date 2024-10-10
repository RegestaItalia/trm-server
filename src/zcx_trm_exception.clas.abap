CLASS zcx_trm_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PROTECTED .

  PUBLIC SECTION.
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
      END OF c_reason .

    METHODS constructor
      IMPORTING
        !textid   LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL.

    METHODS reason
      RETURNING VALUE(rv_reason) TYPE string.

    METHODS log
      RETURNING VALUE(rt_log) TYPE tyt_log.

    CLASS-METHODS raise
      IMPORTING iv_message TYPE string OPTIONAL
                iv_reason  TYPE string OPTIONAL
                it_log     TYPE tyt_log OPTIONAL
      RAISING   zcx_trm_exception.

    DATA: message TYPE symsg READ-ONLY.
  PROTECTED SECTION.
    DATA: gv_reason TYPE string,
          gt_log    TYPE tyt_log.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcx_trm_exception IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    MOVE-CORRESPONDING sy TO me->message.
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
  ENDMETHOD.

  METHOD raise.
    DATA lo_exc TYPE REF TO zcx_trm_exception.
    IF iv_message IS SUPPLIED.
      cl_message_helper=>set_msg_vars_for_clike( iv_message ).
    ENDIF.
    CREATE OBJECT lo_exc.
    lo_exc->gv_reason = iv_reason.
    lo_exc->gt_log = it_log.
    RAISE EXCEPTION lo_exc.
  ENDMETHOD.

ENDCLASS.

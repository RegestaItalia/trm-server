CLASS zcx_trm_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PROTECTED .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !textid    LIKE textid OPTIONAL
        !previous  LIKE previous OPTIONAL.

    METHODS reason
      RETURNING VALUE(rv_reason) TYPE string.

    CLASS-METHODS raise
      IMPORTING iv_message TYPE string
                iv_reason TYPE string OPTIONAL
      RAISING
        zcx_trm_exception.

  PROTECTED SECTION.
    DATA: gv_reason TYPE string.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcx_trm_exception IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
  ENDMETHOD.

  METHOD reason.
    rv_reason = gv_reason.
  ENDMETHOD.

  METHOD raise.
    DATA lo_exc TYPE REF TO zcx_trm_exception.
    cl_message_helper=>set_msg_vars_for_clike( iv_message ).
    CREATE OBJECT lo_exc.
    lo_exc->gv_reason = iv_reason.
    RAISE EXCEPTION lo_exc.
  ENDMETHOD.

ENDCLASS.

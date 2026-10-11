FUNCTION /atrm/get_exception_log.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      LOG STRUCTURE  TLINE
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  PERFORM check_auth.

  "go_exc is the function group global set by the last failed call
  "in this RFC session: read its log once, then clear it
  DATA: lt_log  TYPE /atrm/cx_exception=>tyt_log,
        ls_log  LIKE LINE OF lt_log,
        ls_line TYPE tline.

  IF go_exc IS BOUND.
    lt_log = go_exc->log( ).
    LOOP AT lt_log INTO ls_log.
      ls_line-tdline = ls_log.
      APPEND ls_line TO log.
    ENDLOOP.
    CLEAR go_exc.
  ENDIF.

ENDFUNCTION.

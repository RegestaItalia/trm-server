FUNCTION /atrm/execute_post_activity.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DATA) TYPE  XSTRING
*"     VALUE(PRE) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     VALUE(EXECUTE) TYPE  FLAG
*"  TABLES
*"      MESSAGES STRUCTURE  SYMSG
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      PA_NOT_FOUND
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      IF pre EQ 'X'.
        /atrm/cl_post_activity=>pre(
          EXPORTING
            data     = data
          IMPORTING
            messages = messages[]
            execute  = execute
        ).
      ELSE.
        /atrm/cl_post_activity=>execute(
          EXPORTING
            data     = data
          IMPORTING
            messages = messages[]
        ).
      ENDIF.
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

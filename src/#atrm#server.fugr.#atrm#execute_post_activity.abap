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
        CALL METHOD ('/ATRM/CL_POST_ACTIVITY')=>('PRE')
          EXPORTING
            data     = data
          IMPORTING
            messages = messages[]
            execute  = execute
        .
      ELSE.
        CALL METHOD ('/ATRM/CL_POST_ACTIVITY')=>('EXECUTE')
          EXPORTING
            data     = data
          IMPORTING
            messages = messages[]
        .
      ENDIF.
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

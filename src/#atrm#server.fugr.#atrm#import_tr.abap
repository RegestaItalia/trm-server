FUNCTION /atrm/import_tr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SYSTEM) TYPE  TMSSYSNAM
*"     VALUE(TRKORR) TYPE  TRKORR
*"     VALUE(TEST) TYPE  FLAG
*"  EXPORTING
*"     VALUE(TEST_RESULT) TYPE  XSTRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.
  DATA import TYPE stms_tp_import.

  TRY.
      CREATE OBJECT go_transport EXPORTING trkorr = trkorr.
      import = go_transport->import(
        EXPORTING
          system = system
          test   = test
      ).
      IF import IS NOT INITIAL.
        CALL TRANSFORMATION id
        SOURCE import = import
        RESULT XML test_result.
      ENDIF.
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

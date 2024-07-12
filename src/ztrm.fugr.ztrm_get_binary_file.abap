FUNCTION ztrm_get_binary_file.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_FILE_PATH) TYPE  STRING
*"  EXPORTING
*"     VALUE(EV_FILE) TYPE  XSTRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      zcl_trm_utility=>get_binary_file(
        EXPORTING
          iv_file_path = iv_file_path
        IMPORTING
          ev_file      = ev_file
      ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

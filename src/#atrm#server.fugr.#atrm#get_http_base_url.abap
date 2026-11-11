"! @parameter service | <p class="shorttext synchronized" lang="en">ICF service path (e.g. /ztrmserver), appended to the URL</p>
"! @parameter base_url | <p class="shorttext synchronized" lang="en">URL (protocol://host:port[/service])</p>
FUNCTION /ATRM/GET_HTTP_BASE_URL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SERVICE) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     VALUE(BASE_URL) TYPE  STRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      base_url = /atrm/cl_utilities=>get_http_base_url( service ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

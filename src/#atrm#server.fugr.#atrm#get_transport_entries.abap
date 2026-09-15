FUNCTION /atrm/get_transport_entries.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TRKORR) TYPE  TRKORR
*"  EXPORTING
*"     VALUE(ENTRIES) TYPE  XSTRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  TYPES: BEGIN OF ty_entries,
           e071   TYPE /atrm/cl_transport=>tyt_e071,
           tadir  TYPE scts_tadir,
           tdevc  TYPE /atrm/cl_core=>tyt_tdevc,
           tdevct TYPE /atrm/cl_core=>tyt_tdevct,
         END OF ty_entries.
  DATA ls_entries TYPE ty_entries.

  PERFORM check_auth.

  TRY.
      DATA lo_transport TYPE REF TO /atrm/cl_transport.
      CREATE OBJECT lo_transport EXPORTING trkorr = trkorr.
      lo_transport->get_entries(
        IMPORTING
          e071 = ls_entries-e071
          tadir = ls_entries-tadir
          tdevc = ls_entries-tdevc
          tdevct = ls_entries-tdevct
      ).

      CALL TRANSFORMATION id
        SOURCE entries = ls_entries
        RESULT XML entries.
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.
ENDFUNCTION.

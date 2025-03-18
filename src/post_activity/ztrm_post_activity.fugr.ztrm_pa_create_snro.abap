FUNCTION ztrm_pa_create_snro.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(OBJECT) TYPE  NROBJ
*"     REFERENCE(SUBOBJECT) TYPE  NRSOBJ
*"     REFERENCE(NRRANGENR) TYPE  NRNR
*"     REFERENCE(TOYEAR) TYPE  NRYEAR
*"     REFERENCE(FROMNUMBER) TYPE  NRFROM OPTIONAL
*"     REFERENCE(TONUMBER) TYPE  NRTO OPTIONAL
*"     REFERENCE(NRLEVEL) TYPE  NRLEVEL OPTIONAL
*"     REFERENCE(EXTERNIND) TYPE  NRIND OPTIONAL
*"     REFERENCE(PROCIND) TYPE  PROCIND OPTIONAL
*"     REFERENCE(TRANSPORT_REQUEST) TYPE  TRKORR OPTIONAL
*"  EXCEPTIONS
*"      PA_DYNAMIC
*"      GENERIC
*"----------------------------------------------------------------------
  DATA: ls_number_range TYPE zcl_trm_pa_number_range=>ty_number_range,
        ls_option       TYPE zcl_trm_pa_number_range=>ty_nr_option.
  ls_number_range-object = object.
  ls_number_range-subobject = subobject.
  ls_number_range-nrrangenr = nrrangenr.
  ls_number_range-toyear = toyear.
  IF transport_request IS NOT INITIAL.
    ls_option-transport = 'X'.
    ls_option-transport_request = transport_request.
  ENDIF.
  TRY.
      zcl_trm_pa_number_range=>create_if_not_exists(
        EXPORTING
          is_number_range = ls_number_range
          iv_fromnumber   = fromnumber
          iv_tonumber     = tonumber
          iv_nrlevel      = nrlevel
          iv_externind    = externind
          iv_procind      = procind
          is_option       = ls_option
      ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.


ENDFUNCTION.

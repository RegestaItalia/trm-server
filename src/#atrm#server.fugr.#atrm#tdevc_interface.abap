FUNCTION /ATRM/TDEVC_INTERFACE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DEVCLASS) TYPE  DEVCLASS
*"     VALUE(PARENTCL) TYPE  DEVCLASS OPTIONAL
*"     VALUE(RM_PARENTCL) TYPE  FLAG OPTIONAL
*"     VALUE(DEVLAYER) TYPE  DEVLAYER OPTIONAL
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      CREATE OBJECT go_package EXPORTING devclass = devclass.
      go_package->interface(
        EXPORTING
          parentcl    = parentcl
          rm_parentcl = rm_parentcl
          devlayer    = devlayer
      ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

FUNCTION ztrm_get_dependencies.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DEVCLASS) TYPE  DEVCLASS
*"     VALUE(IV_INCL_SUB) TYPE  FLAG
*"  EXPORTING
*"     VALUE(ET_DEPENDENCIES) TYPE  ZTRM_OBJECT_DEPENDENCIES_T
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      CREATE OBJECT lo_obj_dispacher EXPORTING devclass = iv_devclass.
      lo_obj_dispacher->get_objects_dependencies(
        EXPORTING
          incl_sub     = iv_incl_sub
        RECEIVING
          dependencies = et_dependencies[]
      ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

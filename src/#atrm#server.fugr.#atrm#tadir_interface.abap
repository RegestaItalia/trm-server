FUNCTION /atrm/tadir_interface.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PGMID) TYPE  PGMID
*"     VALUE(OBJECT) TYPE  TROBJTYPE
*"     VALUE(OBJ_NAME) TYPE  SOBJ_NAME
*"     VALUE(DEVCLASS) TYPE  DEVCLASS OPTIONAL
*"     VALUE(SRCSYSTEM) TYPE  SRCSYSTEM OPTIONAL
*"     VALUE(AUTHOR) TYPE  RESPONSIBL OPTIONAL
*"     VALUE(SET_GENFLAG) TYPE  GENFLAG OPTIONAL
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CALL METHOD ('/ATRM/CL_UTILITIES')=>('TADIR_INTERFACE')
      EXPORTING
        pgmid     = pgmid
        object    = object
        objname   = obj_name
        devclass  = devclass
        srcsystem = srcsystem
        author    = author
        genflag   = set_genflag
    .
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.

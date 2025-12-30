CLASS zcl_trm_senvi_incl DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_senvi_incl IMPLEMENTATION.

  METHOD determine.
    " empty implementation
    " object is most of the times the PROG name
    " however encl_object contains various things (eg. includes, enhancement implementations, badi enhancement implementations...)
    " needs more testing if needed
    " to test -> ABAP object make INCL entries
  ENDMETHOD.

ENDCLASS.

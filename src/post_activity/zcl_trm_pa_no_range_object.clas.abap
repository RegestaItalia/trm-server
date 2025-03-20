CLASS zcl_trm_pa_no_range_object DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS trm_pa TYPE flag VALUE 'X' ##NO_TEXT.

    CLASS-METHODS execute
      IMPORTING
        !object            TYPE nrobj
        !subobject         TYPE nrsobj
        !nrrangenr         TYPE nrnr
        !toyear            TYPE nryear
        !fromnumber        TYPE nrfrom OPTIONAL
        !tonumber          TYPE nrto OPTIONAL
        !nrlevel           TYPE nrlevel OPTIONAL
        !externind         TYPE nrind OPTIONAL
        !procind           TYPE procind OPTIONAL
        !transport_request TYPE trkorr OPTIONAL
      EXPORTING
        !messages          TYPE symsg_tab
      RAISING
        zcx_trm_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_pa_no_range_object IMPLEMENTATION.

  METHOD execute.

  ENDMETHOD.

ENDCLASS.

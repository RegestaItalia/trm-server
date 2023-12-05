FUNCTION ZTRM_GET_OBJ_LOCK_TR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PGMID) TYPE  PGMID
*"     VALUE(IV_OBJECT) TYPE  TROBJTYPE
*"     VALUE(IV_OBJ_NAME) TYPE  TROBJ_NAME
*"  EXPORTING
*"     VALUE(EV_TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

  DATA ls_e070 TYPE e070.
  SELECT SINGLE e070~trkorr e070~strkorr
  FROM e071
  INNER JOIN e070 ON e070~trkorr = e071~trkorr
  INTO CORRESPONDING FIELDS OF ls_e070
  WHERE e071~pgmid EQ iv_pgmid AND e071~object EQ iv_object AND e071~obj_name EQ iv_obj_name
        AND ( e070~trfunction EQ 'K' OR e070~trfunction EQ 'S' OR e070~trfunction EQ 'R' )
        AND e070~trstatus EQ 'D'.

  IF ls_e070-strkorr IS NOT INITIAL.
    ev_trkorr = ls_e070-strkorr.
  ELSE.
    ev_trkorr = ls_e070-trkorr.
  ENDIF.

ENDFUNCTION.

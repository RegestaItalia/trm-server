FUNCTION ztrm_get_manifest_objects.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      ET_TADIR STRUCTURE  TADIR
*"----------------------------------------------------------------------
  CLEAR et_tadir.

  DATA lt_tadir TYPE STANDARD TABLE OF tadir.
  SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE lt_tadir
  WHERE ( object EQ 'INTF' OR object EQ 'SKTD' )
  AND ( obj_name LIKE 'ZTRM%' OR obj_name LIKE 'YTRM%' OR obj_name LIKE '/%/TRM%' ).

  DATA ls_tadir_intf LIKE LINE OF lt_tadir.
  DATA ls_tadir_sktd LIKE LINE OF lt_tadir.
  DATA lv_is_manifest TYPE flag.
  LOOP AT lt_tadir INTO ls_tadir_intf WHERE object EQ 'INTF'.
    CLEAR lv_is_manifest.
    PERFORM is_intf_manifest USING ls_tadir_intf CHANGING lv_is_manifest.
    IF lv_is_manifest EQ 'X'.
      APPEND ls_tadir_intf TO et_tadir.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_tadir INTO ls_tadir_sktd WHERE object EQ 'SKTD'.
    CLEAR lv_is_manifest.
    PERFORM is_sktd_manifest USING ls_tadir_sktd CHANGING lv_is_manifest.
    IF lv_is_manifest EQ 'X'.
      APPEND ls_tadir_sktd TO et_tadir.
    ENDIF.
  ENDLOOP.


ENDFUNCTION.

FORM is_intf_manifest USING is_tadir TYPE tadir CHANGING cv_is_manifest TYPE flag.
  CLEAR cv_is_manifest.

  DATA lv_intf_name TYPE string.
  FIELD-SYMBOLS <fs_is_manifest> TYPE flag.
  lv_intf_name = is_tadir-obj_name.

  ASSIGN (lv_intf_name)=>('IS_TRM_MANIFEST') TO <fs_is_manifest>.
  IF <fs_is_manifest> IS ASSIGNED.
    cv_is_manifest = <fs_is_manifest>.
  ENDIF.
ENDFORM.

FORM is_sktd_manifest USING is_tadir TYPE tadir CHANGING cv_is_manifest TYPE flag.
  cv_is_manifest = 'X'.
ENDFORM.

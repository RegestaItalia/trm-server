FUNCTION ztrm_get_tadir_entry.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PGMID) LIKE  TADIR-PGMID
*"     VALUE(IV_OBJECT) LIKE  TADIR-OBJECT
*"     VALUE(IV_OBJ_NAME) LIKE  TADIR-OBJ_NAME
*"  EXPORTING
*"     VALUE(ES_TADIR) TYPE  TADIR
*"  EXCEPTIONS
*"      OBJECT_NOT_FOUND
*"----------------------------------------------------------------------
  SELECT SINGLE * FROM tadir INTO CORRESPONDING FIELDS OF es_tadir WHERE pgmid EQ iv_pgmid AND object EQ iv_object AND obj_name EQ iv_obj_name.
  IF sy-subrc <> 0.
    RAISE object_not_found.
  ENDIF.




ENDFUNCTION.

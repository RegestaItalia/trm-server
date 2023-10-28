FUNCTION ZTRM_TADIR_INTERFACE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PGMID) TYPE  PGMID
*"     VALUE(IV_OBJECT) TYPE  TROBJTYPE
*"     VALUE(IV_OBJ_NAME) TYPE  SOBJ_NAME
*"     VALUE(IV_DEVCLASS) TYPE  DEVCLASS OPTIONAL
*"     VALUE(IV_SRCSYSTEM) TYPE  SRCSYSTEM OPTIONAL
*"     VALUE(IV_AUTHOR) TYPE  RESPONSIBL OPTIONAL
*"     VALUE(IV_SET_GENFLAG) TYPE  GENFLAG OPTIONAL
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      TADIR_ENTRY_NOT_EXISTING
*"      TADIR_ENTRY_ILL_TYPE
*"      NO_SYSTEMNAME
*"      NO_SYSTEMTYPE
*"      ORIGINAL_SYSTEM_CONFLICT
*"      OBJECT_RESERVED_FOR_DEVCLASS
*"      OBJECT_EXISTS_GLOBAL
*"      OBJECT_EXISTS_LOCAL
*"      OBJECT_IS_DISTRIBUTED
*"      OBJ_SPECIFICATION_NOT_UNIQUE
*"      NO_AUTHORIZATION_TO_DELETE
*"      DEVCLASS_NOT_EXISTING
*"      SIMULTANIOUS_SET_REMOVE_REPAIR
*"      ORDER_MISSING
*"      NO_MODIFICATION_OF_HEAD_SYST
*"      PGMID_OBJECT_NOT_ALLOWED
*"      MASTERLANGUAGE_NOT_SPECIFIED
*"      DEVCLASS_NOT_SPECIFIED
*"      SPECIFY_OWNER_UNIQUE
*"      LOC_PRIV_OBJS_NO_REPAIR
*"      GTADIR_NOT_REACHED
*"      OBJECT_LOCKED_FOR_ORDER
*"      CHANGE_OF_CLASS_NOT_ALLOWED
*"      NO_CHANGE_FROM_SAP_TO_TMP
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

  CALL FUNCTION 'TR_TADIR_INTERFACE'
    EXPORTING
      wi_test_modus                  = ' '
      wi_tadir_pgmid                 = iv_pgmid
      wi_tadir_object                = iv_object
      wi_tadir_obj_name              = iv_obj_name
      wi_tadir_devclass              = iv_devclass
      wi_tadir_srcsystem             = iv_srcsystem
      wi_tadir_author                = iv_author
      wi_set_genflag                 = iv_set_genflag
*     iv_no_pak_check                = 'X'
    EXCEPTIONS
      tadir_entry_not_existing       = 1
      tadir_entry_ill_type           = 2
      no_systemname                  = 3
      no_systemtype                  = 4
      original_system_conflict       = 5
      object_reserved_for_devclass   = 6
      object_exists_global           = 7
      object_exists_local            = 8
      object_is_distributed          = 9
      obj_specification_not_unique   = 10
      no_authorization_to_delete     = 11
      devclass_not_existing          = 12
      simultanious_set_remove_repair = 13
      order_missing                  = 14
      no_modification_of_head_syst   = 15
      pgmid_object_not_allowed       = 16
      masterlanguage_not_specified   = 17
      devclass_not_specified         = 18
      specify_owner_unique           = 19
      loc_priv_objs_no_repair        = 20
      gtadir_not_reached             = 21
      object_locked_for_order        = 22
      change_of_class_not_allowed    = 23
      no_change_from_sap_to_tmp      = 24.

  IF sy-subrc EQ 1.
    RAISE tadir_entry_not_existing.
  ELSEIF sy-subrc EQ 2.
    RAISE tadir_entry_ill_type.
  ELSEIF sy-subrc EQ 3.
    RAISE no_systemname.
  ELSEIF sy-subrc EQ 4.
    RAISE no_systemtype.
  ELSEIF sy-subrc EQ 5.
    RAISE original_system_conflict.
  ELSEIF sy-subrc EQ 6.
    RAISE object_reserved_for_devclass.
  ELSEIF sy-subrc EQ 7.
    RAISE object_exists_global.
  ELSEIF sy-subrc EQ 8.
    RAISE object_exists_local.
  ELSEIF sy-subrc EQ 9.
    RAISE object_is_distributed.
  ELSEIF sy-subrc EQ 10.
    RAISE obj_specification_not_unique.
  ELSEIF sy-subrc EQ 11.
    RAISE no_authorization_to_delete.
  ELSEIF sy-subrc EQ 12.
    RAISE devclass_not_existing.
  ELSEIF sy-subrc EQ 13.
    RAISE simultanious_set_remove_repair.
  ELSEIF sy-subrc EQ 14.
    RAISE order_missing.
  ELSEIF sy-subrc EQ 15.
    RAISE no_modification_of_head_syst.
  ELSEIF sy-subrc EQ 16.
    RAISE pgmid_object_not_allowed.
  ELSEIF sy-subrc EQ 17.
    RAISE masterlanguage_not_specified.
  ELSEIF sy-subrc EQ 18.
    RAISE devclass_not_specified.
  ELSEIF sy-subrc EQ 19.
    RAISE specify_owner_unique.
  ELSEIF sy-subrc EQ 20.
    RAISE loc_priv_objs_no_repair.
  ELSEIF sy-subrc EQ 21.
    RAISE gtadir_not_reached.
  ELSEIF sy-subrc EQ 22.
    RAISE object_locked_for_order.
  ELSEIF sy-subrc EQ 23.
    RAISE change_of_class_not_allowed.
  ELSEIF sy-subrc EQ 24.
    RAISE no_change_from_sap_to_tmp.
  ENDIF.

ENDFUNCTION.

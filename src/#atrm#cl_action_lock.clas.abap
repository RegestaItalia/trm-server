CLASS /atrm/cl_action_lock DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tyt_keys TYPE STANDARD TABLE OF /atrm/act_lock_k WITH DEFAULT KEY.
    TYPES ty_token TYPE c LENGTH 32.
    TYPES ty_action TYPE c LENGTH 40.
    TYPES ty_resource_type TYPE c LENGTH 16.
    TYPES ty_resource_hash TYPE c LENGTH 64.
    CLASS-METHODS acquire
      IMPORTING it_keys TYPE tyt_keys
                iv_owner_token TYPE ty_token
                iv_action_name TYPE ty_action
      RAISING /atrm/cx_exception.
    CLASS-METHODS release
      IMPORTING it_keys TYPE tyt_keys
                iv_owner_token TYPE ty_token
      RAISING /atrm/cx_exception.
    CLASS-METHODS force_delete
      IMPORTING iv_resource_type TYPE ty_resource_type
                iv_resource_hash TYPE ty_resource_hash
                iv_owner_token TYPE ty_token
      RAISING /atrm/cx_exception.
  PRIVATE SECTION.
    CLASS-METHODS enqueue_table
      RAISING /atrm/cx_exception.
    CLASS-METHODS dequeue_table.
ENDCLASS.

CLASS /atrm/cl_action_lock IMPLEMENTATION.
  METHOD enqueue_table.
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        tabname = '/ATRM/ACT_LOCK'
      EXCEPTIONS
        foreign_lock = 1
        system_failure = 2
        OTHERS = 3.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise(
        iv_reason = /atrm/cx_exception=>c_reason-enqueue_error
        iv_message = 'TRM action lock table is busy' ).
    ENDIF.
  ENDMETHOD.

  METHOD dequeue_table.
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING tabname = '/ATRM/ACT_LOCK'.
  ENDMETHOD.

  METHOD acquire.
    DATA: lt_keys TYPE tyt_keys,
          ls_key TYPE /atrm/act_lock_k,
          ls_existing TYPE /atrm/act_lock,
          ls_lock TYPE /atrm/act_lock,
          lo_error TYPE REF TO /atrm/cx_exception,
          lo_root TYPE REF TO cx_root,
          lv_message TYPE string.

    IF iv_owner_token IS INITIAL OR iv_action_name IS INITIAL OR it_keys IS INITIAL.
      /atrm/cx_exception=>raise(
        iv_reason = /atrm/cx_exception=>c_reason-invalid_input
        iv_message = 'Action lock owner, action and keys are required' ).
    ENDIF.
    lt_keys = it_keys.
    SORT lt_keys BY resource_type resource_hash.
    DELETE ADJACENT DUPLICATES FROM lt_keys COMPARING resource_type resource_hash.
    LOOP AT lt_keys INTO ls_key.
      IF ls_key-resource_type IS INITIAL OR ls_key-resource_hash IS INITIAL OR ls_key-resource_name IS INITIAL.
        /atrm/cx_exception=>raise(
          iv_reason = /atrm/cx_exception=>c_reason-invalid_input
          iv_message = 'Action lock resource type, hash and name are required' ).
      ENDIF.
    ENDLOOP.

    enqueue_table( ).
    TRY.
        LOOP AT lt_keys INTO ls_key.
          CLEAR ls_existing.
          SELECT SINGLE * FROM /atrm/act_lock INTO ls_existing
            WHERE resource_type = ls_key-resource_type
              AND resource_hash = ls_key-resource_hash.
          IF sy-subrc = 0.
            IF ls_existing-owner_token <> iv_owner_token OR ls_existing-resource_name <> ls_key-resource_name.
              CONCATENATE 'Action lock held:' ls_existing-resource_type ls_existing-resource_name
                'by' ls_existing-created_by INTO lv_message SEPARATED BY space.
              /atrm/cx_exception=>raise(
                iv_reason = /atrm/cx_exception=>c_reason-enqueue_error
                iv_message = lv_message ).
            ENDIF.
          ENDIF.
        ENDLOOP.

        LOOP AT lt_keys INTO ls_key.
          CLEAR ls_existing.
          SELECT SINGLE * FROM /atrm/act_lock INTO ls_existing
            WHERE resource_type = ls_key-resource_type
              AND resource_hash = ls_key-resource_hash.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
          CLEAR ls_lock.
          ls_lock-resource_type = ls_key-resource_type.
          ls_lock-resource_hash = ls_key-resource_hash.
          ls_lock-resource_name = ls_key-resource_name.
          ls_lock-owner_token = iv_owner_token.
          ls_lock-action_name = iv_action_name.
          ls_lock-created_by = sy-uname.
          GET TIME STAMP FIELD ls_lock-created_at.
          INSERT /atrm/act_lock FROM ls_lock.
          IF sy-subrc <> 0.
            /atrm/cx_exception=>raise(
              iv_reason = /atrm/cx_exception=>c_reason-insert_error
              iv_message = 'Could not persist TRM action lock' ).
          ENDIF.
        ENDLOOP.
        COMMIT WORK AND WAIT.
      CATCH /atrm/cx_exception INTO lo_error.
        ROLLBACK WORK.
        dequeue_table( ).
        RAISE EXCEPTION lo_error.
      CATCH cx_root INTO lo_root.
        ROLLBACK WORK.
        dequeue_table( ).
        /atrm/cx_exception=>raise(
          iv_reason = /atrm/cx_exception=>c_reason-generic
          io_root = lo_root ).
    ENDTRY.
    dequeue_table( ).
  ENDMETHOD.

  METHOD release.
    DATA: lt_keys TYPE tyt_keys,
          ls_key TYPE /atrm/act_lock_k,
          ls_existing TYPE /atrm/act_lock,
          lo_error TYPE REF TO /atrm/cx_exception,
          lo_root TYPE REF TO cx_root,
          lo_delete_error TYPE REF TO cx_root,
          lv_mismatch TYPE abap_bool.

    IF iv_owner_token IS INITIAL.
      /atrm/cx_exception=>raise(
        iv_reason = /atrm/cx_exception=>c_reason-invalid_input
        iv_message = 'Action lock owner is required' ).
    ENDIF.
    lt_keys = it_keys.
    SORT lt_keys BY resource_type resource_hash.
    DELETE ADJACENT DUPLICATES FROM lt_keys COMPARING resource_type resource_hash.
    enqueue_table( ).
    TRY.
        LOOP AT lt_keys INTO ls_key.
          CLEAR ls_existing.
          SELECT SINGLE * FROM /atrm/act_lock INTO ls_existing
            WHERE resource_type = ls_key-resource_type
              AND resource_hash = ls_key-resource_hash.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          IF ls_existing-owner_token <> iv_owner_token OR ls_existing-resource_name <> ls_key-resource_name.
            lv_mismatch = abap_true.
            CONTINUE.
          ENDIF.
          TRY.
              DELETE FROM /atrm/act_lock
                WHERE resource_type = ls_key-resource_type
                  AND resource_hash = ls_key-resource_hash
                  AND owner_token = iv_owner_token.
              IF sy-subrc <> 0.
                lv_mismatch = abap_true.
              ENDIF.
            CATCH cx_root INTO lo_delete_error.
              IF lo_root IS NOT BOUND.
                lo_root = lo_delete_error.
              ENDIF.
              " Continue attempting independent releases.
          ENDTRY.
        ENDLOOP.
        COMMIT WORK AND WAIT.
      CATCH /atrm/cx_exception INTO lo_error.
        ROLLBACK WORK.
        dequeue_table( ).
        RAISE EXCEPTION lo_error.
      CATCH cx_root INTO lo_root.
        ROLLBACK WORK.
        dequeue_table( ).
        /atrm/cx_exception=>raise(
          iv_reason = /atrm/cx_exception=>c_reason-generic
          io_root = lo_root ).
    ENDTRY.
    dequeue_table( ).
    IF lo_root IS BOUND.
      /atrm/cx_exception=>raise(
        iv_reason = /atrm/cx_exception=>c_reason-generic
        io_root = lo_root ).
    ENDIF.
    IF lv_mismatch = abap_true.
      /atrm/cx_exception=>raise(
        iv_reason = /atrm/cx_exception=>c_reason-enqueue_error
        iv_message = 'Some action locks belong to another owner' ).
    ENDIF.
  ENDMETHOD.

  METHOD force_delete.
    DATA: lo_root TYPE REF TO cx_root.
    IF iv_resource_type IS INITIAL OR iv_resource_hash IS INITIAL OR iv_owner_token IS INITIAL.
      /atrm/cx_exception=>raise(
        iv_reason = /atrm/cx_exception=>c_reason-invalid_input
        iv_message = 'Exact resource type and hash are required' ).
    ENDIF.
    enqueue_table( ).
    TRY.
        DELETE FROM /atrm/act_lock
          WHERE resource_type = iv_resource_type
            AND resource_hash = iv_resource_hash
            AND owner_token = iv_owner_token.
        IF sy-subrc <> 0.
          /atrm/cx_exception=>raise(
            iv_reason = /atrm/cx_exception=>c_reason-not_found
            iv_message = 'Action lock changed or was already removed' ).
        ENDIF.
        COMMIT WORK AND WAIT.
      CATCH cx_root INTO lo_root.
        ROLLBACK WORK.
        dequeue_table( ).
        /atrm/cx_exception=>raise(
          iv_reason = /atrm/cx_exception=>c_reason-generic
          io_root = lo_root ).
    ENDTRY.
    dequeue_table( ).
  ENDMETHOD.
ENDCLASS.

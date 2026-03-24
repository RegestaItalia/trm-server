CLASS /atrm/cl_log_polling DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA: id           TYPE /atrm/polling_id READ-ONLY,
          last_message TYPE /atrm/polling_last_msg READ-ONLY.
    METHODS constructor
      IMPORTING id TYPE /atrm/polling_id OPTIONAL.
    CLASS-METHODS create
      IMPORTING event      TYPE /atrm/polling_event
      RETURNING VALUE(log) TYPE REF TO /atrm/cl_log_polling
      RAISING   /atrm/cx_exception
                cx_uuid_error.
    METHODS delete.
    METHODS update_message
      IMPORTING message TYPE /atrm/polling_last_msg.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_log_polling IMPLEMENTATION.

  METHOD constructor.
    CHECK id IS NOT INITIAL.
    DATA polling TYPE /atrm/polling.
    SELECT SINGLE polling_id last_message FROM /atrm/polling INTO CORRESPONDING FIELDS OF polling WHERE polling_id = id.
    me->id = polling-polling_id.
    me->last_message = polling-last_message.
  ENDMETHOD.

  METHOD create.
    DATA polling TYPE /atrm/polling.
    polling-mandt = sy-mandt.
    polling-polling_id = cl_system_uuid=>create_uuid_x16_static( ).
    polling-polling_event = event.
    polling-last_update_user = sy-uname.
    polling-last_update_time = sy-uzeit.
    polling-last_update_date = sy-datum.
    INSERT /atrm/polling FROM polling.
    COMMIT WORK AND WAIT.
    CREATE OBJECT log EXPORTING id = polling-polling_id.
  ENDMETHOD.

  METHOD update_message.
    CHECK me->id IS NOT INITIAL.
    UPDATE /atrm/polling SET last_message = message WHERE polling_id = id.
    UPDATE /atrm/polling SET last_update_user = sy-uname WHERE polling_id = id.
    UPDATE /atrm/polling SET last_update_time = sy-uzeit WHERE polling_id = id.
    UPDATE /atrm/polling SET last_update_date = sy-datum WHERE polling_id = id.
    COMMIT WORK AND WAIT.
    last_message = message.
  ENDMETHOD.

  METHOD delete.
    DELETE FROM /atrm/polling WHERE polling_id = id.
    COMMIT WORK AND WAIT.
  ENDMETHOD.

ENDCLASS.

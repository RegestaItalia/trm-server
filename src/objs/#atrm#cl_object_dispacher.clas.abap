CLASS /atrm/cl_object_dispacher DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS /atrm/cl_object.

  PUBLIC SECTION.

    CLASS-METHODS get_package_dependencies
      IMPORTING package             TYPE devclass
                incl_sub            TYPE flag
                log_id              TYPE /atrm/polling_id OPTIONAL
      RETURNING VALUE(dependencies) TYPE /atrm/object_dependencies_t
      RAISING   /atrm/cx_exception.

    CLASS-METHODS get
      IMPORTING key                TYPE /atrm/object
      RETURNING VALUE(ro_instance) TYPE REF TO /atrm/if_object.

  PROTECTED SECTION.
    CLASS-DATA: dependencies TYPE /atrm/object_dependencies_t,
                log          TYPE REF TO /ATRM/CL_LOG_POLLING.
  PRIVATE SECTION.
CLASS-METHODS reading_message
      IMPORTING package        TYPE devclass
      RETURNING VALUE(message) TYPE /atrm/polling_last_msg.
CLASS-METHODS progress_message
      IMPORTING percentage     TYPE p
      RETURNING VALUE(message) TYPE /atrm/polling_last_msg.
ENDCLASS.



CLASS /ATRM/CL_OBJECT_DISPACHER IMPLEMENTATION.


  METHOD get_package_dependencies.
    DATA: lo_package     TYPE REF TO /ATRM/CL_PACKAGE,
          lt_objects     TYPE scts_tadir,
          ls_object      LIKE LINE OF lt_objects,
          ls_key         TYPE /atrm/object,
          lo_object      TYPE REF TO /atrm/if_object,
          lv_total_objs  TYPE i,
          lv_counter     TYPE i,
          lv_percentage  TYPE p LENGTH 16 DECIMALS 4,
          lv_message     TYPE /atrm/polling_last_msg.
    FIELD-SYMBOLS <fs_object_deps> TYPE /atrm/object_dependencies.

    CREATE OBJECT log EXPORTING id = log_id.

    CREATE OBJECT lo_package EXPORTING devclass = package.
    lv_message = reading_message( package ).
    log->update_message( lv_message ).
    CALL METHOD lo_package->get_objects
      EXPORTING
        incl_sub = incl_sub
      IMPORTING
        tadir    = lt_objects
    .
    DESCRIBE TABLE lt_objects LINES lv_total_objs.
    CLEAR lv_percentage.
    CLEAR lv_counter.
    CLEAR /atrm/cl_object_dispacher=>dependencies[].
    LOOP AT lt_objects INTO ls_object.
      lv_counter = sy-tabix.
      lv_message = progress_message( lv_percentage ).
      log->update_message( lv_message ).
      CLEAR ls_key.
      CLEAR lo_object.
      UNASSIGN <fs_object_deps>.
      MOVE-CORRESPONDING ls_object TO ls_key.
      APPEND INITIAL LINE TO /atrm/cl_object_dispacher=>dependencies ASSIGNING <fs_object_deps>.
      MOVE-CORRESPONDING ls_key TO <fs_object_deps>.
      CALL METHOD get
        EXPORTING
          key         = ls_key
        RECEIVING
          ro_instance = lo_object.
      CALL METHOD lo_object->get_dependencies
        IMPORTING
          dependencies = <fs_object_deps>-dependencies
      .
      SORT <fs_object_deps>-dependencies BY tabname tabkey.
      DELETE ADJACENT DUPLICATES FROM <fs_object_deps>-dependencies COMPARING tabname tabkey.
      lv_percentage = ( ( lv_counter + 1 ) / lv_total_objs ) * 100.
    ENDLOOP.
    dependencies = /atrm/cl_object_dispacher=>dependencies.
  ENDMETHOD.


  METHOD get.
    DATA: lv_class_name TYPE string,
          lo_badi       TYPE REF TO /atrm/objs_handler.
    CONCATENATE '/ATRM/CL_OBJECT_' key-object INTO lv_class_name.
    TRANSLATE lv_class_name TO UPPER CASE.
    TRY.
        CREATE OBJECT ro_instance TYPE (lv_class_name)
          EXPORTING key = key.
      CATCH cx_dynamic_check.
        CREATE OBJECT ro_instance TYPE /atrm/cl_object
          EXPORTING
            key = key.
    ENDTRY.
    TRY.
        GET BADI lo_badi.
        CALL BADI lo_badi->change_object_handler
          EXPORTING
            key     = key
          CHANGING
            handler = ro_instance.
      CATCH cx_badi_not_implemented cx_badi_initial_reference.
    ENDTRY.
  ENDMETHOD.

  METHOD reading_message.
    CONCATENATE 'Reading "' package '" objects' INTO message.
  ENDMETHOD.

  METHOD progress_message.
    DATA: lv_percentage TYPE p LENGTH 3 DECIMALS 1,
          lv_text       TYPE string.
    lv_percentage = percentage.
    lv_text = lv_percentage.
    CONDENSE lv_text.
    CONCATENATE 'Finding dependencies (' lv_text '%)' INTO message.
  ENDMETHOD.
ENDCLASS.

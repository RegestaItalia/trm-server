"! Activate a SICF node if not active already.
"!
CLASS /atrm/cl_pa_activate_sicf_node DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS trm_pa TYPE flag VALUE 'X' ##NO_TEXT.

    TYPES: ty_icfname   TYPE c LENGTH 15.

    "! @parameter url              | Url
    "! @parameter hostname         | Hostname
    "! @parameter execute          | If post activity should be executed (X = true)
    "! @parameter messages         | Message table capturing success or error feedback
    "! @raising /atrm/cx_exception | Raised if check fails
    CLASS-METHODS pre
      IMPORTING
        !url      TYPE string
        !hostname TYPE ty_icfname
      EXPORTING
        !messages TYPE /atrm/symsg_tab
        !execute  TYPE flag
      RAISING
        /atrm/cx_exception.

    "! @parameter url              | Url
    "! @parameter hostname         | Hostname
    "! @parameter messages         | Message table capturing success or error feedback
    "! @raising /atrm/cx_exception | Raised if activation fails
    CLASS-METHODS execute
      IMPORTING
        !url      TYPE string
        !hostname TYPE ty_icfname
      EXPORTING
        !messages TYPE /atrm/symsg_tab
      RAISING
        /atrm/cx_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS /atrm/cl_pa_activate_sicf_node IMPLEMENTATION.

  METHOD pre.
    DATA: root   TYPE REF TO cx_dynamic_check,
          active TYPE c.
    CLEAR execute.
    TRY.
        CALL METHOD ('CL_ICF_TREE')=>is_service_active
          EXPORTING
            url                    = url
            hostname               = hostname
          IMPORTING
            active                 = active
          EXCEPTIONS
            invalid_url            = 1
            empty_url_and_nodeguid = 2
            internal_error         = 3
            OTHERS                 = 4.
        IF sy-subrc EQ 0.
          IF execute <> 'X'.
            execute = 'X'.
          ENDIF.
        ELSE.
          /atrm/cx_exception=>raise( iv_reason  = /atrm/cx_exception=>c_reason-generic ).
        ENDIF.
      CATCH cx_sy_dyn_call_error INTO root.
        /atrm/cx_exception=>raise(
          io_root    = root
          iv_reason  = /atrm/cx_exception=>c_reason-pa_dynamic
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD execute.
    DATA: root       TYPE REF TO cx_dynamic_check,
          lv_message TYPE string,
          ls_message LIKE LINE OF messages.
    TRY.
        CALL METHOD ('CL_ICF_TREE')=>activate_node
          EXPORTING
            url                      = url
            hostname                 = hostname
          EXCEPTIONS
            node_not_existing        = 1
            enqueue_error            = 2
            no_authority             = 3
            url_and_nodeguid_space   = 4
            url_and_nodeguid_fill_in = 5
            OTHERS                   = 6.
        IF sy-subrc EQ 0.
          CONCATENATE 'Activated SICF node' url 'host' hostname INTO lv_message SEPARATED BY space.
          CONDENSE lv_message.
          cl_message_helper=>set_msg_vars_for_clike( lv_message ).
          MOVE-CORRESPONDING sy TO ls_message.
          ls_message-msgty = 'S'.
          APPEND ls_message TO messages.
        ELSE.
          /atrm/cx_exception=>raise( iv_reason  = /atrm/cx_exception=>c_reason-generic ).
        ENDIF.
      CATCH cx_sy_dyn_call_error INTO root.
        /atrm/cx_exception=>raise(
          io_root    = root
          iv_reason  = /atrm/cx_exception=>c_reason-pa_dynamic
        ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS /atrm/cl_object_ucsa DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_ucsa IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lo_persist     TYPE REF TO object,
      lr_data        TYPE REF TO data,
      lv_icf_name    TYPE sobj_name,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <ls_data>      TYPE any,
      <lt_items>     TYPE ANY TABLE,
      <ls_item>      TYPE any,
      <lv_obj_name>  TYPE any,
      <lv_parent>    TYPE any.

    TRY.
        CREATE DATA lr_data TYPE ('UCONSERVASCOMPLETE').
        ASSIGN lr_data->* TO <ls_data>.

        CALL METHOD ('CL_UCON_SA_DB_PERSIST')=>('IF_UCON_SA_PERSIST~GET_INSTANCE')
          EXPORTING id = me->key-obj_name
          RECEIVING instance = lo_persist.
        CALL METHOD lo_persist->('IF_UCON_SA_PERSIST~LOAD')
          EXPORTING
            version = 'A'
            language = sy-langu
          IMPORTING
            sa = <ls_data>.

        ASSIGN COMPONENT 'HEADER-PARENT' OF STRUCTURE <ls_data> TO <lv_obj_name>.
        IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
          TRY.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'UCSA' obj_name = <lv_obj_name>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        UNASSIGN <lv_obj_name>.
        ASSIGN COMPONENT 'HEADER-DEFAULT_PROFILE'
          OF STRUCTURE <ls_data> TO <lv_obj_name>.
        IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'HTTP' obj_name = <lv_obj_name>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        ASSIGN COMPONENT 'SUB_SERVICE_AS' OF STRUCTURE <ls_data> TO <lt_items>.
        IF sy-subrc = 0.
          LOOP AT <lt_items> ASSIGNING <ls_item>.
            UNASSIGN <lv_obj_name>.
            ASSIGN COMPONENT 'SUB_SAS' OF STRUCTURE <ls_item> TO <lv_obj_name>.
            IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
              TRY.
                  CLEAR ls_dependency.
                  CALL METHOD get_tadir_dependency
                    EXPORTING object = 'UCSA' obj_name = <lv_obj_name>
                    RECEIVING dependency = ls_dependency.
                  APPEND ls_dependency TO dependencies.
                CATCH cx_root.
                  " optional dependency may not exist in the target system
              ENDTRY.
            ENDIF.
          ENDLOOP.
        ENDIF.

        UNASSIGN <lt_items>.
        ASSIGN COMPONENT 'SUB_HTTP_SERVICES' OF STRUCTURE <ls_data> TO <lt_items>.
        IF sy-subrc = 0.
          LOOP AT <lt_items> ASSIGNING <ls_item>.
            UNASSIGN <lv_obj_name>.
            ASSIGN COMPONENT 'SERVICENAME' OF STRUCTURE <ls_item> TO <lv_obj_name>.
            IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
              TRY.
                  CLEAR ls_dependency.
                  CALL METHOD get_tadir_dependency
                    EXPORTING object = 'HTTP' obj_name = <lv_obj_name>
                    RECEIVING dependency = ls_dependency.
                  APPEND ls_dependency TO dependencies.
                CATCH cx_root.
                  " optional dependency may not exist in the target system
              ENDTRY.
            ENDIF.
          ENDLOOP.
        ENDIF.

        UNASSIGN <lt_items>.
        ASSIGN COMPONENT 'SUB_RFC_SERVICES' OF STRUCTURE <ls_data> TO <lt_items>.
        IF sy-subrc = 0.
          LOOP AT <lt_items> ASSIGNING <ls_item>.
            UNASSIGN <lv_obj_name>.
            ASSIGN COMPONENT 'RFCSERVICE' OF STRUCTURE <ls_item> TO <lv_obj_name>.
            IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
              TRY.
                  CLEAR ls_dependency.
                  CALL METHOD get_tadir_dependency
                    EXPORTING object = 'SRFC' obj_name = <lv_obj_name>
                    RECEIVING dependency = ls_dependency.
                  APPEND ls_dependency TO dependencies.
                CATCH cx_root.
                  " optional dependency may not exist in the target system
              ENDTRY.
            ENDIF.
          ENDLOOP.
        ENDIF.

        UNASSIGN <lt_items>.
        ASSIGN COMPONENT 'SUB_RFC_FUNCS' OF STRUCTURE <ls_data> TO <lt_items>.
        IF sy-subrc = 0.
          LOOP AT <lt_items> ASSIGNING <ls_item>.
            UNASSIGN <lv_obj_name>.
            ASSIGN COMPONENT 'RFCFUNCNAME' OF STRUCTURE <ls_item> TO <lv_obj_name>.
            IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
              TRY.
                  CLEAR ls_dependency.
                  CALL METHOD get_tfdir_dependency
                    EXPORTING funcname = <lv_obj_name>
                    RECEIVING dependency = ls_dependency.
                  APPEND ls_dependency TO dependencies.
                CATCH cx_root.
                  " optional dependency may not exist in the target system
              ENDTRY.
            ENDIF.
          ENDLOOP.
        ENDIF.

        UNASSIGN <lt_items>.
        ASSIGN COMPONENT 'SUB_ICF_SERVICES' OF STRUCTURE <ls_data> TO <lt_items>.
        IF sy-subrc = 0.
          LOOP AT <lt_items> ASSIGNING <ls_item>.
            UNASSIGN: <lv_obj_name>, <lv_parent>.
            ASSIGN COMPONENT 'SUB_ICF_NAME' OF STRUCTURE <ls_item> TO <lv_obj_name>.
            ASSIGN COMPONENT 'SUB_ICFPARGUID' OF STRUCTURE <ls_item> TO <lv_parent>.
            IF <lv_obj_name> IS ASSIGNED
              AND <lv_parent> IS ASSIGNED
              AND <lv_obj_name> IS NOT INITIAL.
              CONCATENATE <lv_obj_name> <lv_parent> INTO lv_icf_name.
              TRY.
                  CLEAR ls_dependency.
                  CALL METHOD get_tadir_dependency
                    EXPORTING object = 'SICF' obj_name = lv_icf_name
                    RECEIVING dependency = ls_dependency.
                  APPEND ls_dependency TO dependencies.
                CATCH cx_root.
                  " optional dependency may not exist in the target system
              ENDTRY.
            ENDIF.
          ENDLOOP.
        ENDIF.
      CATCH cx_root.
        " optional unified connectivity API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

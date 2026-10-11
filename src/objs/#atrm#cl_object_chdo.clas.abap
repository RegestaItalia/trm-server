CLASS /atrm/cl_object_chdo DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_chdo IMPLEMENTATION.

METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_table_name TYPE tabname,
      lv_row_type TYPE string,
      lr_rows TYPE REF TO data,
      ls_object_type TYPE wbobjtype,
      lo_operator TYPE REF TO object,
      lo_data_model TYPE REF TO object,
      lr_data TYPE REF TO data,
      lv_data_type TYPE string,
      lv_generated_class TYPE sobj_name,
      lv_candidate_class TYPE sobj_name,
      lv_class_prefix TYPE sobj_name,
      lv_class_suffix TYPE sobj_name,
      lv_prefix_length TYPE i,
      lv_offset TYPE i,
      lv_component TYPE fieldname,
      ls_dependency TYPE /atrm/object_dependency.
    FIELD-SYMBOLS:
      <lt_rows> TYPE ANY TABLE,
      <ls_row> TYPE any,
      <lv_name> TYPE any,
      <ls_data> TYPE any,
      <ls_content> TYPE any,
      <lv_generated> TYPE any.

    TRY.
        lv_table_name = 'TCDOB'.
        lv_row_type = 'TCDOB'.
        CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_row_type).
        ASSIGN lr_rows->* TO <lt_rows>.
        SELECT * FROM (lv_table_name) INTO TABLE <lt_rows>
          WHERE object = me->key-obj_name.
        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          lv_component = 'TABNAME'.
          ASSIGN COMPONENT lv_component OF STRUCTURE <ls_row> TO <lv_name>.
          IF sy-subrc = 0 AND <lv_name> IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'TABL' obj_name = <lv_name>
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
            ENDTRY.
          ENDIF.
          lv_component = 'REFNAME'.
          ASSIGN COMPONENT lv_component OF STRUCTURE <ls_row> TO <lv_name>.
          IF sy-subrc = 0 AND <lv_name> IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'TABL' obj_name = <lv_name>
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
            ENDTRY.
          ENDIF.
          lv_component = 'OLDTABNAME'.
          ASSIGN COMPONENT lv_component OF STRUCTURE <ls_row> TO <lv_name>.
          IF sy-subrc = 0 AND <lv_name> IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'TABL' obj_name = <lv_name>
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
            ENDTRY.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        " Classic change-document persistence is optional across releases
    ENDTRY.

    TRY.
        CLEAR ls_dependency.
        CALL METHOD get_tadir_dependency
          EXPORTING object = 'FUGR' obj_name = me->key-obj_name
          RECEIVING dependency = ls_dependency.
        APPEND ls_dependency TO dependencies.
      CATCH cx_root.
        " Classic function-group generation may not have been requested
    ENDTRY.

    TRY.
        ls_object_type-objtype_tr = 'CHDO'.
        ls_object_type-subtype_wb = 'CHD'.
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING object_type = ls_object_type object_key = me->key-obj_name
          RECEIVING result = lo_operator.
        CALL METHOD lo_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING version = 'A' data_selection = 'AL'
          IMPORTING eo_object_data = lo_data_model.
        CALL METHOD lo_data_model->('IF_WB_OBJECT_DATA_MODEL~GET_DATATYPE_NAME')
          EXPORTING p_data_selection = 'AL'
          RECEIVING result = lv_data_type.
        IF lv_data_type IS NOT INITIAL.
          CREATE DATA lr_data TYPE (lv_data_type).
          ASSIGN lr_data->* TO <ls_data>.
          CALL METHOD lo_data_model->('IF_WB_OBJECT_DATA_MODEL~GET_SELECTED_DATA')
            EXPORTING p_data_selection = 'AL'
            IMPORTING p_data = <ls_data>.
          ASSIGN COMPONENT 'CONTENT' OF STRUCTURE <ls_data> TO <ls_content>.
          IF sy-subrc <> 0.
            ASSIGN <ls_data> TO <ls_content>.
          ENDIF.
          ASSIGN COMPONENT 'GENERAL_INFORMATION-GENERATED_OBJECT'
            OF STRUCTURE <ls_content> TO <lv_generated>.
          IF sy-subrc = 0 AND <lv_generated> IS NOT INITIAL.
            lv_generated_class = <lv_generated>.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'CLAS' obj_name = lv_generated_class
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " Generated writer class may not be active
            ENDTRY.
          ENDIF.
        ENDIF.
      CATCH cx_root.
        " Workbench change-document APIs are optional across SAP releases
    ENDTRY.

    " The standard SCDO class generator names its class CL_<CHDO>_CHDO
    lv_class_suffix = me->key-obj_name.
    IF lv_class_suffix(1) = '/'.
      FIND '/' IN lv_class_suffix+1 MATCH OFFSET lv_offset.
      IF sy-subrc = 0.
        lv_prefix_length = lv_offset + 2.
        lv_class_prefix = lv_class_suffix(lv_prefix_length).
        lv_class_suffix = lv_class_suffix+lv_prefix_length.
      ENDIF.
    ENDIF.
    CONCATENATE lv_class_prefix 'CL_' lv_class_suffix '_CHDO'
      INTO lv_candidate_class.
    IF lv_candidate_class IS NOT INITIAL.
      TRY.
          CLEAR ls_dependency.
          CALL METHOD get_tadir_dependency
            EXPORTING object = 'CLAS' obj_name = lv_candidate_class
            RECEIVING dependency = ls_dependency.
          APPEND ls_dependency TO dependencies.
        CATCH cx_root.
          " No conventionally named generated writer class exists
      ENDTRY.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

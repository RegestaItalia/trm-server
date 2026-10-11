CLASS /atrm/cl_object_vtrr DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_vtrr IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    TYPES:
      BEGIN OF ty_reference,
        path   TYPE string,
        object TYPE trobjtype,
      END OF ty_reference.

    DATA:
      ls_object_type TYPE wbobjtype,
      lo_operator    TYPE REF TO object,
      lo_data_model  TYPE REF TO object,
      lr_data        TYPE REF TO data,
      lv_data_type   TYPE string,
      lt_references  TYPE STANDARD TABLE OF ty_reference,
      ls_reference   TYPE ty_reference,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <ls_data>      TYPE any,
      <ls_content>   TYPE any,
      <lv_obj_name>  TYPE any.

    TRY.
        ls_object_type-objtype_tr = 'VTRR'.
        ls_object_type-subtype_wb = 'TYP'.

        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING object_type = ls_object_type object_key = me->key-obj_name
          RECEIVING result = lo_operator.
        CALL METHOD lo_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING version = 'A' data_selection = 'AL'
          IMPORTING eo_object_data = lo_data_model.
        CALL METHOD lo_data_model->('IF_WB_OBJECT_DATA_MODEL~GET_DATATYPE_NAME')
          EXPORTING p_data_selection = 'AL'
          RECEIVING result = lv_data_type.
        CHECK lv_data_type IS NOT INITIAL.

        CREATE DATA lr_data TYPE (lv_data_type).
        ASSIGN lr_data->* TO <ls_data>.
        CALL METHOD lo_data_model->('IF_WB_OBJECT_DATA_MODEL~GET_SELECTED_DATA')
          EXPORTING p_data_selection = 'AL'
          IMPORTING p_data = <ls_data>.

        ASSIGN COMPONENT 'CONTENT' OF STRUCTURE <ls_data> TO <ls_content>.
        IF sy-subrc <> 0.
          ASSIGN <ls_data> TO <ls_content>.
        ENDIF.

        ls_reference-path = 'SOURCE_TABLE'.
        ls_reference-object = 'TABL'.
        APPEND ls_reference TO lt_references.
        ls_reference-path = 'KEY_STRUCTURE'.
        ls_reference-object = 'TABL'.
        APPEND ls_reference TO lt_references.
        ls_reference-path = 'COMPLETE_KEY_STRUCTURE'.
        ls_reference-object = 'TABL'.
        APPEND ls_reference TO lt_references.
        ls_reference-path = 'PLUG_IN_CLASS'.
        ls_reference-object = 'CLAS'.
        APPEND ls_reference TO lt_references.

        LOOP AT lt_references INTO ls_reference.
          UNASSIGN <lv_obj_name>.
          ASSIGN COMPONENT ls_reference-path OF STRUCTURE <ls_content>
            TO <lv_obj_name>.
          CHECK sy-subrc = 0.
          CHECK <lv_obj_name> IS NOT INITIAL.

          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = ls_reference-object obj_name = <lv_obj_name>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " optional verticalization registry API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS /atrm/cl_object_dsfi DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_dsfi IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      ls_object_type TYPE wbobjtype,
      lo_operator    TYPE REF TO object,
      lo_data_model  TYPE REF TO object,
      lr_data        TYPE REF TO data,
      lv_data_type   TYPE string,
      lv_reference   TYPE string,
      lv_class_name  TYPE seoclsname,
      lv_method      TYPE string,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <ls_data>       TYPE any,
      <ls_content>    TYPE any,
      <lv_reference>  TYPE any.

    TRY.
        ls_object_type-objtype_tr = 'DSFI'.
        ls_object_type-subtype_wb = 'SFI'.

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

        ASSIGN COMPONENT 'SQL_PROPERTIES-AMDP_REFERENCE'
          OF STRUCTURE <ls_content> TO <lv_reference>.
        CHECK sy-subrc = 0.
        CHECK <lv_reference> IS NOT INITIAL.

        lv_reference = <lv_reference>.
        SPLIT lv_reference AT '=>' INTO lv_class_name lv_method.
        CHECK lv_class_name IS NOT INITIAL.

        CALL METHOD get_tadir_dependency
          EXPORTING object = 'CLAS' obj_name = lv_class_name
          RECEIVING dependency = ls_dependency.
        APPEND ls_dependency TO dependencies.
      CATCH cx_root.
        " optional scalar function API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

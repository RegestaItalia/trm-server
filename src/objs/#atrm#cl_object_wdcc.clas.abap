CLASS /atrm/cl_object_wdcc DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_wdcc IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_table      TYPE tabname,
      lv_where      TYPE string,
      lv_wul_where  TYPE string,
      ls_config_key TYPE wdy_config_key,
      lv_name       TYPE sobj_name,
      lr_rows       TYPE REF TO data,
      lv_intf       TYPE string,
      lv_method     TYPE string,
      lv_par_in     TYPE abap_parmname,
      lv_par_out    TYPE abap_parmname,
      lr_analyzer   TYPE REF TO data,
      lr_types      TYPE REF TO data,
      lr_type       TYPE REF TO data,
      lr_attributes TYPE REF TO data,
      lo_analyzer   TYPE REF TO object,
      lo_descr      TYPE REF TO cl_abap_typedescr,
      lo_intfdescr  TYPE REF TO cl_abap_objectdescr,
      ls_methdescr  TYPE abap_methdescr,
      ls_parmdescr  TYPE abap_parmdescr,
      lt_ptab       TYPE abap_parmbind_tab,
      ls_ptab       TYPE abap_parmbind,
      ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_rows>  TYPE STANDARD TABLE,
      <ls_row>   TYPE any,
      <lv_id>    TYPE any,
      <lv_type>  TYPE any,
      <lv_var>   TYPE any,
      <lo_analyzer>  TYPE any,
      <lt_types>     TYPE ANY TABLE,
      <ls_type>      TYPE any,
      <lt_attrs>     TYPE ANY TABLE,
      <ls_attr>      TYPE any,
      <lt_values>    TYPE ANY TABLE,
      <ls_value>     TYPE any,
      <lv_comp>      TYPE any,
      <ls_kinds>     TYPE any,
      <lv_value> TYPE any.

    ls_config_key = me->key-obj_name.
    lv_name = ls_config_key-config_id.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'CONFIG_ID = ''' lv_name ''' AND CONFIG_TYPE = ''' ls_config_key-config_type
                ''' AND CONFIG_VAR = ''' ls_config_key-config_var '''' INTO lv_where.
    CONCATENATE lv_where ' AND ENHNAME = '' ''' INTO lv_wul_where RESPECTING BLANKS.

    " Configured component
    append_table_dependencies(
      EXPORTING table_name   = 'WDY_CONFIG_DATA'
                where_clause = lv_where
                object_field = 'COMPONENT'
                object_type  = 'WDYN'
      CHANGING  dependencies = dependencies ).

    " Used configurations (parent configuration, embedded UIBB configurations) and components
    TRY.
        lv_table = 'WDY_CFG_CMP_WUL'.
        CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_rows->* TO <lt_rows>.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_wul_where).
        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'USED_CONFIG_ID' OF STRUCTURE <ls_row> TO <lv_id>.
          ASSIGN COMPONENT 'USED_CONFIG_TYPE' OF STRUCTURE <ls_row> TO <lv_type>.
          ASSIGN COMPONENT 'USED_CONFIG_VAR' OF STRUCTURE <ls_row> TO <lv_var>.
          IF <lv_id> IS ASSIGNED AND <lv_id> IS NOT INITIAL.
            CLEAR ls_config_key.
            ls_config_key-config_id = <lv_id>.
            ls_config_key-config_type = <lv_type>.
            ls_config_key-config_var = <lv_var>.
            lv_name = ls_config_key.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'WDCC' obj_name = lv_name
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
          ASSIGN COMPONENT 'USED_COMPONENT' OF STRUCTURE <ls_row> TO <lv_value>.
          IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'WDYN' obj_name = <lv_value>
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
          UNASSIGN: <lv_id>, <lv_type>, <lv_var>.
        ENDLOOP.
      CATCH cx_root.
        " optional repository table may not exist in the target system
    ENDTRY.

    " Components and feeder classes typed in the configuration content (FPM UIBBs).
    " The Web Dynpro configuration analyzer API is optional: resolve it fully dynamically.
    TRY.
        lv_intf = 'IF_WDR_CFG_ANALYZE_CONFIG_DATA'.
        CONCATENATE lv_intf '~GET_ATTRIBUTES_FOR_TYPE' INTO lv_method.

        " Parameter names of GET_ATTRIBUTES_FOR_TYPE (single importing + returning)
        CALL METHOD cl_abap_typedescr=>describe_by_name
          EXPORTING
            p_name         = lv_intf
          RECEIVING
            p_descr_ref    = lo_descr
          EXCEPTIONS
            type_not_found = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
        lo_intfdescr ?= lo_descr.
        READ TABLE lo_intfdescr->methods INTO ls_methdescr WITH KEY name = 'GET_ATTRIBUTES_FOR_TYPE'.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
        LOOP AT ls_methdescr-parameters INTO ls_parmdescr.
          CASE ls_parmdescr-parm_kind.
            WHEN cl_abap_objectdescr=>importing.
              lv_par_in = ls_parmdescr-name.
            WHEN cl_abap_objectdescr=>returning.
              lv_par_out = ls_parmdescr-name.
          ENDCASE.
        ENDLOOP.
        IF lv_par_in IS INITIAL OR lv_par_out IS INITIAL.
          RETURN.
        ENDIF.

        CREATE DATA lr_analyzer TYPE REF TO (lv_intf).
        ASSIGN lr_analyzer->* TO <lo_analyzer>.
        ls_config_key = me->key-obj_name.
        CALL METHOD ('CL_WDR_CFG_ANALYZE_CONFIG_DATA')=>('CREATE_FOR_CONFIG_KEY')
          EXPORTING
            config_key           = ls_config_key
          IMPORTING
            config_data_analyzer = <lo_analyzer>.
        lo_analyzer = <lo_analyzer>.
        IF lo_analyzer IS NOT BOUND.
          RETURN.
        ENDIF.

        CREATE DATA lr_types TYPE (`IF_WDR_CFG_ANALYZE_CONFIG_DATA=>TY_T_TYPES`).
        ASSIGN lr_types->* TO <lt_types>.
        CREATE DATA lr_type LIKE LINE OF <lt_types>.
        ASSIGN lr_type->* TO <ls_type>.
        ASSIGN (`IF_WDR_CFG_ANALYZE_CONFIG_DATA=>CO_TYPE_KIND`) TO <ls_kinds>.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
        ASSIGN COMPONENT 'DDIC_ROLLNAME' OF STRUCTURE <ls_kinds> TO <lv_value>.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
        ASSIGN COMPONENT 'TYPE_KIND' OF STRUCTURE <ls_type> TO <lv_comp>.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
        <lv_comp> = <lv_value>.
        ASSIGN COMPONENT 'TYPE_NAME' OF STRUCTURE <ls_type> TO <lv_comp>.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
        <lv_comp> = 'FPM_COMPONENT_NAME'.
        INSERT <ls_type> INTO TABLE <lt_types>.
        <lv_comp> = 'WDY_COMPONENT_NAME'.
        INSERT <ls_type> INTO TABLE <lt_types>.
        <lv_comp> = 'SEOCLSNAME'.
        INSERT <ls_type> INTO TABLE <lt_types>.

        CREATE DATA lr_attributes TYPE (`IF_WDR_CFG_ANALYZE_CONFIG_DATA=>TY_T_ATTRIBUTE_VALUES`).
        ASSIGN lr_attributes->* TO <lt_attrs>.
        ls_ptab-name = lv_par_in.
        ls_ptab-kind = cl_abap_objectdescr=>exporting.
        ls_ptab-value = lr_types.
        INSERT ls_ptab INTO TABLE lt_ptab.
        ls_ptab-name = lv_par_out.
        ls_ptab-kind = cl_abap_objectdescr=>receiving.
        ls_ptab-value = lr_attributes.
        INSERT ls_ptab INTO TABLE lt_ptab.
        CALL METHOD lo_analyzer->(lv_method)
          PARAMETER-TABLE lt_ptab.

        LOOP AT <lt_attrs> ASSIGNING <ls_attr>.
          ASSIGN COMPONENT 'ATTRIBUTES' OF STRUCTURE <ls_attr> TO <lt_values>.
          CHECK sy-subrc = 0.
          LOOP AT <lt_values> ASSIGNING <ls_value>.
            ASSIGN COMPONENT 'VALUE' OF STRUCTURE <ls_value> TO <lv_value>.
            CHECK sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
            lv_name = <lv_value>.
            TRANSLATE lv_name TO UPPER CASE.
            ASSIGN COMPONENT 'TYPE' OF STRUCTURE <ls_value> TO <ls_type>.
            CHECK sy-subrc = 0.
            ASSIGN COMPONENT 'TYPE_NAME' OF STRUCTURE <ls_type> TO <lv_comp>.
            CHECK sy-subrc = 0.
            TRY.
                CLEAR ls_dependency.
                IF <lv_comp> = 'SEOCLSNAME'.
                  TRY.
                      CALL METHOD get_tadir_dependency
                        EXPORTING object = 'CLAS' obj_name = lv_name
                        RECEIVING dependency = ls_dependency.
                    CATCH cx_root.
                      CALL METHOD get_tadir_dependency
                        EXPORTING object = 'INTF' obj_name = lv_name
                        RECEIVING dependency = ls_dependency.
                  ENDTRY.
                ELSE.
                  CALL METHOD get_tadir_dependency
                    EXPORTING object = 'WDYN' obj_name = lv_name
                    RECEIVING dependency = ls_dependency.
                ENDIF.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDLOOP.
        ENDLOOP.
      CATCH cx_root.
        " optional Web Dynpro configuration API may not exist in the target system
    ENDTRY.

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.

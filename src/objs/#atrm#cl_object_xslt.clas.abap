CLASS /atrm/cl_object_xslt DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS /atrm/cl_object_xslt IMPLEMENTATION.
  METHOD /atrm/if_object~get_dependencies.
    TYPES: BEGIN OF ty_target,
             object TYPE trobjtype,
             obj_name TYPE sobj_name,
           END OF ty_target.
    DATA:
      lo_xslt_api TYPE REF TO object,
      lr_source TYPE REF TO data,
      lv_source_type TYPE string,
      lv_source TYPE string,
      lv_source_lower TYPE string,
      lv_devclass TYPE devclass,
      lv_attribute TYPE string,
      lv_quote TYPE c LENGTH 1,
      lv_search_offset TYPE i,
      lv_match_offset TYPE i,
      lv_value_offset TYPE i,
      lv_value_length TYPE i,
      lv_index TYPE i,
      lv_target_text TYPE string,
      lt_targets TYPE STANDARD TABLE OF ty_target,
      ls_target TYPE ty_target,
      lt_attributes TYPE STANDARD TABLE OF string,
      ls_dependency TYPE /atrm/object_dependency.
    FIELD-SYMBOLS:
      <lt_source> TYPE ANY TABLE,
      <ls_source_line> TYPE any,
      <lv_line> TYPE any.

    lv_source_type = 'O2PAGELINE_TABLE'.
    TRY.
        CREATE OBJECT lo_xslt_api TYPE ('CL_O2_XSLT_API_INTERNAL').
        CREATE DATA lr_source TYPE (lv_source_type).
        ASSIGN lr_source->* TO <lt_source>.
        CALL METHOD lo_xslt_api->('READ_LOCAL_TRANSFORMATION')
          EXPORTING i_transformation_name = me->key-obj_name
          IMPORTING e_transformation_source = <lt_source>.
        LOOP AT <lt_source> ASSIGNING <ls_source_line>.
          ASSIGN COMPONENT 'LINE' OF STRUCTURE <ls_source_line> TO <lv_line>.
          IF sy-subrc = 0.
            IF lv_source IS INITIAL.
              lv_source = <lv_line>.
            ELSE.
              CONCATENATE lv_source <lv_line> INTO lv_source
                SEPARATED BY space.
            ENDIF.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        " Internal source reader is unavailable on older releases
    ENDTRY.

    IF lv_source IS INITIAL.
      TRY.
          CLEAR lr_source.
          CREATE DATA lr_source TYPE (lv_source_type).
          ASSIGN lr_source->* TO <lt_source>.
          CALL METHOD ('CL_O2_API_XSLTDESC')=>('LOAD')
            EXPORTING p_xslt_desc = me->key-obj_name
            IMPORTING p_source = <lt_source>.
          LOOP AT <lt_source> ASSIGNING <ls_source_line>.
            ASSIGN COMPONENT 'LINE' OF STRUCTURE <ls_source_line> TO <lv_line>.
            IF sy-subrc = 0.
              IF lv_source IS INITIAL.
                lv_source = <lv_line>.
              ELSE.
                CONCATENATE lv_source <lv_line> INTO lv_source
                  SEPARATED BY space.
              ENDIF.
            ENDIF.
          ENDLOOP.
        CATCH cx_root.
          " Classic transformation API is optional across releases
      ENDTRY.
    ENDIF.

    CHECK lv_source IS NOT INITIAL.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
      IN lv_source WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
      IN lv_source WITH space.
    lv_source_lower = lv_source.
    TRANSLATE lv_source_lower TO LOWER CASE.

    APPEND 'href=' TO lt_attributes.
    APPEND 'src=' TO lt_attributes.
    LOOP AT lt_attributes INTO lv_attribute.
      lv_search_offset = 0.
      DO.
        FIND FIRST OCCURRENCE OF lv_attribute
          IN lv_source_lower+lv_search_offset MATCH OFFSET lv_match_offset.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        lv_value_offset = lv_search_offset + lv_match_offset
          + strlen( lv_attribute ).
        IF lv_value_offset >= strlen( lv_source_lower ).
          EXIT.
        ENDIF.
        lv_quote = lv_source_lower+lv_value_offset(1).
        IF lv_quote = '"' OR lv_quote = ''''.
          lv_value_offset = lv_value_offset + 1.
          FIND FIRST OCCURRENCE OF lv_quote
            IN lv_source_lower+lv_value_offset MATCH OFFSET lv_value_length.
          IF sy-subrc <> 0 OR lv_value_length <= 0.
            EXIT.
          ENDIF.
          CLEAR ls_target.
          lv_target_text = lv_source+lv_value_offset(lv_value_length).
          TRANSLATE lv_target_text TO UPPER CASE.
          ls_target-object = 'XSLT'.
          ls_target-obj_name = lv_target_text.
          APPEND ls_target TO lt_targets.
          lv_search_offset = lv_value_offset + lv_value_length + 1.
        ELSE.
          lv_search_offset = lv_value_offset.
        ENDIF.
      ENDDO.
    ENDLOOP.

    lv_search_offset = 0.
    DO.
      FIND FIRST OCCURRENCE OF 'type='
        IN lv_source_lower+lv_search_offset MATCH OFFSET lv_match_offset.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      lv_value_offset = lv_search_offset + lv_match_offset + 5.
      IF lv_value_offset >= strlen( lv_source_lower ).
        EXIT.
      ENDIF.
      lv_quote = lv_source_lower+lv_value_offset(1).
      IF lv_quote = '"' OR lv_quote = ''''.
        lv_value_offset = lv_value_offset + 1.
        FIND FIRST OCCURRENCE OF lv_quote
          IN lv_source_lower+lv_value_offset MATCH OFFSET lv_value_length.
        IF sy-subrc <> 0 OR lv_value_length <= 0.
          EXIT.
        ENDIF.
        lv_target_text = lv_source_lower+lv_value_offset(lv_value_length).
        IF lv_target_text(5) = 'ddic:'.
          SHIFT lv_target_text BY 5 PLACES LEFT.
          TRANSLATE lv_target_text TO UPPER CASE.
          CLEAR ls_target.
          ls_target-object = 'TABL'.
          ls_target-obj_name = lv_target_text.
          APPEND ls_target TO lt_targets.
        ENDIF.
        lv_search_offset = lv_value_offset + lv_value_length + 1.
      ELSE.
        lv_search_offset = lv_value_offset.
      ENDIF.
    ENDDO.

    LOOP AT lt_targets INTO ls_target.
      CLEAR lv_devclass.
      SELECT SINGLE devclass FROM tadir INTO lv_devclass
        WHERE pgmid = 'R3TR'
          AND object = ls_target-object
          AND obj_name = ls_target-obj_name.
      IF sy-subrc = 0.
        CLEAR ls_dependency.
        TRY.
            CALL METHOD get_tadir_dependency
              EXPORTING object = ls_target-object
                        obj_name = ls_target-obj_name
              RECEIVING dependency = ls_dependency.
          CATCH cx_root.
            " Build exact TADIR key below if helper lookup is unavailable
        ENDTRY.
        ls_dependency-tabname = 'TADIR'.
        CONCATENATE 'R3TR' ls_target-object ls_target-obj_name
          INTO ls_dependency-tabkey.
        ls_dependency-devclass = lv_devclass.
        APPEND ls_dependency TO dependencies.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

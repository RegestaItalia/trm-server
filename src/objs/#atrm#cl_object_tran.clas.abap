CLASS /atrm/cl_object_tran DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS /atrm/cl_object_tran IMPLEMENTATION.
  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_table TYPE tabname VALUE 'TSTCP',
          lv_where TYPE string,
          lv_text TYPE string,
          lv_name TYPE sobj_name,
          lv_offset TYPE i,
          lv_length TYPE i,
          lr_rows TYPE REF TO data,
          ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS: <lt_rows> TYPE STANDARD TABLE,
                   <ls_row> TYPE any,
                   <lv_param> TYPE any.

    super->/atrm/if_object~get_dependencies(
      IMPORTING dependencies = dependencies
    ).

    CONCATENATE 'TCODE = ''' me->key-obj_name '''' INTO lv_where.

    TRY.
        CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_rows->* TO <lt_rows>.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).

        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'PARAM' OF STRUCTURE <ls_row> TO <lv_param>.
          CHECK sy-subrc = 0 AND <lv_param> IS NOT INITIAL.
          lv_text = <lv_param>.
          FIND FIRST OCCURRENCE OF 'CLASS=' IN lv_text
            MATCH OFFSET lv_offset.
          CHECK sy-subrc = 0.
          lv_offset = lv_offset + 6.
          CHECK lv_offset < strlen( lv_text ).
          FIND FIRST OCCURRENCE OF ';' IN lv_text+lv_offset
            MATCH OFFSET lv_length.
          IF sy-subrc <> 0.
            lv_length = strlen( lv_text ) - lv_offset.
          ENDIF.
          CHECK lv_length > 0.
          lv_name = lv_text+lv_offset(lv_length).
          CONDENSE lv_name NO-GAPS.
          TRANSLATE lv_name TO UPPER CASE.
          CHECK lv_name IS NOT INITIAL.

          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING
                  object = 'CLAS'
                  obj_name = lv_name
                RECEIVING
                  dependency = ls_dependency.
              READ TABLE dependencies TRANSPORTING NO FIELDS
                WITH KEY tabname = ls_dependency-tabname
                         tabkey = ls_dependency-tabkey.
              IF sy-subrc <> 0.
                APPEND ls_dependency TO dependencies.
              ENDIF.
            CATCH cx_root.
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

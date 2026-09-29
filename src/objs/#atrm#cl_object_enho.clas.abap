CLASS /atrm/cl_object_enho DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS /atrm/cl_object_enho IMPLEMENTATION.
  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_table TYPE tabname VALUE 'ENHOBJ',
          lv_where TYPE string,
          lr_rows TYPE REF TO data,
          ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS: <lt_rows> TYPE STANDARD TABLE,
                   <ls_row> TYPE any,
                   <lv_type> TYPE any,
                   <lv_name> TYPE any.

    super->/atrm/if_object~get_dependencies(
      IMPORTING dependencies = dependencies
    ).

    CONCATENATE 'ENHNAME = ''' me->key-obj_name
      ''' AND VERSION = ''A'' AND PGMID = ''R3TR''' INTO lv_where.

    TRY.
        CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_rows->* TO <lt_rows>.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).

        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'OBJ_TYPE' OF STRUCTURE <ls_row> TO <lv_type>.
          CHECK sy-subrc = 0 AND <lv_type> IS NOT INITIAL.
          ASSIGN COMPONENT 'OBJ_NAME' OF STRUCTURE <ls_row> TO <lv_name>.
          CHECK sy-subrc = 0 AND <lv_name> IS NOT INITIAL.

          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING
                  object = <lv_type>
                  obj_name = <lv_name>
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

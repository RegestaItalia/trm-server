CLASS /atrm/cl_object_dsel DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_dsel IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      ls_key        TYPE rsdsqcat,
      lt_tables     TYPE STANDARD TABLE OF rsdstabs,
      ls_table      TYPE rsdstabs,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        ls_key-origin = me->key-obj_name+0(3).
        ls_key-dbna = me->key-obj_name+3(14).
        ls_key-name = me->key-obj_name+17.

        IF ls_key-dbna IS NOT INITIAL.
          CLEAR ls_dependency.
          CALL METHOD get_tadir_dependency
            EXPORTING object = 'LDBA' obj_name = ls_key-dbna
            RECEIVING dependency = ls_dependency.
          IF ls_dependency IS NOT INITIAL.
            APPEND ls_dependency TO dependencies.
          ENDIF.
        ENDIF.

        CALL FUNCTION 'FREE_SELECTIONS_INIT'
          EXPORTING
            kind             = 'G'
            field_groups_key = ls_key
          TABLES
            tables_tab       = lt_tables
          EXCEPTIONS
            OTHERS           = 1.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        LOOP AT lt_tables INTO ls_table.
          IF ls_table-prim_tab IS NOT INITIAL.
            CLEAR ls_dependency.
            CALL METHOD get_tadir_dependency
              EXPORTING object = 'TABL' obj_name = ls_table-prim_tab
              RECEIVING dependency = ls_dependency.
            IF ls_dependency IS NOT INITIAL.
              APPEND ls_dependency TO dependencies.
            ENDIF.
          ENDIF.
          IF ls_table-sec_tab IS NOT INITIAL.
            CLEAR ls_dependency.
            CALL METHOD get_tadir_dependency
              EXPORTING object = 'TABL' obj_name = ls_table-sec_tab
              RECEIVING dependency = ls_dependency.
            IF ls_dependency IS NOT INITIAL.
              APPEND ls_dependency TO dependencies.
            ENDIF.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        " selection view APIs may not be available in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

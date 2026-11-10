CLASS /atrm/cl_object_smim DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_smim IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_table      TYPE tabname,
      lv_where      TYPE string,
      lv_loio       TYPE string,
      lv_parent     TYPE string,
      lv_path       TYPE string,
      lv_segment    TYPE string,
      lv_depth      TYPE i,
      lt_segments   TYPE STANDARD TABLE OF string,
      lv_appl       TYPE string,
      lr_row        TYPE REF TO data,
      ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <ls_row>    TYPE any,
      <lv_parent> TYPE any,
      <lv_name>   TYPE any.

    TRY.
        lv_table = 'SMIMLOIO'.
        CREATE DATA lr_row TYPE (lv_table).
        ASSIGN lr_row->* TO <ls_row>.

        " Walk the folder hierarchy: PROP08 is the parent folder, PROP09 the object name
        lv_loio = me->key-obj_name.
        WHILE lv_loio IS NOT INITIAL AND lv_depth < 50.
          lv_depth = lv_depth + 1.
          REPLACE ALL OCCURRENCES OF '''' IN lv_loio WITH ''''''.
          CONCATENATE 'LOIO_ID = ''' lv_loio '''' INTO lv_where.
          SELECT SINGLE * FROM (lv_table) INTO <ls_row> WHERE (lv_where).
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          ASSIGN COMPONENT 'PROP08' OF STRUCTURE <ls_row> TO <lv_parent>.
          ASSIGN COMPONENT 'PROP09' OF STRUCTURE <ls_row> TO <lv_name>.
          IF <lv_parent> IS NOT ASSIGNED OR <lv_name> IS NOT ASSIGNED.
            EXIT.
          ENDIF.
          lv_segment = <lv_name>.
          CONCATENATE '/' lv_segment lv_path INTO lv_path.
          lv_loio = <lv_parent>.

          " Parent folder
          IF lv_depth = 1 AND lv_loio IS NOT INITIAL.
            lv_parent = lv_loio.
            TRY.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'SMIM' obj_name = lv_parent
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
        ENDWHILE.

        " MIME objects below /SAP/BC/BSP/<namespace>/<application> belong to that BSP application
        TRANSLATE lv_path TO UPPER CASE.
        IF lv_path CP '/SAP/BC/BSP/*'.
          lv_path = lv_path+12.
          SPLIT lv_path AT '/' INTO TABLE lt_segments.
          IF lines( lt_segments ) >= 2.
            READ TABLE lt_segments INDEX 1 INTO lv_segment.
            READ TABLE lt_segments INDEX 2 INTO lv_appl.
            IF lv_segment <> 'SAP'.
              CONCATENATE '/' lv_segment '/' lv_appl INTO lv_appl.
            ENDIF.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'WAPA' obj_name = lv_appl
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
        ENDIF.
      CATCH cx_root.
        " optional repository table may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

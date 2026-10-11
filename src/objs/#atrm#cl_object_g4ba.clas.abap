CLASS /atrm/cl_object_g4ba DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_g4ba IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lr_table        TYPE REF TO data,
      lv_table        TYPE tabname,
      lv_group        TYPE sobj_name,
      lv_service      TYPE sobj_name,
      lv_where        TYPE string.

    FIELD-SYMBOLS:
      <lt_rows>        TYPE STANDARD TABLE,
      <ls_row>         TYPE any,
      <lv_service>     TYPE any.

    lv_group = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_group WITH ''''''.

    TRY.
        lv_table = '/IWBEP/I_V4_MSGA'.
        CREATE DATA lr_table TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_table->* TO <lt_rows>.
        CONCATENATE 'GROUP_ID = ''' lv_group '''' INTO lv_where.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).

        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'SERVICE_ID' OF STRUCTURE <ls_row> TO <lv_service>.
          CHECK sy-subrc = 0.
          CHECK <lv_service> IS NOT INITIAL.

          lv_service = <lv_service>.
          REPLACE ALL OCCURRENCES OF '''' IN lv_service WITH ''''''.
          CONCATENATE 'SERVICE_ID = ''' lv_service '''' INTO lv_where.
          CALL METHOD append_composite_deps
            EXPORTING
              table_name    = '/IWBEP/I_V4_MSRV'
              where_clause  = lv_where
              first_field   = 'SERVICE_ID'
              second_field  = 'SERVICE_VERSION'
              second_offset = 36
              object_type   = 'G4BS'
            CHANGING
              dependencies  = dependencies.
        ENDLOOP.
      CATCH cx_root.
        " optional Gateway V4 API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

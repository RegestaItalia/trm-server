CLASS /atrm/cl_object_smod DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ty_component_type TYPE c LENGTH 1,
           ty_member_name    TYPE c LENGTH 100,
           ty_program_name   TYPE c LENGTH 40,
           ty_group_name     TYPE c LENGTH 40,
           ty_function_name  TYPE c LENGTH 30,
           ty_badi_name      TYPE c LENGTH 20,
           ty_spot_name      TYPE c LENGTH 30,
           ty_migration_name TYPE c LENGTH 20.

    TYPES:
      BEGIN OF ty_component,
        typ    TYPE ty_component_type,
        member TYPE ty_member_name,
      END OF ty_component,
      tyt_component TYPE STANDARD TABLE OF ty_component.

    METHODS add_tadir_dependency
      IMPORTING
        iv_object TYPE any
        iv_obj_name TYPE any
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t.

    METHODS add_tfdir_dependency
      IMPORTING
        iv_funcname TYPE any
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t.

    METHODS add_program_dependency
      IMPORTING
        iv_program TYPE any
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t.

    METHODS add_screen_menu_dependencies
      IMPORTING
        is_component TYPE ty_component
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t.

    METHODS add_component_dependencies
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t.

    METHODS add_migration_dependencies
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t.
ENDCLASS.



CLASS /atrm/cl_object_smod IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    TRY.
        CALL METHOD add_component_dependencies
          CHANGING
            ct_dependencies = dependencies.

        CALL METHOD add_migration_dependencies
          CHANGING
            ct_dependencies = dependencies.
      CATCH cx_root.
        " SMOD is optional. Missing enhancement repository objects must
        " not prevent this class from being used on older systems.
    ENDTRY.
  ENDMETHOD.

  METHOD add_tadir_dependency.
    DATA ls_dependency TYPE /atrm/object_dependency.

    CHECK iv_object IS NOT INITIAL.
    CHECK iv_obj_name IS NOT INITIAL.

    CLEAR ls_dependency.
    TRY.
        CALL METHOD get_tadir_dependency
          EXPORTING
            object     = iv_object
            obj_name   = iv_obj_name
          RECEIVING
            dependency = ls_dependency.

        READ TABLE ct_dependencies
          WITH KEY tabname = 'TADIR'
                   tabkey  = ls_dependency-tabkey
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND ls_dependency TO ct_dependencies.
        ENDIF.
      CATCH /atrm/cx_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD add_tfdir_dependency.
    DATA ls_dependency TYPE /atrm/object_dependency.

    CHECK iv_funcname IS NOT INITIAL.

    CLEAR ls_dependency.
    TRY.
        CALL METHOD get_tfdir_dependency
          EXPORTING
            funcname   = iv_funcname
          RECEIVING
            dependency = ls_dependency.

        READ TABLE ct_dependencies
          WITH KEY tabname = 'TFDIR'
                   tabkey  = ls_dependency-tabkey
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND ls_dependency TO ct_dependencies.
        ENDIF.
      CATCH /atrm/cx_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD add_program_dependency.
    DATA: lv_program_name TYPE ty_program_name,
          lv_group_name   TYPE ty_group_name.

    lv_program_name = iv_program.
    CHECK lv_program_name IS NOT INITIAL.

    CALL METHOD add_tadir_dependency
      EXPORTING
        iv_object       = 'PROG'
        iv_obj_name     = lv_program_name
      CHANGING
        ct_dependencies = ct_dependencies.

    IF lv_program_name(4) EQ 'SAPL'.
      lv_group_name = lv_program_name+4.
      CALL METHOD add_tadir_dependency
        EXPORTING
          iv_object       = 'FUGR'
          iv_obj_name     = lv_group_name
        CHANGING
          ct_dependencies = ct_dependencies.
    ENDIF.
  ENDMETHOD.

  METHOD add_screen_menu_dependencies.
    DATA: lv_function_name TYPE ty_function_name,
          lv_calling_prog  TYPE ty_program_name,
          lv_customer_prog TYPE ty_program_name.

    lv_function_name = 'MOD_SAP_MEMBER_PARTS'.

    TRY.
        CALL FUNCTION lv_function_name
          EXPORTING
            member    = is_component-member
            typ       = is_component-typ
          IMPORTING
            gprogname = lv_calling_prog
            cprogname = lv_customer_prog
          EXCEPTIONS
            wrong_name = 1
            wrong_type = 2
            OTHERS     = 3.

        CHECK sy-subrc EQ 0.

        CALL METHOD add_program_dependency
          EXPORTING
            iv_program      = lv_calling_prog
          CHANGING
            ct_dependencies = ct_dependencies.

        IF lv_customer_prog NE lv_calling_prog.
          CALL METHOD add_program_dependency
            EXPORTING
              iv_program      = lv_customer_prog
            CHANGING
              ct_dependencies = ct_dependencies.
        ENDIF.
      CATCH cx_root.
        " The SMOD component parser is not available on every release.
    ENDTRY.
  ENDMETHOD.

  METHOD add_component_dependencies.
    DATA: lv_table_name TYPE tabname,
          lt_components TYPE tyt_component,
          ls_component  TYPE ty_component.

    lv_table_name = 'MODSAP'.

    TRY.
        SELECT typ member
          FROM (lv_table_name)
          INTO TABLE lt_components
          WHERE name EQ me->key-obj_name
            AND member NE space.

        LOOP AT lt_components INTO ls_component.
          CASE ls_component-typ.
            WHEN 'E'.
              CALL METHOD add_tfdir_dependency
                EXPORTING
                  iv_funcname     = ls_component-member
                CHANGING
                  ct_dependencies = ct_dependencies.
            WHEN 'S' OR 'C'.
              CALL METHOD add_screen_menu_dependencies
                EXPORTING
                  is_component    = ls_component
                CHANGING
                  ct_dependencies = ct_dependencies.
            WHEN 'T'.
              CALL METHOD add_tadir_dependency
                EXPORTING
                  iv_object       = 'TABL'
                  iv_obj_name     = ls_component-member
                CHANGING
                  ct_dependencies = ct_dependencies.
          ENDCASE.
        ENDLOOP.
      CATCH cx_root.
        " The SMOD repository table is not available on every release.
    ENDTRY.
  ENDMETHOD.

  METHOD add_migration_dependencies.
    DATA: lv_table_name      TYPE tabname,
          lv_badi_definition TYPE ty_badi_name,
          lv_spot_name       TYPE ty_spot_name,
          lv_migration_name  TYPE ty_migration_name.

    lv_table_name = 'MODSAPA'.

    TRY.
        SELECT SINGLE badi_def
          FROM (lv_table_name)
          INTO lv_badi_definition
          WHERE name EQ me->key-obj_name.

        IF lv_badi_definition IS NOT INITIAL.
          CALL METHOD add_tadir_dependency
            EXPORTING
              iv_object       = 'SXSD'
              iv_obj_name     = lv_badi_definition
            CHANGING
              ct_dependencies = ct_dependencies.
        ENDIF.
      CATCH cx_root.
        " BADI_DEF is optional on older MODSAPA versions.
    ENDTRY.

    lv_table_name = 'BADI_SPOT'.
    CONCATENATE '%' me->key-obj_name INTO lv_migration_name.

    TRY.
        SELECT SINGLE enhspotname
          FROM (lv_table_name)
          INTO lv_spot_name
          WHERE mig_exit_name EQ lv_migration_name.

        IF lv_spot_name IS NOT INITIAL.
          CALL METHOD add_tadir_dependency
            EXPORTING
              iv_object       = 'ENHS'
              iv_obj_name     = lv_spot_name
            CHANGING
              ct_dependencies = ct_dependencies.
        ENDIF.
      CATCH cx_root.
        " Direct enhancement spot migration metadata is optional.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

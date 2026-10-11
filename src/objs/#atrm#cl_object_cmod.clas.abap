CLASS /atrm/cl_object_cmod DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ty_member_name TYPE c LENGTH 100,
           ty_badi_name   TYPE c LENGTH 20.

    TYPES:
      BEGIN OF ty_member,
        member TYPE ty_member_name,
      END OF ty_member,
      tyt_member TYPE STANDARD TABLE OF ty_member.

    TYPES:
      BEGIN OF ty_badi,
        badi_imp TYPE ty_badi_name,
      END OF ty_badi,
      tyt_badi TYPE STANDARD TABLE OF ty_badi.

    METHODS add_tadir_dependency
      IMPORTING
        iv_object TYPE any
        iv_obj_name TYPE any
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t.

    METHODS add_smod_dependencies
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t.

    METHODS add_badi_dependencies
      CHANGING
        ct_dependencies TYPE /atrm/object_dependency_t.
ENDCLASS.



CLASS /atrm/cl_object_cmod IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    TRY.
        CALL METHOD add_smod_dependencies
          CHANGING
            ct_dependencies = dependencies.

        CALL METHOD add_badi_dependencies
          CHANGING
            ct_dependencies = dependencies.
      CATCH cx_root.
        " CMOD is optional. Missing enhancement repository objects must
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

  METHOD add_smod_dependencies.
    DATA: lv_table_name TYPE tabname,
          lt_members    TYPE tyt_member,
          ls_member     TYPE ty_member.

    lv_table_name = 'MODACT'.

    TRY.
        SELECT member
          FROM (lv_table_name)
          INTO TABLE lt_members
          WHERE name EQ me->key-obj_name
            AND member NE space.

        LOOP AT lt_members INTO ls_member.
          CALL METHOD add_tadir_dependency
            EXPORTING
              iv_object       = 'SMOD'
              iv_obj_name     = ls_member-member
            CHANGING
              ct_dependencies = ct_dependencies.
        ENDLOOP.
      CATCH cx_root.
        " The CMOD repository table is not available on every release.
    ENDTRY.
  ENDMETHOD.

  METHOD add_badi_dependencies.
    DATA: lv_table_name TYPE tabname,
          lt_badis      TYPE tyt_badi,
          ls_badi       TYPE ty_badi,
          lv_badi_name  TYPE ty_badi_name.

    lv_table_name = 'MODACT'.

    TRY.
        SELECT badi_imp
          FROM (lv_table_name)
          INTO TABLE lt_badis
          WHERE name EQ me->key-obj_name
            AND badi_imp NE space.

        LOOP AT lt_badis INTO ls_badi.
          CALL METHOD add_tadir_dependency
            EXPORTING
              iv_object       = 'SXCI'
              iv_obj_name     = ls_badi-badi_imp
            CHANGING
              ct_dependencies = ct_dependencies.

          CALL METHOD add_tadir_dependency
            EXPORTING
              iv_object       = 'ENHO'
              iv_obj_name     = ls_badi-badi_imp
            CHANGING
              ct_dependencies = ct_dependencies.

          lv_badi_name = ls_badi-badi_imp.
          IF lv_badi_name+1 IS NOT INITIAL.
            lv_badi_name = lv_badi_name+1.
            CALL METHOD add_tadir_dependency
              EXPORTING
                iv_object       = 'SXCI'
                iv_obj_name     = lv_badi_name
              CHANGING
                ct_dependencies = ct_dependencies.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        " BADI_IMP was added in later releases and is therefore optional.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

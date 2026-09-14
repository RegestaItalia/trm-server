CLASS /atrm/cl_object_doma DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_doma IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_convexit   TYPE dd01l-convexit,
      lv_funcname   TYPE tfdir-funcname,
      lv_fugr       TYPE tadir-obj_name,
      ls_dependency TYPE /atrm/object_dependency.

    super->/atrm/if_object~get_dependencies(
      IMPORTING dependencies = dependencies
    ).

    SELECT SINGLE convexit
      FROM dd01l
      INTO lv_convexit
      WHERE domname = me->key-obj_name
        AND as4local = 'A'.

    CHECK sy-subrc = 0.
    CHECK lv_convexit IS NOT INITIAL.

    CONCATENATE 'CONVERSION_EXIT_' lv_convexit '_INPUT'
      INTO lv_funcname.

    SELECT SINGLE area
      FROM v_fdir
      INTO lv_fugr
      WHERE funcname = lv_funcname.

    CHECK sy-subrc = 0.
    CHECK lv_fugr IS NOT INITIAL.

    TRY.
        CLEAR ls_dependency.
        CALL METHOD get_tadir_dependency
          EXPORTING
            object     = 'FUGR'
            obj_name   = lv_fugr
          RECEIVING
            dependency = ls_dependency.

        READ TABLE dependencies
          WITH KEY tabname = ls_dependency-tabname
                   tabkey = ls_dependency-tabkey
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND ls_dependency TO dependencies.
        ENDIF.

        CLEAR ls_dependency.
        CALL METHOD get_tfdir_dependency
          EXPORTING
            funcname   = lv_funcname
          RECEIVING
            dependency = ls_dependency.

        READ TABLE dependencies
          WITH KEY tabname = ls_dependency-tabname
                   tabkey = ls_dependency-tabkey
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND ls_dependency TO dependencies.
        ENDIF.
      CATCH cx_root.
        " optional dependency may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS /atrm/cl_object_sush DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_sush IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name       TYPE usobkey-name,
      lv_type       TYPE usobkey-type,
      ls_hash       TYPE usobhash,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        lv_name = me->key-obj_name(32).
        CONDENSE lv_name.
        lv_type = me->key-obj_name+32(2).

        SELECT SINGLE *
          FROM usobhash
          INTO ls_hash
          WHERE name = lv_name
            AND type = lv_type.

        CHECK sy-subrc = 0.
        CHECK ls_hash-pgmid = 'R3TR'.
        CHECK ls_hash-object IS NOT INITIAL.
        CHECK ls_hash-obj_name IS NOT INITIAL.

        CALL METHOD get_tadir_dependency
          EXPORTING object = ls_hash-object obj_name = ls_hash-obj_name
          RECEIVING dependency = ls_dependency.
        APPEND ls_dependency TO dependencies.
      CATCH cx_root.
        " optional SU22 API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

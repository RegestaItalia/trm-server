CLASS zcl_trm_object_ddlx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_trm_object .
    DATA: key TYPE ztrm_object READ-ONLY.

    METHODS constructor
      IMPORTING key TYPE ztrm_object.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_object_ddlx IMPLEMENTATION.

  METHOD constructor.
    me->key = key.
  ENDMETHOD.

  METHOD zif_trm_object~get_dependencies.
    DATA: lo_provider   TYPE REF TO object,
          lt_queries    TYPE REF TO data,
          lt_ddlx_names TYPE REF TO data,
          lv_tabkey     TYPE string.
    FIELD-SYMBOLS: <fs_queries>    TYPE STANDARD TABLE,
                   <fs_query>      TYPE any,
                   <fs_entity>     TYPE any,
                   <fs_ddlx_names> TYPE STANDARD TABLE,
                   <fs_row>        TYPE any,
                   <fs_dependency> TYPE ztrm_object_dependency.

    TRY.
        CREATE OBJECT lo_provider TYPE ('CL_DDLX_METADATA_PROVIDER').
        CREATE DATA lt_queries TYPE ('IF_DDLX_METADATA_PROVIDER=>TY_T_MDATA_QUERY').
        CREATE DATA lt_ddlx_names TYPE ('IF_DDLX_METADATA_PROVIDER=>TY_T_DDLXNAME_EXT').
        ASSIGN lt_queries->* TO <fs_queries>.
        CHECK <fs_queries> IS ASSIGNED.
        ASSIGN lt_ddlx_names->* TO <fs_ddlx_names>.
        CHECK <fs_ddlx_names> IS ASSIGNED.
        APPEND INITIAL LINE TO <fs_queries> ASSIGNING <fs_query>.
        CHECK <fs_query> IS ASSIGNED.
        ASSIGN COMPONENT 'ENTITY' OF STRUCTURE <fs_query> TO <fs_entity>.
        CHECK <fs_entity> IS ASSIGNED.
        <fs_entity> = key-obj_name.

        CALL METHOD lo_provider->('GET_ANNOTATIONS_FOR_ENTITIES')
          EXPORTING
            i_mdata_queries = <fs_queries>
          IMPORTING
            e_ddlxnames     = <fs_ddlx_names>.
      CATCH cx_dynamic_check.
        RETURN.
    ENDTRY.

    LOOP AT <fs_ddlx_names> ASSIGNING <fs_row>.
      UNASSIGN <fs_entity>.
      CLEAR lv_tabkey.
      ASSIGN COMPONENT 'ENTITY' OF STRUCTURE <fs_row> TO <fs_entity>.
      CHECK <fs_entity> IS ASSIGNED.
      CONCATENATE 'R3TR' 'DDLS' <fs_entity> INTO lv_tabkey.
      READ TABLE et_dependencies TRANSPORTING NO FIELDS WITH KEY tabname = 'TADIR' tabkey = lv_tabkey.
      CHECK sy-subrc <> 0.
      APPEND INITIAL LINE TO et_dependencies ASSIGNING <fs_dependency>.
      <fs_dependency>-tabname = 'TADIR'.
      <fs_dependency>-tabkey = lv_tabkey.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

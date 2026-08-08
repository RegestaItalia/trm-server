CLASS /atrm/cl_object_sicf DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ty_icf_name  TYPE c LENGTH 15,
           ty_icfguid   TYPE c LENGTH 25,
           ty_icf_hand  TYPE c LENGTH 32,
           ty_icfurlbuf TYPE c LENGTH 116.
    TYPES:
      BEGIN OF ty_parent,
        path       TYPE string,
        icfname    TYPE ty_icf_name,
        icfnodguid TYPE ty_icfguid,
      END OF ty_parent,
      tyt_parent TYPE STANDARD TABLE OF ty_parent WITH DEFAULT KEY.

    CLASS-METHODS get_parent_nodes
      IMPORTING
        iv_icf_name      TYPE ty_icf_name
        iv_icfparguid    TYPE ty_icfguid
      RETURNING
        VALUE(rt_parent) TYPE tyt_parent.

    METHODS single_dependencies
      IMPORTING
        !serv_info    TYPE REF TO data
      CHANGING
        !dependencies TYPE /atrm/object_dependency_t.
ENDCLASS.



CLASS /atrm/cl_object_sicf IMPLEMENTATION.

  METHOD get_parent_nodes.
    TYPES: tyt_parts TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA:
      lv_node_guid   TYPE ty_icfguid,
      lv_icf_name    TYPE ty_icf_name,
      lv_url         TYPE ty_icfurlbuf,
      lv_ext_url     TYPE string,
      lv_query       TYPE string,
      lv_path        TYPE string,
      lt_parts       TYPE tyt_parts,
      lv_parts_count TYPE i,
      lv_part        LIKE LINE OF lt_parts,
      lv_parent_path TYPE string.
    FIELD-SYMBOLS: <fs_parent> TYPE ty_parent.

    SELECT SINGLE icfnodguid FROM icfservice INTO lv_node_guid WHERE icf_name EQ iv_icf_name AND icfparguid EQ iv_icfparguid.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'HTTP_GET_URL_FROM_NODGUID'
      EXPORTING
        nodguid      = lv_node_guid
      IMPORTING
        url          = lv_url
        extended_url = lv_ext_url
      EXCEPTIONS
        icf_inconst  = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF lv_ext_url IS NOT INITIAL.
      lv_path = lv_ext_url.
    ELSE.
      lv_path = lv_url.
    ENDIF.

    SPLIT lv_path AT '?' INTO lv_path lv_query.

    SPLIT lv_path AT '/' INTO TABLE lt_parts.
    DELETE lt_parts WHERE table_line IS INITIAL.

    IF lt_parts IS NOT INITIAL.
      DESCRIBE TABLE lt_parts LINES lv_parts_count.
      DELETE lt_parts INDEX lv_parts_count.
    ENDIF.

    CLEAR lv_parent_path.

    LOOP AT lt_parts INTO lv_part.
      CONCATENATE lv_parent_path lv_part INTO lv_parent_path SEPARATED BY '/'.
      CLEAR: lv_node_guid, lv_icf_name.
      CALL METHOD ('CL_ICF_TREE')=>('IF_ICF_TREE~SERVICE_FROM_URL')
        EXPORTING
          url                   = lv_parent_path
          hostnumber            = 0
          authority_check       = ' '
        IMPORTING
          icfnodguid            = lv_node_guid
          icf_name              = lv_icf_name
        EXCEPTIONS
          wrong_application     = 1
          no_application        = 2
          not_allow_application = 3
          wrong_url             = 4
          no_authority          = 5
          OTHERS                = 6.

      IF sy-subrc EQ 0.
        UNASSIGN <fs_parent>.
        APPEND INITIAL LINE TO rt_parent ASSIGNING <fs_parent>.
        <fs_parent>-path       = lv_parent_path.
        <fs_parent>-icfnodguid = lv_node_guid.
        <fs_parent>-icfname     = lv_icf_name.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD /atrm/if_object~get_dependencies.
    TRY.
        TYPES: BEGIN OF ty_parent_icf,
                 icf_name   TYPE ty_icf_name,
                 icfparguid TYPE ty_icfguid,
                 icfnodguid TYPE ty_icfguid,
               END OF ty_parent_icf,
               tyt_parent_icf TYPE STANDARD TABLE OF ty_parent_icf WITH DEFAULT KEY.
        DATA: lv_icf_name   TYPE ty_icf_name,
              lv_icfparguid TYPE ty_icfguid,
              lt_parent     TYPE tyt_parent,
              lt_parent_icf TYPE tyt_parent_icf,
              ls_parent_icf LIKE LINE OF lt_parent_icf,
              obj_name      TYPE sobj_name,
              ls_dependency TYPE /atrm/object_dependency,
              lt_serv_info  TYPE REF TO data.
        FIELD-SYMBOLS: <fs_serv_info>  TYPE ANY TABLE.
        lv_icf_name = me->key-obj_name(15).
        lv_icfparguid = me->key-obj_name+15.
        lt_parent = get_parent_nodes( iv_icf_name = lv_icf_name iv_icfparguid = lv_icfparguid ).
        CREATE DATA lt_serv_info TYPE ('ICFSERVTBL').
        ASSIGN lt_serv_info->* TO <fs_serv_info>.
        CALL METHOD ('CL_ICF_TREE')=>('IF_ICF_TREE~GET_INFO_FROM_SERV')
          EXPORTING
            icf_name          = lv_icf_name
            icfparguid        = lv_icfparguid
          IMPORTING
            serv_info         = <fs_serv_info>
          EXCEPTIONS
            wrong_name        = 1
            wrong_parguid     = 2
            incorrect_service = 3
            no_authority      = 4
            OTHERS            = 5.
        CHECK sy-subrc EQ 0.
        single_dependencies(
          EXPORTING
            serv_info    = lt_serv_info
          CHANGING
            dependencies = dependencies
        ).
        IF lt_parent[] IS NOT INITIAL.
          SELECT icf_name icfparguid icfnodguid FROM icfservice
          INTO CORRESPONDING FIELDS OF TABLE lt_parent_icf
          FOR ALL ENTRIES IN lt_parent
          WHERE icf_name EQ lt_parent-icfname
          AND icfnodguid EQ lt_parent-icfnodguid.
          LOOP AT lt_parent_icf INTO ls_parent_icf.
            CLEAR obj_name.
            obj_name+0(15)  = ls_parent_icf-icf_name.
            obj_name+15(25) = ls_parent_icf-icfparguid.
            TRY.
                get_tadir_dependency(
                  EXPORTING
                    object     = 'SICF'
                    obj_name   = obj_name
                  RECEIVING
                    dependency = ls_dependency
                ).
                READ TABLE dependencies WITH KEY tabname = 'TADIR' tabkey = ls_dependency-tabkey TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0.
                  APPEND ls_dependency TO dependencies.
                ENDIF.
              CATCH /atrm/cx_exception.
            ENDTRY.
            CLEAR lt_serv_info.
            UNASSIGN <fs_serv_info>.
            CREATE DATA lt_serv_info TYPE ('ICFSERVTBL').
            ASSIGN lt_serv_info->* TO <fs_serv_info>.
            CALL METHOD ('CL_ICF_TREE')=>('IF_ICF_TREE~GET_INFO_FROM_SERV')
              EXPORTING
                icf_name          = ls_parent_icf-icf_name
                icfparguid        = ls_parent_icf-icfparguid
              IMPORTING
                serv_info         = <fs_serv_info>
              EXCEPTIONS
                wrong_name        = 1
                wrong_parguid     = 2
                incorrect_service = 3
                no_authority      = 4
                OTHERS            = 5.
            CHECK sy-subrc EQ 0.
            single_dependencies(
              EXPORTING
                serv_info    = lt_serv_info
              CHANGING
                dependencies = dependencies
            ).
          ENDLOOP.
        ENDIF.
      CATCH cx_dynamic_check.
    ENDTRY.
  ENDMETHOD.

  METHOD single_dependencies.
    DATA: handlertbl TYPE REF TO data,
          dependency TYPE /atrm/object_dependency.
    FIELD-SYMBOLS: <serv_info>  TYPE ANY TABLE,
                   <info>       TYPE any,
                   <handlertbl> TYPE ANY TABLE,
                   <handler>    TYPE any,
                   <icfhandler> TYPE ty_icf_hand.
    CHECK serv_info IS BOUND.
    ASSIGN serv_info->* TO <serv_info>.
    CHECK sy-subrc EQ 0.
    LOOP AT <serv_info> ASSIGNING <info>.
      ASSIGN COMPONENT 'HANDLERTBL' OF STRUCTURE <info> TO <handlertbl>.
      CHECK sy-subrc EQ 0.
      LOOP AT <handlertbl> ASSIGNING <handler>.
        UNASSIGN <icfhandler>.
        ASSIGN COMPONENT 'ICFHANDLER' OF STRUCTURE <handler> TO <icfhandler>.
        CHECK sy-subrc EQ 0.
        CLEAR dependency.
        TRY.
            get_tadir_dependency(
              EXPORTING
                object     = 'CLAS'
                obj_name   = <icfhandler>
              RECEIVING
                dependency = dependency
            ).
            READ TABLE dependencies WITH KEY tabname = 'TADIR' tabkey = dependency-tabkey TRANSPORTING NO FIELDS.
            CHECK sy-subrc <> 0.
            APPEND dependency TO dependencies.
          CATCH /atrm/cx_exception.
        ENDTRY.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

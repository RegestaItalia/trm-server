*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_trm_package DEFINITION.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING iv_name     TYPE string
                iv_registry TYPE string
                is_manifest TYPE zif_trm_core=>ty_manifest OPTIONAL
                iv_manifest TYPE xstring OPTIONAL.

    DATA: xmanifest TYPE xstring READ-ONLY,
          manifest  TYPE zif_trm_core=>ty_manifest READ-ONLY,
          name      TYPE string READ-ONLY,
          version   TYPE string READ-ONLY,
          registry  TYPE string READ-ONLY.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_trm_package IMPLEMENTATION.

  METHOD constructor.
    me->xmanifest = iv_manifest.
    me->manifest = is_manifest.
    me->name = iv_name.
    me->registry = iv_registry.
    me->version = me->manifest-version.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_trm_manifest DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING is_manifest TYPE zif_trm_core=>ty_manifest
                iv_manifest TYPE xstring OPTIONAL.

    METHODS get_trm_package
      RETURNING VALUE(ro_package) TYPE REF TO lcl_trm_package.

    CLASS-METHODS from_abap_xml
      IMPORTING iv_xml             TYPE string
      RETURNING VALUE(ro_manifest) TYPE REF TO lcl_trm_manifest.

  PRIVATE SECTION.

    DATA: gv_manifest TYPE xstring,
          gs_manifest TYPE zif_trm_core=>ty_manifest.

ENDCLASS.

CLASS lcl_trm_manifest IMPLEMENTATION.

  METHOD constructor.
    gs_manifest = is_manifest.
    gv_manifest = iv_manifest.
  ENDMETHOD.

  METHOD from_abap_xml.
    DATA: lv_xml      TYPE xstring,
          ls_manifest TYPE zif_trm_core=>ty_manifest.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = iv_xml
      IMPORTING
        buffer = lv_xml.
    CHECK lv_xml IS NOT INITIAL.
    TRY.
        CALL TRANSFORMATION id SOURCE XML lv_xml RESULT trm_manifest = ls_manifest.
      CATCH cx_xslt_format_error.
    ENDTRY.
    CHECK ls_manifest IS NOT INITIAL.
    CREATE OBJECT ro_manifest EXPORTING is_manifest = ls_manifest iv_manifest = lv_xml.
  ENDMETHOD.

  METHOD get_trm_package.
    CREATE OBJECT ro_package
      EXPORTING
        iv_name     = gs_manifest-name
        iv_registry = gs_manifest-registry
        is_manifest = gs_manifest
        iv_manifest = gv_manifest.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_trm_transport DEFINITION.

  PUBLIC SECTION.

    TYPES: tyt_transport TYPE STANDARD TABLE OF REF TO lcl_trm_transport WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING iv_trkorr    TYPE trkorr
                iv_migration TYPE flag.

    METHODS get_linked_package
      RETURNING VALUE(ro_linked_package) TYPE REF TO lcl_trm_package.

    METHODS get_date
      RETURNING VALUE(rv_date) TYPE timestamp.

    METHODS get_devclass
      RETURNING VALUE(rv_devclass) TYPE devclass.

    CLASS-METHODS get_latest
      IMPORTING it_transports       TYPE tyt_transport
      RETURNING VALUE(ro_transport) TYPE REF TO lcl_trm_transport.

    DATA: trkorr    TYPE trkorr,
          migration TYPE flag.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_documentation,
             langu   TYPE doku_langu,
             version TYPE dokvers,
             value   TYPE string,
           END OF ty_documentation.

    TYPES: tyt_e071          TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
           tyt_documentation TYPE STANDARD TABLE OF ty_documentation WITH DEFAULT KEY.

    DATA: gs_e070           TYPE e070,
          gt_e071           TYPE tyt_e071,
          go_linked_package TYPE REF TO lcl_trm_package,
          gt_docs           TYPE tyt_documentation.

    METHODS get_e070
      RETURNING VALUE(rs_e070) TYPE e070.

    METHODS get_e071
      RETURNING VALUE(rt_e071) TYPE tyt_e071.

    METHODS is_trm_relevant
      RETURNING VALUE(rv_relevant) TYPE flag.

    METHODS get_documentation
      RETURNING VALUE(rt_docs) TYPE tyt_documentation.

ENDCLASS.

CLASS lcl_trm_transport IMPLEMENTATION.

  METHOD constructor.
    me->trkorr = iv_trkorr.
    me->migration = iv_migration.
  ENDMETHOD.

  METHOD get_e071.
    IF gt_e071[] IS INITIAL.
      IF migration <> 'X'.
        SELECT pgmid object obj_name FROM e071 INTO CORRESPONDING FIELDS OF TABLE gt_e071 WHERE trkorr EQ trkorr.
      ELSE.
        SELECT pgmid object obj_name FROM ztrm_e071 INTO CORRESPONDING FIELDS OF TABLE gt_e071 WHERE trkorr EQ trkorr.
      ENDIF.
    ENDIF.
    rt_e071[] = gt_e071[].
  ENDMETHOD.

  METHOD get_e070.
    IF gs_e070 IS INITIAL.
      IF migration <> 'X'.
        SELECT SINGLE trkorr trfunction trstatus as4date as4time FROM e070 INTO CORRESPONDING FIELDS OF gs_e070 WHERE trkorr EQ trkorr.
      ELSE.
        SELECT SINGLE trkorr trfunction trstatus as4date as4time FROM ztrm_e070 INTO CORRESPONDING FIELDS OF gs_e070 WHERE trkorr EQ trkorr.
      ENDIF.
    ENDIF.
    rs_e070 = gs_e070.
  ENDMETHOD.

  METHOD is_trm_relevant.
    DATA: ls_e071        TYPE e071,
          lv_has_name    TYPE flag,
          lv_has_version TYPE flag.
    DATA(lt_e071) = get_e071( ).
    DELETE lt_e071 WHERE NOT ( pgmid EQ '*' AND object EQ 'ZTRM' ).
    LOOP AT lt_e071 INTO ls_e071.
      IF ls_e071-obj_name CP 'name=*'.
        lv_has_name = 'X'.
      ENDIF.
      IF ls_e071-obj_name CP 'version=*'.
        lv_has_version = 'X'.
      ENDIF.
    ENDLOOP.
    IF lv_has_name EQ 'X' AND lv_has_version EQ 'X'.
      rv_relevant = 'X'.
    ENDIF.
  ENDMETHOD.

  METHOD get_documentation.
    IF gt_docs[] IS INITIAL.
      TYPES: BEGIN OF ty_doktl_line,
               no    TYPE doku_line,
               value TYPE string,
             END OF ty_doktl_line,
             tyt_doktl_line TYPE STANDARD TABLE OF ty_doktl_line WITH DEFAULT KEY,
             BEGIN OF ty_doktl,
               langu     TYPE doku_langu,
               version   TYPE dokvers,
               doc_lines TYPE tyt_doktl_line,
             END OF ty_doktl.
      DATA: lt_doktl        TYPE STANDARD TABLE OF doktl,
            ls_doktl        LIKE LINE OF lt_doktl,
            lt_trkorr_doktl TYPE STANDARD TABLE OF ty_doktl,
            ls_trkorr_doktl LIKE LINE OF lt_trkorr_doktl,
            ls_dokt_line    TYPE ty_doktl_line,
            lt_lines        TYPE STANDARD TABLE OF string.
      FIELD-SYMBOLS: <fs_trkorr_doktl>      TYPE ty_doktl,
                     <fs_trkorr_doktl_line> TYPE ty_doktl_line,
                     <fs_doc>               TYPE ty_documentation.
      IF migration <> 'X'.
        SELECT langu dokversion line doktext FROM doktl INTO CORRESPONDING FIELDS OF TABLE lt_doktl WHERE id EQ 'TA' AND object EQ trkorr.
      ELSE.
        SELECT langu dokversion line doktext FROM ztrm_doktl INTO CORRESPONDING FIELDS OF TABLE lt_doktl WHERE trm_trokrr EQ trkorr.
      ENDIF.
      LOOP AT lt_doktl INTO ls_doktl.
        UNASSIGN <fs_trkorr_doktl>.
        UNASSIGN <fs_trkorr_doktl_line>.
        READ TABLE lt_trkorr_doktl ASSIGNING <fs_trkorr_doktl> WITH KEY langu = ls_doktl-langu version = ls_doktl-dokversion.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO lt_trkorr_doktl ASSIGNING <fs_trkorr_doktl>.
          <fs_trkorr_doktl>-langu = ls_doktl-langu.
          <fs_trkorr_doktl>-version = ls_doktl-dokversion.
        ENDIF.
        APPEND INITIAL LINE TO <fs_trkorr_doktl>-doc_lines ASSIGNING <fs_trkorr_doktl_line>.
        <fs_trkorr_doktl_line>-no = ls_doktl-line.
        <fs_trkorr_doktl_line>-value = ls_doktl-doktext.
        SORT <fs_trkorr_doktl>-doc_lines BY no ASCENDING.
      ENDLOOP.
      SORT lt_trkorr_doktl BY version ASCENDING.
      LOOP AT lt_trkorr_doktl INTO ls_trkorr_doktl.
        UNASSIGN <fs_doc>.
        CLEAR lt_lines.
        CLEAR ls_dokt_line.
        APPEND INITIAL LINE TO gt_docs ASSIGNING <fs_doc>.
        <fs_doc>-langu = ls_trkorr_doktl-langu.
        <fs_doc>-version = ls_trkorr_doktl-version.
        LOOP AT ls_trkorr_doktl-doc_lines INTO ls_dokt_line.
          APPEND ls_dokt_line-value TO lt_lines.
        ENDLOOP.
        CONCATENATE LINES OF lt_lines INTO <fs_doc>-value.
      ENDLOOP.
    ENDIF.
    rt_docs[] = gt_docs[].
  ENDMETHOD.

  METHOD get_linked_package.
    IF go_linked_package IS NOT BOUND.
      DATA: lt_docs     TYPE tyt_documentation,
            ls_doc      LIKE LINE OF lt_docs,
            lo_manifest TYPE REF TO lcl_trm_manifest.
      CHECK is_trm_relevant( ) EQ 'X'.
      lt_docs = get_documentation( ).
      CHECK lt_docs[] IS NOT INITIAL.
      READ TABLE lt_docs INTO ls_doc WITH KEY langu = sy-langu.
      IF sy-subrc <> 0.
        READ TABLE lt_docs INTO ls_doc INDEX 1.
      ENDIF.
      lo_manifest = lcl_trm_manifest=>from_abap_xml( ls_doc-value ).
      CHECK lo_manifest IS BOUND.
      go_linked_package = lo_manifest->get_trm_package( ).
    ENDIF.
    ro_linked_package = go_linked_package.
  ENDMETHOD.

  METHOD get_latest.
    DATA lo_transport TYPE REF TO lcl_trm_transport.
    LOOP AT it_transports INTO lo_transport.
      IF ro_transport IS NOT BOUND.
        ro_transport = lo_transport.
      ELSE.
        IF lo_transport->get_date( ) GT ro_transport->get_date( ).
          ro_transport = lo_transport.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_date.
    DATA(ls_e070) = get_e070( ).
    CONVERT DATE ls_e070-as4date TIME ls_e070-as4time INTO TIME STAMP rv_date TIME ZONE sy-zonlo.
  ENDMETHOD.

  METHOD get_devclass.

  ENDMETHOD.

ENDCLASS.

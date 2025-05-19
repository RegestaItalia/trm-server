CLASS zcl_trm_core DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_trm_transport,
             trkorr    TYPE trkorr,
             migration TYPE flag,
           END OF ty_trm_transport,
           BEGIN OF ty_trm_package,
             name      TYPE string,
             version   TYPE string,
             registry  TYPE string,
             devclass  TYPE devclass,
             manifest  TYPE zif_trm_core=>ty_manifest,
             xmanifest TYPE xstring,
             transport TYPE ty_trm_transport,
             timestamp TYPE timestamp,
           END OF ty_trm_package.
    TYPES: tyt_trkorr           TYPE STANDARD TABLE OF trkorr WITH DEFAULT KEY,
           tyt_migration_trkorr TYPE STANDARD TABLE OF ztrm_trkorr WITH DEFAULT KEY,
           tyt_trm_package      TYPE STANDARD TABLE OF ty_trm_package WITH DEFAULT KEY.

    CLASS-METHODS get_source_trkorr
      RETURNING VALUE(rt_trkorr) TYPE tyt_trkorr.
    CLASS-METHODS get_ignored_trkorr
      RETURNING VALUE(rt_trkorr) TYPE tyt_trkorr.

    CLASS-METHODS get_installed_packages
      RETURNING VALUE(rt_packages) TYPE tyt_trm_package.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_core IMPLEMENTATION.

  METHOD get_source_trkorr.
    SELECT trkorr FROM ztrm_src_trkorr INTO TABLE rt_trkorr.
  ENDMETHOD.

  METHOD get_ignored_trkorr.
    SELECT trkorr FROM ztrm_skip_trkorr INTO TABLE rt_trkorr.
  ENDMETHOD.

  METHOD get_installed_packages.
    TYPES: BEGIN OF ty_all_trkorr,
             trkorr    TYPE trkorr,
             migration TYPE flag,
           END OF ty_all_trkorr,
           tyt_transport TYPE STANDARD TABLE OF REF TO lcl_trm_transport WITH DEFAULT KEY,
           BEGIN OF ty_trkorr_package,
             trkorr           TYPE tyt_transport,
             package          TYPE REF TO lcl_trm_package,
             package_name     TYPE string,
             package_registry TYPE string,
           END OF ty_trkorr_package.
    DATA: lt_source_trkorr    TYPE tyt_trkorr,
          lt_ignored_trkorr   TYPE tyt_trkorr,
          lt_actual_trkorr    TYPE tyt_trkorr,
          lv_actual_trkorr    LIKE LINE OF lt_actual_trkorr,
          lt_migration_trkorr TYPE tyt_migration_trkorr,
          lv_migration_trkorr LIKE LINE OF lt_migration_trkorr,
          lt_trkorr           TYPE STANDARD TABLE OF ty_all_trkorr,
          lt_trkorr_copy      LIKE lt_trkorr,
          ls_trkorr           LIKE LINE OF lt_trkorr,
          lt_transport        TYPE STANDARD TABLE OF REF TO lcl_trm_transport,
          lo_transport        TYPE REF TO lcl_trm_transport,
          lt_trkorr_package   TYPE STANDARD TABLE OF ty_trkorr_package,
          ls_trkorr_package   LIKE LINE OF lt_trkorr_package,
          lo_package          TYPE REF TO lcl_trm_package,
          ls_trm_server       LIKE LINE OF rt_packages,
          ls_trm_rest         LIKE LINE OF rt_packages.
    FIELD-SYMBOLS: <fs_trkorr>           TYPE ty_all_trkorr,
                   <fs_trkorr_package>   TYPE ty_trkorr_package,
                   <fs_package>          TYPE ty_trm_package,
                   <fs_trm_rest_version> TYPE string.

    lt_source_trkorr = get_source_trkorr( ).
    lt_ignored_trkorr = get_ignored_trkorr( ).
    SELECT DISTINCT trkorr FROM e071 INTO TABLE lt_actual_trkorr WHERE pgmid EQ '*' AND object EQ 'ZTRM'.
    SELECT trm_trokrr FROM ztrm_e070 INTO TABLE lt_migration_trkorr.
    LOOP AT lt_actual_trkorr INTO lv_actual_trkorr.
      CLEAR ls_trkorr.
      ls_trkorr-trkorr = lv_actual_trkorr.
      ls_trkorr-migration = ''.
      APPEND ls_trkorr TO lt_trkorr.
    ENDLOOP.
    CLEAR ls_trkorr.
    LOOP AT lt_migration_trkorr INTO lv_migration_trkorr.
      CLEAR ls_trkorr.
      ls_trkorr-trkorr = lv_migration_trkorr.
      ls_trkorr-migration = 'X'.
      APPEND ls_trkorr TO lt_trkorr.
    ENDLOOP.
    CLEAR ls_trkorr.
    lt_trkorr_copy[] = lt_trkorr[].
    LOOP AT lt_trkorr_copy ASSIGNING <fs_trkorr>.
      READ TABLE lt_source_trkorr TRANSPORTING NO FIELDS WITH KEY table_line = <fs_trkorr>-trkorr.
      CHECK sy-subrc EQ 0.
      CLEAR <fs_trkorr>.
    ENDLOOP.
    UNASSIGN <fs_trkorr>.
    DELETE lt_trkorr_copy WHERE table_line IS INITIAL.
    IF lt_trkorr_copy[] IS NOT INITIAL.
      DATA: lt_y_migration     LIKE lt_trkorr_copy,
            lt_tms_y_migration TYPE STANDARD TABLE OF ztrm_tmsbuffer,
            ls_tms_y_migration LIKE LINE OF lt_tms_y_migration,
            lt_n_migration     LIKE lt_trkorr_copy,
            lt_tms_n_migration TYPE STANDARD TABLE OF tmsbuffer,
            ls_tms_n_migration LIKE LINE OF lt_tms_n_migration,
            lv_maxrc           TYPE i.
      LOOP AT lt_trkorr_copy INTO ls_trkorr WHERE migration = 'X'.
        APPEND ls_trkorr TO lt_y_migration.
      ENDLOOP.
      CLEAR ls_trkorr.
      LOOP AT lt_trkorr_copy INTO ls_trkorr WHERE migration <> 'X'.
        APPEND ls_trkorr TO lt_n_migration.
      ENDLOOP.
      CLEAR ls_trkorr.
      IF lt_y_migration[] IS NOT INITIAL.
        SELECT trkorr maxrc FROM ztrm_tmsbuffer
        INTO CORRESPONDING FIELDS OF TABLE lt_tms_y_migration
        FOR ALL ENTRIES IN lt_y_migration
        WHERE sysnam EQ sy-sysid AND trkorr EQ lt_y_migration-trkorr AND impsing <> 'X'.
      ENDIF.
      IF lt_n_migration[] IS NOT INITIAL.
        SELECT trkorr maxrc FROM tmsbuffer
        INTO CORRESPONDING FIELDS OF TABLE lt_tms_n_migration
        FOR ALL ENTRIES IN lt_n_migration
        WHERE sysnam EQ sy-sysid AND trkorr EQ lt_n_migration-trkorr AND impsing <> 'X'.
      ENDIF.
      LOOP AT lt_trkorr_copy INTO ls_trkorr.
        IF ls_trkorr-migration EQ 'X'.
          LOOP AT lt_tms_y_migration INTO ls_tms_y_migration WHERE trkorr EQ ls_trkorr-trkorr.
            CLEAR lv_maxrc.
            lv_maxrc = ls_tms_y_migration-maxrc.
            IF lv_maxrc LT 0.
              APPEND ls_trkorr TO lt_ignored_trkorr.
            ENDIF.
          ENDLOOP.
          IF sy-subrc <> 0.
            APPEND ls_trkorr TO lt_ignored_trkorr.
          ENDIF.
          CLEAR ls_tms_y_migration.
        ELSE.
          LOOP AT lt_tms_n_migration INTO ls_tms_n_migration WHERE trkorr EQ ls_trkorr-trkorr.
            CLEAR lv_maxrc.
            lv_maxrc = ls_tms_n_migration-maxrc.
            IF lv_maxrc LT 0.
              APPEND ls_trkorr TO lt_ignored_trkorr.
            ENDIF.
          ENDLOOP.
          IF sy-subrc <> 0.
            APPEND ls_trkorr TO lt_ignored_trkorr.
          ENDIF.
          CLEAR ls_tms_n_migration.
        ENDIF.
      ENDLOOP.
      CLEAR ls_trkorr.
    ENDIF.
    SORT lt_ignored_trkorr.
    DELETE ADJACENT DUPLICATES FROM lt_ignored_trkorr.
    LOOP AT lt_trkorr ASSIGNING <fs_trkorr>.
      READ TABLE lt_ignored_trkorr TRANSPORTING NO FIELDS WITH KEY table_line = <fs_trkorr>-trkorr.
      CHECK sy-subrc EQ 0.
      CLEAR <fs_trkorr>.
    ENDLOOP.
    UNASSIGN <fs_trkorr>.
    DELETE lt_trkorr WHERE table_line IS INITIAL.
    LOOP AT lt_trkorr INTO ls_trkorr.
      CLEAR lo_transport.
      UNASSIGN <fs_trkorr_package>.
      CREATE OBJECT lo_transport EXPORTING iv_trkorr = ls_trkorr-trkorr iv_migration = ls_trkorr-migration.
      IF lo_transport->get_linked_package( ) IS BOUND.
        READ TABLE lt_trkorr_package ASSIGNING <fs_trkorr_package> WITH KEY package_name = lo_transport->get_linked_package( )->name
                                                                            package_registry = lo_transport->get_linked_package( )->registry.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO lt_trkorr_package ASSIGNING <fs_trkorr_package>.
          <fs_trkorr_package>-package = lo_transport->get_linked_package( ).
          <fs_trkorr_package>-package_name = lo_transport->get_linked_package( )->name.
          <fs_trkorr_package>-package_registry = lo_transport->get_linked_package( )->registry.
        ENDIF.
        APPEND lo_transport TO <fs_trkorr_package>-trkorr.
      ENDIF.
    ENDLOOP.
    LOOP AT lt_trkorr_package INTO ls_trkorr_package.
      CLEAR lo_transport.
      CLEAR lo_package.
      UNASSIGN <fs_package>.
      lo_transport = lcl_trm_transport=>get_latest( ls_trkorr_package-trkorr ).
      lo_package = lo_transport->get_linked_package( ).
      APPEND INITIAL LINE TO rt_packages ASSIGNING <fs_package>.
      <fs_package>-transport-trkorr = lo_transport->trkorr.
      <fs_package>-transport-migration = lo_transport->migration.
      <fs_package>-xmanifest = lo_package->xmanifest.
      <fs_package>-manifest = lo_package->manifest.
      <fs_package>-name = <fs_package>-manifest-name.
      <fs_package>-version = <fs_package>-manifest-version.
      <fs_package>-registry = <fs_package>-manifest-registry.
      <fs_package>-timestamp = lo_transport->get_date( ).
    ENDLOOP.
    SORT rt_packages BY timestamp DESCENDING.

    "add trm-server and trm-rest (in installed)
    LOOP AT rt_packages INTO ls_trm_server WHERE name = 'trm-server' AND registry IS INITIAL.
      CONTINUE.
    ENDLOOP.
    IF sy-subrc EQ 0.
      DELETE rt_packages INDEX sy-tabix.
      IF ls_trm_server-version <> zif_trm=>version.
        CLEAR ls_trm_server-timestamp.
        CLEAR ls_trm_server-transport.
        CLEAR ls_trm_server-manifest.
        CLEAR ls_trm_server-xmanifest.
      ENDIF.
    ENDIF.
    ls_trm_server-name = 'trm-server'.
    ls_trm_server-version = zif_trm=>version.
    ls_trm_server-manifest-name = ls_trm_server-name.
    ls_trm_server-manifest-version = ls_trm_server-version.
    IF ls_trm_server-xmanifest IS INITIAL.
      CALL TRANSFORMATION id
      SOURCE trm_manifest = ls_trm_server-manifest
      RESULT XML ls_trm_server-xmanifest.
    ENDIF.
    INSERT ls_trm_server INTO rt_packages INDEX 1.

    ASSIGN ('ZIF_TRM_REST')=>('VERSION') TO <fs_trm_rest_version>.
    IF sy-subrc EQ 0.
      LOOP AT rt_packages INTO ls_trm_rest WHERE name = 'trm-rest' AND registry IS INITIAL.
        CONTINUE.
      ENDLOOP.
      IF sy-subrc EQ 0.
        DELETE rt_packages INDEX sy-tabix.
        IF ls_trm_rest-version <> <fs_trm_rest_version>.
          CLEAR ls_trm_rest-timestamp.
          CLEAR ls_trm_rest-transport.
          CLEAR ls_trm_rest-manifest.
          CLEAR ls_trm_rest-xmanifest.
          ls_trm_rest-version = <fs_trm_rest_version>.
        ENDIF.
      ENDIF.
      ls_trm_rest-name = 'trm-rest'.
      ls_trm_rest-version = <fs_trm_rest_version>.
      ls_trm_rest-manifest-name = ls_trm_rest-name.
      ls_trm_rest-manifest-version = ls_trm_rest-version.
      IF ls_trm_rest-xmanifest IS INITIAL.
        CALL TRANSFORMATION id
        SOURCE trm_manifest = ls_trm_rest-manifest
        RESULT XML ls_trm_rest-xmanifest.
      ENDIF.
      INSERT ls_trm_rest INTO rt_packages INDEX 2.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

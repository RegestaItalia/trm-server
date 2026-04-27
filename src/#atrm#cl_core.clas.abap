CLASS /atrm/cl_core DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: tyt_tdevc TYPE STANDARD TABLE OF tdevc WITH DEFAULT KEY,
           BEGIN OF ty_trm_transport,
             trkorr    TYPE trkorr,
             migration TYPE flag,
           END OF ty_trm_transport,
           BEGIN OF ty_trm_package_legacy,
             name      TYPE /atrm/package_name,
             version   TYPE string,
             registry  TYPE /atrm/package_registry,
             tdevc     TYPE tyt_tdevc,
             manifest  TYPE /atrm/if_core=>ty_manifest,
             xmanifest TYPE xstring,
             transport TYPE ty_trm_transport,
             trkorr    TYPE trkorr,
             timestamp TYPE timestamp,
           END OF ty_trm_package_legacy.
    TYPES: tyt_trkorr             TYPE STANDARD TABLE OF trkorr WITH DEFAULT KEY,
           tyt_migration_trkorr   TYPE STANDARD TABLE OF /atrm/trkorr WITH DEFAULT KEY,
           tyt_trm_package_legacy TYPE STANDARD TABLE OF ty_trm_package_legacy WITH DEFAULT KEY.

    CLASS-METHODS get_installed_packages_legacy
      RETURNING VALUE(packages) TYPE tyt_trm_package_legacy.
    CLASS-METHODS get_installed_packages
      IMPORTING package_name     TYPE /atrm/package_name OPTIONAL
                package_registry TYPE /atrm/package_registry OPTIONAL
      RETURNING VALUE(packages)  TYPE /atrm/packages_t.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_core IMPLEMENTATION.

  METHOD get_installed_packages_legacy.
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
           END OF ty_trkorr_package,
           BEGIN OF ty_wb_trkorr_package,
             obj_name  TYPE trobj_name, "name=*
             trkorr    TYPE trkorr,
             transport TYPE REF TO lcl_trm_transport,
             package   TYPE REF TO lcl_trm_package,
           END OF ty_wb_trkorr_package,
           BEGIN OF ty_package_wb_transport,
             package_name     TYPE string,
             package_registry TYPE string,
             transports       TYPE tyt_transport,
           END OF ty_package_wb_transport.
    DATA: lt_source_trkorr         TYPE tyt_trkorr,
          lt_ignored_trkorr        TYPE tyt_trkorr,
          lt_actual_trkorr         TYPE tyt_trkorr,
          lv_actual_trkorr         LIKE LINE OF lt_actual_trkorr,
          lt_migration_trkorr      TYPE tyt_migration_trkorr,
          lv_migration_trkorr      LIKE LINE OF lt_migration_trkorr,
          lt_trkorr                TYPE STANDARD TABLE OF ty_all_trkorr,
          lt_trkorr_copy           LIKE lt_trkorr,
          ls_trkorr                LIKE LINE OF lt_trkorr,
          lt_transport             TYPE STANDARD TABLE OF REF TO lcl_trm_transport,
          lo_transport             TYPE REF TO lcl_trm_transport,
          lt_trkorr_package        TYPE STANDARD TABLE OF ty_trkorr_package,
          ls_trkorr_package        LIKE LINE OF lt_trkorr_package,
          lo_package               TYPE REF TO lcl_trm_package,
          lv_tabix                 TYPE syst_tabix,
          lo_devclass              TYPE REF TO /atrm/cl_package,
          lt_subpackages           TYPE cl_pak_package_queries=>tt_subpackage_info,
          ls_subpackage            LIKE LINE OF lt_subpackages,
          lt_devclass              TYPE STANDARD TABLE OF devclass,
          ls_trm_server            LIKE LINE OF packages,
          ls_trm_rest              LIKE LINE OF packages,
          lv_devclass              TYPE devclass,
          ls_trm_package           TYPE ty_trm_package_legacy,
          lt_wb_trkorr_name        TYPE STANDARD TABLE OF trobj_name,
          lt_wb_trkorr_package     TYPE STANDARD TABLE OF ty_wb_trkorr_package,
          lt_package_wb_transports TYPE STANDARD TABLE OF ty_package_wb_transport,
          ls_package_wb_transports LIKE LINE OF lt_package_wb_transports,
          lo_wb_transport          TYPE REF TO lcl_trm_transport.
    FIELD-SYMBOLS: <fs_trkorr>               TYPE ty_all_trkorr,
                   <fs_trkorr_package>       TYPE ty_trkorr_package,
                   <fs_package>              TYPE ty_trm_package_legacy,
                   <fs_trm_rest_version>     TYPE string,
                   <fs_dependency>           TYPE /atrm/if_core=>ty_dependency,
                   <fs_wb_trkorr_name>       TYPE trobj_name,
                   <fs_wb_trkorr_package>    TYPE ty_wb_trkorr_package,
                   <fs_package_wb_transport> TYPE ty_package_wb_transport.

    SELECT DISTINCT trkorr FROM e071 INTO TABLE lt_actual_trkorr WHERE pgmid EQ '*' AND object EQ 'ZTRM'.
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
      IF lt_n_migration[] IS NOT INITIAL.
        SELECT trkorr maxrc FROM tmsbuffer
        INTO CORRESPONDING FIELDS OF TABLE lt_tms_n_migration
        FOR ALL ENTRIES IN lt_n_migration
        WHERE sysnam EQ sy-sysid AND trkorr EQ lt_n_migration-trkorr AND impsing <> 'X'.
      ENDIF.
      LOOP AT lt_trkorr_copy INTO ls_trkorr.
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
      APPEND INITIAL LINE TO packages ASSIGNING <fs_package>.
      <fs_package>-transport-trkorr = lo_transport->trkorr.
      <fs_package>-transport-migration = lo_transport->migration.
      <fs_package>-xmanifest = lo_package->xmanifest.
      <fs_package>-manifest = lo_package->manifest.
      <fs_package>-name = <fs_package>-manifest-name.
      <fs_package>-version = <fs_package>-manifest-version.
      <fs_package>-registry = <fs_package>-manifest-registry.
      <fs_package>-timestamp = lo_transport->get_date( ).
      <fs_package>-tdevc = lo_transport->get_tdevc( ).
    ENDLOOP.
    SORT packages BY timestamp DESCENDING.

    "add trm-server and trm-rest (in installed)
    CLEAR lv_tabix.
    LOOP AT packages INTO ls_trm_server WHERE name = 'trm-server' AND registry IS INITIAL.
      lv_tabix = sy-tabix.
    ENDLOOP.
    IF sy-subrc EQ 0.
      DELETE packages INDEX lv_tabix.
      IF ls_trm_server-version <> /atrm/if_server=>version.
        CLEAR ls_trm_server-timestamp.
        CLEAR ls_trm_server-transport.
        CLEAR ls_trm_server-manifest.
        CLEAR ls_trm_server-xmanifest.
        CLEAR ls_trm_server-tdevc.
      ENDIF.
    ENDIF.
    CLEAR lv_devclass.
    ls_trm_server-name = 'trm-server'.
    ls_trm_server-version = /atrm/if_server=>version.
    ls_trm_server-manifest-name = ls_trm_server-name.
    ls_trm_server-manifest-version = ls_trm_server-version.
    IF ls_trm_server-xmanifest IS INITIAL.
      CALL TRANSFORMATION id
      SOURCE trm_manifest = ls_trm_server-manifest
      RESULT XML ls_trm_server-xmanifest.
    ENDIF.
    SELECT SINGLE devclass FROM tadir INTO lv_devclass WHERE pgmid EQ 'R3TR' AND object EQ 'INTF' AND obj_name EQ '/ATRM/IF'.
    IF lv_devclass IS NOT INITIAL.
      CLEAR lo_devclass.
      CLEAR lt_devclass[].
      CLEAR lt_subpackages[].
      CLEAR ls_subpackage.
      CREATE OBJECT lo_devclass EXPORTING devclass = lv_devclass.
      lt_subpackages = lo_devclass->get_subpackages( ).
      LOOP AT lt_subpackages INTO ls_subpackage.
        APPEND ls_subpackage-package TO lt_devclass.
      ENDLOOP.
      APPEND lv_devclass TO lt_devclass.
      SELECT * FROM tdevc INTO CORRESPONDING FIELDS OF TABLE ls_trm_server-tdevc FOR ALL ENTRIES IN lt_devclass WHERE devclass EQ lt_devclass-table_line.
    ENDIF.
    INSERT ls_trm_server INTO packages INDEX 1.

    ASSIGN ('/ATRM/IF_REST')=>('VERSION') TO <fs_trm_rest_version>.
    IF sy-subrc EQ 0.
      CLEAR lv_tabix.
      LOOP AT packages INTO ls_trm_rest WHERE name = 'trm-rest' AND registry IS INITIAL.
        lv_tabix = sy-tabix.
      ENDLOOP.
      IF sy-subrc EQ 0.
        DELETE packages INDEX lv_tabix.
        IF ls_trm_rest-version <> <fs_trm_rest_version>.
          CLEAR ls_trm_rest-timestamp.
          CLEAR ls_trm_rest-transport.
          CLEAR ls_trm_rest-manifest.
          CLEAR ls_trm_rest-xmanifest.
          CLEAR ls_trm_rest-tdevc.
        ENDIF.
      ENDIF.
      CLEAR lv_devclass.
      ls_trm_rest-name = 'trm-rest'.
      ls_trm_rest-version = <fs_trm_rest_version>.
      ls_trm_rest-manifest-name = ls_trm_rest-name.
      IF ls_trm_rest-manifest-dependencies[] IS INITIAL.
        UNASSIGN <fs_dependency>.
        APPEND INITIAL LINE TO ls_trm_rest-manifest-dependencies ASSIGNING <fs_dependency>.
        <fs_dependency>-name = 'trm-server'.
        CONCATENATE '^' /atrm/if_server=>version INTO <fs_dependency>-version.
      ENDIF.
      ls_trm_rest-manifest-version = ls_trm_rest-version.
      IF ls_trm_rest-xmanifest IS INITIAL.
        CALL TRANSFORMATION id
        SOURCE trm_manifest = ls_trm_rest-manifest
        RESULT XML ls_trm_rest-xmanifest.
      ENDIF.
      SELECT SINGLE devclass FROM tadir INTO lv_devclass WHERE pgmid EQ 'R3TR' AND object EQ 'INTF' AND obj_name EQ '/ATRM/IF_REST'.
      IF lv_devclass IS NOT INITIAL.
        CLEAR lo_devclass.
        CLEAR lt_devclass[].
        CLEAR lt_subpackages[].
        CLEAR ls_subpackage.
        CREATE OBJECT lo_devclass EXPORTING devclass = lv_devclass.
        lt_subpackages = lo_devclass->get_subpackages( ).
        LOOP AT lt_subpackages INTO ls_subpackage.
          APPEND ls_subpackage-package TO lt_devclass.
        ENDLOOP.
        APPEND lv_devclass TO lt_devclass.
        SELECT * FROM tdevc INTO CORRESPONDING FIELDS OF TABLE ls_trm_rest-tdevc FOR ALL ENTRIES IN lt_devclass WHERE devclass EQ lt_devclass-table_line.
      ENDIF.
      INSERT ls_trm_rest INTO packages INDEX 2.
    ENDIF.

    "set workbench transports where possible
    LOOP AT packages INTO ls_trm_package.
      UNASSIGN <fs_wb_trkorr_name>.
      APPEND INITIAL LINE TO lt_wb_trkorr_name ASSIGNING <fs_wb_trkorr_name>.
      CONCATENATE 'name=' ls_trm_package-name INTO <fs_wb_trkorr_name>.
    ENDLOOP.
    IF lt_wb_trkorr_name[] IS NOT INITIAL.
      SELECT DISTINCT e070~trkorr e071~obj_name FROM e071
        INNER JOIN e070 ON e071~trkorr = e070~trkorr
        INTO CORRESPONDING FIELDS OF TABLE lt_wb_trkorr_package
        FOR ALL ENTRIES IN lt_wb_trkorr_name
        WHERE e070~trstatus EQ 'D' AND e071~pgmid EQ '*' AND e071~object EQ 'ZTRM' AND e071~obj_name EQ lt_wb_trkorr_name-table_line.
      "for each transport read its manifest
      LOOP AT lt_wb_trkorr_package ASSIGNING <fs_wb_trkorr_package>.
        UNASSIGN <fs_package_wb_transport>.
        CREATE OBJECT <fs_wb_trkorr_package>-transport EXPORTING iv_trkorr = <fs_wb_trkorr_package>-trkorr iv_migration = ''.
        <fs_wb_trkorr_package>-package = <fs_wb_trkorr_package>-transport->get_linked_package( ).
        CHECK <fs_wb_trkorr_package>-package IS BOUND.
        READ TABLE lt_package_wb_transports ASSIGNING <fs_package_wb_transport> WITH KEY package_name = <fs_wb_trkorr_package>-package->name package_registry = <fs_wb_trkorr_package>-package->registry.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO lt_package_wb_transports ASSIGNING <fs_package_wb_transport>.
          <fs_package_wb_transport>-package_name = <fs_wb_trkorr_package>-package->name.
          <fs_package_wb_transport>-package_registry = <fs_wb_trkorr_package>-package->registry.
        ENDIF.
        APPEND <fs_wb_trkorr_package>-transport TO <fs_package_wb_transport>-transports.
      ENDLOOP.

      UNASSIGN <fs_package>.
      LOOP AT packages ASSIGNING <fs_package>.
        CLEAR ls_package_wb_transports.
        CLEAR lo_wb_transport.
        READ TABLE lt_package_wb_transports INTO ls_package_wb_transports WITH KEY package_name = <fs_package>-name package_registry = <fs_package>-registry.
        IF ls_package_wb_transports-transports[] IS NOT INITIAL.
          lo_wb_transport = lcl_trm_transport=>get_latest( ls_package_wb_transports-transports[] ).
          CHECK lo_wb_transport IS BOUND.
          <fs_package>-trkorr = lo_wb_transport->trkorr.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_installed_packages.
    DATA: e071_to_tadir  TYPE STANDARD TABLE OF tadir,
          aux_e071       TYPE e071,
          package        TYPE REF TO /atrm/cl_package,
          dummy_manifest TYPE /atrm/if_core=>ty_manifest.
    FIELD-SYMBOLS: <row>          TYPE /atrm/package,
                   <aux_tadir>    TYPE tadir,
                   <rest_version> TYPE string.
    SELECT /atrm/packages~package_name
           /atrm/packages~package_registry
           /atrm/packages~manifest
           /atrm/packages~trkorr
           /atrm/packages~integrity
           /atrm/packages~devclass
           e070~as4date
           e070~as4time
      FROM /atrm/packages
      INNER JOIN tdevc ON tdevc~devclass = /atrm/packages~devclass
      INNER JOIN e070 ON e070~trkorr = /atrm/packages~trkorr
      INTO CORRESPONDING FIELDS OF TABLE packages.
    IF package_name IS NOT INITIAL.
      DELETE packages WHERE package_name <> package_name AND package_registry <> package_registry.
    ENDIF.
    LOOP AT packages ASSIGNING <row>.
      CLEAR package.
      CREATE OBJECT package EXPORTING devclass = <row>-devclass.
      <row>-packages = package->get_all_packages( ).
      TRY.
          <row>-dirty = package->get_dirty_entries(
            from_date = <row>-as4date
            from_time = <row>-as4time
          ).
        CATCH /atrm/cx_exception.
      ENDTRY.
    ENDLOOP.
    " add trm-server (and trm-rest eventually) if installed via abapgit
    READ TABLE packages TRANSPORTING NO FIELDS WITH KEY package_name = 'trm-server' package_registry = 'public'.
    IF sy-subrc <> 0 AND ( package_name IS INITIAL OR ( package_name EQ 'trm-server' AND package_registry EQ 'public' ) ).
      UNASSIGN <row>.
      CLEAR dummy_manifest.
      APPEND INITIAL LINE TO packages ASSIGNING <row>.
      <row>-package_name = 'trm-server'.
      <row>-package_registry = 'public'.
      <row>-as4date = '10000101'.
      <row>-as4time = '000000'.
      SELECT SINGLE devclass FROM tadir INTO <row>-devclass WHERE pgmid EQ 'R3TR' AND object EQ 'INTF' AND obj_name EQ '/ATRM/IF_SERVER'.
      IF <row>-devclass IS NOT INITIAL.
        CLEAR package.
        CREATE OBJECT package EXPORTING devclass = <row>-devclass.
        <row>-packages = package->get_all_packages( ).
      ENDIF.
      dummy_manifest-name = <row>-package_name.
      dummy_manifest-version = /atrm/if_server=>version.
      CALL TRANSFORMATION id
       SOURCE trm_manifest = dummy_manifest
       RESULT XML <row>-manifest.
    ENDIF.
    READ TABLE packages TRANSPORTING NO FIELDS WITH KEY package_name = 'trm-rest' package_registry = 'public'.
    IF sy-subrc <> 0 AND ( package_name IS INITIAL OR ( package_name EQ 'trm-rest' AND package_registry EQ 'public' ) ).
      ASSIGN ('/ATRM/IF_REST')=>('VERSION') TO <rest_version>.
      IF sy-subrc EQ 0.
        UNASSIGN <row>.
        CLEAR dummy_manifest.
        APPEND INITIAL LINE TO packages ASSIGNING <row>.
        <row>-package_name = 'trm-rest'.
        <row>-package_registry = 'public'.
        <row>-as4date = '10000101'.
        <row>-as4time = '000001'.
        SELECT SINGLE devclass FROM tadir INTO <row>-devclass WHERE pgmid EQ 'R3TR' AND object EQ 'INTF' AND obj_name EQ '/ATRM/IF_REST'.
        IF <row>-devclass IS NOT INITIAL.
          CLEAR package.
          CREATE OBJECT package EXPORTING devclass = <row>-devclass.
          <row>-packages = package->get_all_packages( ).
        ENDIF.
        dummy_manifest-name = <row>-package_name.
        dummy_manifest-version = <rest_version>.
        CALL TRANSFORMATION id
         SOURCE trm_manifest = dummy_manifest
         RESULT XML <row>-manifest.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

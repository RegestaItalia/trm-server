CLASS /atrm/cl_object DEFINITION
  PUBLIC
  CREATE PUBLIC
  GLOBAL FRIENDS /atrm/cl_senvi_map.

  PUBLIC SECTION.
    INTERFACES /atrm/if_object.

    TYPES: tyt_senvi TYPE STANDARD TABLE OF senvi.

    DATA: key   TYPE /atrm/object READ-ONLY,
          senvi TYPE tyt_senvi READ-ONLY.

    METHODS constructor
      IMPORTING key TYPE /atrm/object.

  PROTECTED SECTION.
    CLASS-METHODS get_tadir_dependency
      IMPORTING object            TYPE any
                obj_name          TYPE any
      RETURNING VALUE(dependency) TYPE /atrm/object_dependency
      RAISING   /atrm/cx_exception.
    CLASS-METHODS get_tfdir_dependency
      IMPORTING funcname          TYPE any
      RETURNING VALUE(dependency) TYPE /atrm/object_dependency
      RAISING   /atrm/cx_exception.
CLASS-METHODS get_cds_dependency
      IMPORTING entity     TYPE any
      EXPORTING dependency TYPE /atrm/object_dependency
      RAISING   /atrm/cx_exception.
CLASS-METHODS append_table_dependencies
      IMPORTING table_name    TYPE tabname
                where_clause  TYPE string
                object_field  TYPE fieldname
                object_type   TYPE trobjtype
      CHANGING  dependencies  TYPE /atrm/object_dependency_t.
CLASS-METHODS append_typed_dependencies
      IMPORTING table_name        TYPE tabname
                where_clause      TYPE string
                object_field      TYPE fieldname
                object_type_field TYPE fieldname
      CHANGING  dependencies      TYPE /atrm/object_dependency_t.
CLASS-METHODS get_entity_dependency
      IMPORTING entity     TYPE any
      EXPORTING dependency TYPE /atrm/object_dependency
      RAISING   /atrm/cx_exception.
CLASS-METHODS append_senvi_table_deps
      IMPORTING table_name   TYPE tabname
                where_clause TYPE string
                type_field   TYPE fieldname
                object_field TYPE fieldname
                origin       TYPE REF TO /atrm/if_object
      CHANGING  dependencies TYPE /atrm/object_dependency_t.
CLASS-METHODS append_composite_deps
      IMPORTING table_name    TYPE tabname
                where_clause  TYPE string
                first_field   TYPE fieldname
                second_field  TYPE fieldname
                second_offset TYPE i
                object_type   TYPE trobjtype
      CHANGING  dependencies  TYPE /atrm/object_dependency_t.
CLASS-METHODS append_lrep_dependencies
      IMPORTING object_type  TYPE trobjtype
                object_name  TYPE sobj_name
      CHANGING  dependencies TYPE /atrm/object_dependency_t.
CLASS-METHODS append_sdok_class_deps
      IMPORTING table_name   TYPE tabname
                where_clause TYPE string
                class_field  TYPE fieldname
      CHANGING  dependencies TYPE /atrm/object_dependency_t.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object IMPLEMENTATION.


  METHOD constructor.
    me->key = key.
  ENDMETHOD.


  METHOD get_tadir_dependency.
    DATA: lv_tabname                 TYPE tabname,
          lv_tabkey                  TYPE string,
          ls_dispatcher_dependencies TYPE /atrm/object_dependencies,
          lt_trm_packages            TYPE /atrm/packages_t,
          lv_devclass                TYPE devclass,
          ls_trm_package             LIKE LINE OF lt_trm_packages.

    lv_tabname = 'TADIR'.
    CONCATENATE 'R3TR' object obj_name INTO lv_tabkey.

    LOOP AT /atrm/cl_object_dispacher=>dependencies INTO ls_dispatcher_dependencies.
      READ TABLE ls_dispatcher_dependencies-dependencies INTO dependency WITH KEY tabname = lv_tabname tabkey = lv_tabkey.
      CHECK sy-subrc EQ 0.
      RETURN.
    ENDLOOP.

    lt_trm_packages = /atrm/cl_singleton=>get( )->get_installed_packages( ).
    SELECT SINGLE devclass FROM tadir INTO lv_devclass WHERE pgmid = 'R3TR' AND object = object AND obj_name = obj_name.
    IF sy-subrc EQ 0.
      dependency-tabname = lv_tabname.
      dependency-tabkey = lv_tabkey.
      dependency-devclass = lv_devclass.
      CHECK lv_devclass IS NOT INITIAL.
      LOOP AT lt_trm_packages INTO ls_trm_package.
        READ TABLE ls_trm_package-packages TRANSPORTING NO FIELDS WITH KEY table_line = lv_devclass.
        CHECK sy-subrc EQ 0.
        dependency-trm_package_name = ls_trm_package-package_name.
        dependency-trm_package_registry = ls_trm_package-package_registry.
        EXIT.
      ENDLOOP.
    ELSE.
      /atrm/cx_exception=>raise(
        iv_reason  = /atrm/cx_exception=>c_reason-generic
        iv_message = 'R3TR ' && object && ' ' && obj_name && ' is not in TADIR'
      ).
    ENDIF.
  ENDMETHOD.


  METHOD get_tfdir_dependency.
    DATA: lv_tabname                 TYPE tabname,
          lv_tabkey                  TYPE string,
          ls_dispatcher_dependencies TYPE /atrm/object_dependencies,
          lt_trm_packages            TYPE /atrm/packages_t,
          lv_devclass                TYPE devclass,
          ls_trm_package             LIKE LINE OF lt_trm_packages.

    lv_tabname = 'TFDIR'.
    lv_tabkey = funcname.

    LOOP AT /atrm/cl_object_dispacher=>dependencies INTO ls_dispatcher_dependencies.
      READ TABLE ls_dispatcher_dependencies-dependencies INTO dependency WITH KEY tabname = lv_tabname tabkey = lv_tabkey.
      CHECK sy-subrc EQ 0.
      RETURN.
    ENDLOOP.

    lt_trm_packages = /atrm/cl_singleton=>get( )->get_installed_packages( ).
    SELECT SINGLE tadir~devclass
      FROM tadir
      INNER JOIN v_fdir ON tadir~obj_name = v_fdir~area
      INTO lv_devclass WHERE tadir~pgmid = 'R3TR' AND tadir~object = 'FUGR' AND v_fdir~funcname = funcname.
    IF sy-subrc EQ 0.
      dependency-tabname = lv_tabname.
      dependency-tabkey = lv_tabkey.
      dependency-devclass = lv_devclass.
      CHECK lv_devclass IS NOT INITIAL.
      LOOP AT lt_trm_packages INTO ls_trm_package.
        READ TABLE ls_trm_package-packages TRANSPORTING NO FIELDS WITH KEY table_line = lv_devclass.
        CHECK sy-subrc EQ 0.
        dependency-trm_package_name = ls_trm_package-package_name.
        dependency-trm_package_registry = ls_trm_package-package_registry.
        EXIT.
      ENDLOOP.
    ELSE.
      /atrm/cx_exception=>raise(
        iv_reason  = /atrm/cx_exception=>c_reason-generic
        iv_message = 'Cannot find function group of ' && funcname
      ).
    ENDIF.
  ENDMETHOD.


  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_obj_type TYPE seu_obj,
          lv_obj_name TYPE sobj_name,
          ls_senvi    TYPE senvi,
          lo_map      TYPE REF TO /atrm/cl_senvi_map.
    lv_obj_type = key-object.
    lv_obj_name = key-obj_name.
    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_ALL'
      EXPORTING
        obj_type        = lv_obj_type
        object_name     = lv_obj_name
        deep            = '1'
      TABLES
        environment_tab = senvi.

    LOOP AT senvi INTO ls_senvi.
      CLEAR lo_map.
      /atrm/cl_senvi_map=>get(
        EXPORTING
          senvi  = ls_senvi
          origin = me
        RECEIVING
          map    = lo_map
      ).
      CHECK lo_map IS BOUND.
      lo_map->map_dependencies(
        CHANGING
          deps = dependencies
      ).
    ENDLOOP.
  ENDMETHOD.
  METHOD get_cds_dependency.
    DATA:
      lr_row      TYPE REF TO data,
      lv_table    TYPE tabname,
      lv_name     TYPE sobj_name,
      lv_where    TYPE string.

    FIELD-SYMBOLS:
      <ls_row>    TYPE any,
      <lv_value>  TYPE any.

    lv_name = entity.

    TRY.
        CALL METHOD get_tadir_dependency
          EXPORTING object = 'DDLS' obj_name = lv_name
          RECEIVING dependency = dependency.
        RETURN.
      CATCH cx_root.
        " the entity name can differ from the DDLS source name
    ENDTRY.

    TRY.
        lv_table = 'DDLDEPENDENCY'.
        CREATE DATA lr_row TYPE (lv_table).
        ASSIGN lr_row->* TO <ls_row>.
        REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
        CONCATENATE 'OBJECTNAME = ''' lv_name '''' INTO lv_where.
        SELECT SINGLE * FROM (lv_table) INTO <ls_row> WHERE (lv_where).
        CHECK sy-subrc = 0.

        ASSIGN COMPONENT 'DDLNAME' OF STRUCTURE <ls_row> TO <lv_value>.
        CHECK sy-subrc = 0.
        CHECK <lv_value> IS NOT INITIAL.
        CALL METHOD get_tadir_dependency
          EXPORTING object = 'DDLS' obj_name = <lv_value>
          RECEIVING dependency = dependency.
      CATCH cx_root.
        " optional CDS dependency API may not exist in the target system
    ENDTRY.
  ENDMETHOD.
  METHOD append_table_dependencies.
    DATA:
      lr_table      TYPE REF TO data,
      ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_rows>      TYPE STANDARD TABLE,
      <ls_row>       TYPE any,
      <lv_value>     TYPE any.

    TRY.
        CREATE DATA lr_table TYPE STANDARD TABLE OF (table_name).
        ASSIGN lr_table->* TO <lt_rows>.
        SELECT * FROM (table_name)
          INTO TABLE <lt_rows>
          WHERE (where_clause).

        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT object_field OF STRUCTURE <ls_row> TO <lv_value>.
          CHECK sy-subrc = 0.
          CHECK <lv_value> IS NOT INITIAL.

          TRY.
              CLEAR ls_dependency.
              CASE object_type.
                WHEN 'CDS'.
                  CALL METHOD get_cds_dependency
                    EXPORTING entity = <lv_value>
                    IMPORTING dependency = ls_dependency.
                WHEN 'ENTY'.
                  CALL METHOD get_entity_dependency
                    EXPORTING entity = <lv_value>
                    IMPORTING dependency = ls_dependency.
                WHEN 'FUNC'.
                  CALL METHOD get_tfdir_dependency
                    EXPORTING funcname = <lv_value>
                    RECEIVING dependency = ls_dependency.
                WHEN OTHERS.
                  CALL METHOD get_tadir_dependency
                    EXPORTING object = object_type obj_name = <lv_value>
                    RECEIVING dependency = ls_dependency.
              ENDCASE.
              IF ls_dependency IS NOT INITIAL.
                APPEND ls_dependency TO dependencies.
              ENDIF.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " optional repository table may not exist in the target system
    ENDTRY.
  ENDMETHOD.
  METHOD append_typed_dependencies.
    DATA:
      lr_table      TYPE REF TO data,
      ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_rows>      TYPE STANDARD TABLE,
      <ls_row>       TYPE any,
      <lv_type>      TYPE any,
      <lv_object>    TYPE any.

    TRY.
        CREATE DATA lr_table TYPE STANDARD TABLE OF (table_name).
        ASSIGN lr_table->* TO <lt_rows>.
        SELECT * FROM (table_name)
          INTO TABLE <lt_rows>
          WHERE (where_clause).

        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT object_type_field OF STRUCTURE <ls_row>
            TO <lv_type>.
          CHECK sy-subrc = 0.
          CHECK <lv_type> IS NOT INITIAL.
          ASSIGN COMPONENT object_field OF STRUCTURE <ls_row>
            TO <lv_object>.
          CHECK sy-subrc = 0.
          CHECK <lv_object> IS NOT INITIAL.

          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = <lv_type> obj_name = <lv_object>
                RECEIVING dependency = ls_dependency.
              IF ls_dependency IS NOT INITIAL.
                APPEND ls_dependency TO dependencies.
              ENDIF.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " optional repository table may not exist in the target system
    ENDTRY.
  ENDMETHOD.
  METHOD get_entity_dependency.
    TRY.
        CALL METHOD get_cds_dependency
          EXPORTING entity = entity
          IMPORTING dependency = dependency.
        IF dependency IS NOT INITIAL.
          RETURN.
        ENDIF.
      CATCH cx_root.
        " the entity can be a DDIC object
    ENDTRY.

    TRY.
        CALL METHOD get_tadir_dependency
          EXPORTING object = 'TABL' obj_name = entity
          RECEIVING dependency = dependency.
        RETURN.
      CATCH cx_root.
        " the entity can be a view
    ENDTRY.

    TRY.
        CALL METHOD get_tadir_dependency
          EXPORTING object = 'VIEW' obj_name = entity
          RECEIVING dependency = dependency.
      CATCH cx_root.
        " optional entity may not exist in the target system
    ENDTRY.
  ENDMETHOD.
  METHOD append_senvi_table_deps.
    DATA:
      lr_table  TYPE REF TO data,
      ls_senvi  TYPE senvi,
      lo_map    TYPE REF TO /atrm/cl_senvi_map,
      lo_origin TYPE REF TO /atrm/cl_object.

    FIELD-SYMBOLS:
      <lt_rows>   TYPE STANDARD TABLE,
      <ls_row>    TYPE any,
      <lv_type>   TYPE any,
      <lv_object> TYPE any.

    TRY.
        lo_origin ?= origin.
        CREATE DATA lr_table TYPE STANDARD TABLE OF (table_name).
        ASSIGN lr_table->* TO <lt_rows>.
        SELECT * FROM (table_name)
          INTO TABLE <lt_rows>
          WHERE (where_clause).

        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT type_field OF STRUCTURE <ls_row> TO <lv_type>.
          CHECK sy-subrc = 0.
          ASSIGN COMPONENT object_field OF STRUCTURE <ls_row>
            TO <lv_object>.
          CHECK sy-subrc = 0.
          CHECK <lv_type> IS NOT INITIAL.
          CHECK <lv_object> IS NOT INITIAL.

          CLEAR ls_senvi.
          ls_senvi-type = <lv_type>.
          ls_senvi-object = <lv_object>.
          TRY.
              CLEAR lo_map.
              CALL METHOD /atrm/cl_senvi_map=>get
                EXPORTING senvi = ls_senvi origin = lo_origin
                RECEIVING map = lo_map.
              CHECK lo_map IS BOUND.
              CALL METHOD lo_map->map_dependencies
                CHANGING deps = dependencies.
            CATCH cx_root.
              " optional mapped dependency may not exist
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " optional repository table may not exist in the target system
    ENDTRY.
  ENDMETHOD.
  METHOD append_composite_deps.
    DATA:
      lr_table      TYPE REF TO data,
      lv_first      TYPE string,
      lv_second     TYPE string,
      lv_object     TYPE sobj_name,
      ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_rows>   TYPE STANDARD TABLE,
      <ls_row>    TYPE any,
      <lv_first>  TYPE any,
      <lv_second> TYPE any.

    TRY.
        CREATE DATA lr_table TYPE STANDARD TABLE OF (table_name).
        ASSIGN lr_table->* TO <lt_rows>.
        SELECT * FROM (table_name)
          INTO TABLE <lt_rows>
          WHERE (where_clause).

        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT first_field OF STRUCTURE <ls_row> TO <lv_first>.
          CHECK sy-subrc = 0.
          ASSIGN COMPONENT second_field OF STRUCTURE <ls_row> TO <lv_second>.
          CHECK sy-subrc = 0.
          CHECK <lv_first> IS NOT INITIAL.
          CHECK <lv_second> IS NOT INITIAL.

          lv_first = <lv_first>.
          lv_second = <lv_second>.
          CHECK strlen( lv_second ) > second_offset.
          CONCATENATE lv_first lv_second+second_offset INTO lv_object.

          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = object_type obj_name = lv_object
                RECEIVING dependency = ls_dependency.
              IF ls_dependency IS NOT INITIAL.
                APPEND ls_dependency TO dependencies.
              ENDIF.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " optional repository table may not exist in the target system
    ENDTRY.
  ENDMETHOD.
  METHOD append_lrep_dependencies.
    DATA:
      lr_contents    TYPE REF TO data,
      lr_references  TYPE REF TO data,
      lr_target      TYPE REF TO data,
      lv_dcont       TYPE tabname,
      lv_dref        TYPE tabname,
      lv_where       TYPE string,
      lv_source_name TYPE string,
      lv_source_type TYPE string,
      lv_source_ns   TYPE string,
      lv_target_name TYPE string,
      lv_target_type TYPE string,
      lv_target_ns   TYPE string,
      lv_trobjtype   TYPE trobjtype,
      lv_trobjname   TYPE sobj_name,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_contents> TYPE STANDARD TABLE,
      <lt_refs>     TYPE STANDARD TABLE,
      <ls_content>  TYPE any,
      <ls_ref>      TYPE any,
      <ls_target>   TYPE any,
      <lv_value>    TYPE any.

    TRY.
        lv_dcont = '/UIF/LREPDCONT'.
        lv_dref = '/UIF/LREPDREF'.

        CREATE DATA lr_contents TYPE STANDARD TABLE OF (lv_dcont).
        ASSIGN lr_contents->* TO <lt_contents>.
        lv_trobjtype = object_type.
        lv_trobjname = object_name.
        REPLACE ALL OCCURRENCES OF '''' IN lv_trobjname WITH ''''''.
        CONCATENATE 'TROBJTYPE = ''' lv_trobjtype
                    ''' AND TROBJNAME = ''' lv_trobjname ''''
          INTO lv_where.
        SELECT * FROM (lv_dcont)
          INTO TABLE <lt_contents>
          WHERE (lv_where).

        LOOP AT <lt_contents> ASSIGNING <ls_content>.
          CLEAR: lv_source_name, lv_source_type, lv_source_ns.
          ASSIGN COMPONENT 'NAME' OF STRUCTURE <ls_content> TO <lv_value>.
          IF sy-subrc = 0.
            lv_source_name = <lv_value>.
          ENDIF.
          ASSIGN COMPONENT 'TYPE' OF STRUCTURE <ls_content> TO <lv_value>.
          IF sy-subrc = 0.
            lv_source_type = <lv_value>.
          ENDIF.
          ASSIGN COMPONENT 'NAMESPACE' OF STRUCTURE <ls_content> TO <lv_value>.
          IF sy-subrc = 0.
            lv_source_ns = <lv_value>.
          ENDIF.

          REPLACE ALL OCCURRENCES OF '''' IN lv_source_name WITH ''''''.
          REPLACE ALL OCCURRENCES OF '''' IN lv_source_type WITH ''''''.
          REPLACE ALL OCCURRENCES OF '''' IN lv_source_ns WITH ''''''.
          CONCATENATE '( TROBJNAME = ''' lv_trobjname
                      ''' ) OR ( NAME_1 = ''' lv_source_name
                      ''' AND TYPE_1 = ''' lv_source_type
                      ''' AND NAMESPACE_1 = ''' lv_source_ns ''' )'
            INTO lv_where.

          CREATE DATA lr_references TYPE STANDARD TABLE OF (lv_dref).
          ASSIGN lr_references->* TO <lt_refs>.
          SELECT * FROM (lv_dref)
            INTO TABLE <lt_refs>
            WHERE (lv_where).

          LOOP AT <lt_refs> ASSIGNING <ls_ref>.
            CLEAR: lv_target_name, lv_target_type, lv_target_ns.
            ASSIGN COMPONENT 'NAME_2' OF STRUCTURE <ls_ref> TO <lv_value>.
            IF sy-subrc = 0.
              lv_target_name = <lv_value>.
            ENDIF.
            ASSIGN COMPONENT 'TYPE_2' OF STRUCTURE <ls_ref> TO <lv_value>.
            IF sy-subrc = 0.
              lv_target_type = <lv_value>.
            ENDIF.
            ASSIGN COMPONENT 'NAMESPACE_2' OF STRUCTURE <ls_ref> TO <lv_value>.
            IF sy-subrc = 0.
              lv_target_ns = <lv_value>.
            ENDIF.
            CHECK lv_target_name IS NOT INITIAL.
            CHECK lv_target_type IS NOT INITIAL.

            REPLACE ALL OCCURRENCES OF '''' IN lv_target_name WITH ''''''.
            REPLACE ALL OCCURRENCES OF '''' IN lv_target_type WITH ''''''.
            REPLACE ALL OCCURRENCES OF '''' IN lv_target_ns WITH ''''''.
            CONCATENATE 'NAME = ''' lv_target_name
                        ''' AND TYPE = ''' lv_target_type
                        ''' AND NAMESPACE = ''' lv_target_ns ''''
              INTO lv_where.

            CREATE DATA lr_target TYPE (lv_dcont).
            ASSIGN lr_target->* TO <ls_target>.
            SELECT SINGLE * FROM (lv_dcont)
              INTO <ls_target>
              WHERE (lv_where).
            CHECK sy-subrc = 0.

            CLEAR: lv_trobjtype, lv_trobjname, ls_dependency.
            ASSIGN COMPONENT 'TROBJTYPE' OF STRUCTURE <ls_target>
              TO <lv_value>.
            CHECK sy-subrc = 0.
            lv_trobjtype = <lv_value>.
            ASSIGN COMPONENT 'TROBJNAME' OF STRUCTURE <ls_target>
              TO <lv_value>.
            CHECK sy-subrc = 0.
            lv_trobjname = <lv_value>.
            CHECK lv_trobjtype IS NOT INITIAL.
            CHECK lv_trobjname IS NOT INITIAL.

            TRY.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = lv_trobjtype obj_name = lv_trobjname
                  RECEIVING dependency = ls_dependency.
                IF ls_dependency IS NOT INITIAL.
                  APPEND ls_dependency TO dependencies.
                ENDIF.
              CATCH cx_root.
                " optional referenced LRep object may not exist
            ENDTRY.
          ENDLOOP.
        ENDLOOP.
      CATCH cx_root.
        " LRep dependency tables may not exist in the target system
    ENDTRY.
  ENDMETHOD.
  METHOD append_sdok_class_deps.
    DATA:
      lr_classes     TYPE REF TO data,
      lr_definition  TYPE REF TO data,
      lv_table       TYPE tabname,
      lv_name        TYPE sobj_name,
      lv_type        TYPE c LENGTH 1,
      lv_object_type TYPE trobjtype,
      lv_where       TYPE string,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_classes>   TYPE STANDARD TABLE,
      <ls_class>     TYPE any,
      <ls_definition> TYPE any,
      <lv_value>     TYPE any.

    TRY.
        CREATE DATA lr_classes TYPE STANDARD TABLE OF (table_name).
        ASSIGN lr_classes->* TO <lt_classes>.
        SELECT * FROM (table_name)
          INTO TABLE <lt_classes>
          WHERE (where_clause).

        LOOP AT <lt_classes> ASSIGNING <ls_class>.
          ASSIGN COMPONENT class_field OF STRUCTURE <ls_class> TO <lv_value>.
          CHECK sy-subrc = 0.
          CHECK <lv_value> IS NOT INITIAL.
          lv_name = <lv_value>.
          REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

          lv_table = 'SDOKME'.
          CREATE DATA lr_definition TYPE (lv_table).
          ASSIGN lr_definition->* TO <ls_definition>.
          CONCATENATE 'ENTITY = ''' lv_name '''' INTO lv_where.
          SELECT SINGLE * FROM (lv_table)
            INTO <ls_definition>
            WHERE (lv_where).
          CHECK sy-subrc = 0.

          ASSIGN COMPONENT 'TYPE' OF STRUCTURE <ls_definition> TO <lv_value>.
          CHECK sy-subrc = 0.
          lv_type = <lv_value>.
          CLEAR lv_object_type.
          CASE lv_type.
            WHEN '1'. lv_object_type = 'SMD1'.
            WHEN '2'. lv_object_type = 'SMD2'.
            WHEN '3'. lv_object_type = 'SMD3'.
            WHEN '4'. lv_object_type = 'SMD4'.
            WHEN '5'. lv_object_type = 'SMD5'.
            WHEN '6'. lv_object_type = 'SMD6'.
            WHEN 'A'. lv_object_type = 'SMDA'.
            WHEN 'C'. lv_object_type = 'SMDC'.
            WHEN 'M'. lv_object_type = 'SMDM'.
            WHEN 'X'. lv_object_type = 'SMDK'.
          ENDCASE.
          CHECK lv_object_type IS NOT INITIAL.

          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = lv_object_type obj_name = lv_name
                RECEIVING dependency = ls_dependency.
              IF ls_dependency IS NOT INITIAL.
                APPEND ls_dependency TO dependencies.
              ENDIF.
            CATCH cx_root.
              " optional document-model class may not exist
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " document-model APIs may not exist in the target system
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS /atrm/cl_package DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING devclass TYPE devclass.

    CLASS-METHODS create
      IMPORTING data           TYPE scompkdtln
      RETURNING VALUE(package) TYPE REF TO /atrm/cl_package
      RAISING   /atrm/cx_exception.

    METHODS delete_tmp
      RAISING /atrm/cx_exception.

    METHODS get_subpackages
      RETURNING VALUE(subpackages) TYPE cl_pak_package_queries=>tt_subpackage_info.

    METHODS get_all_packages
      RETURNING VALUE(packages) TYPE /atrm/package_devclass_t.

    METHODS get_objects
      IMPORTING incl_sub TYPE flag DEFAULT ' '
      EXPORTING tadir    TYPE scts_tadir
      RAISING   /atrm/cx_exception.

    METHODS interface
      IMPORTING parentcl    TYPE devclass OPTIONAL
                rm_parentcl TYPE flag OPTIONAL
                devlayer    TYPE devlayer OPTIONAL
      RAISING   /atrm/cx_exception.

    METHODS get_dirty_entries
      IMPORTING from_date    TYPE as4date
                from_time    TYPE as4time
      RETURNING VALUE(dirty) TYPE /atrm/dirty_t
      RAISING   /atrm/cx_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: gv_devclass TYPE devclass.

    CLASS-METHODS modify_package_data
      IMPORTING package_data_sign TYPE scompksign
      CHANGING  package_data      TYPE scompkdtln
                transport_request TYPE e070-trkorr
      RAISING   /atrm/cx_exception.

    "! Calls a package API method passing only the parameters its signature has on this release
    "! (e.g. no I_SUPPRESS_DIALOG on 702, no I_SUPPRESS_ACCESS_PERMISSION on 740), so a missing
    "! optional parameter never costs the others
    "! @parameter class_name | Class of a static call; ignored when instance is bound
    "! @parameter instance | Object of an instance call
    "! @parameter method_name | Method, interface methods as INTF~METHOD (upper case)
    "! @parameter tolerate | Classic exception that counts as success, when the method has it
    "! @parameter subrc | 0, or 1 when the method raised a classic exception
    CLASS-METHODS call_api
      IMPORTING class_name   TYPE string OPTIONAL
                instance     TYPE REF TO object OPTIONAL
                method_name  TYPE string
                parameters   TYPE abap_parmbind_tab
                tolerate     TYPE string OPTIONAL
      RETURNING VALUE(subrc) TYPE sy-subrc
      RAISING   /atrm/cx_exception.

    "! Adds an EXPORTING parameter, passed by value
    CLASS-METHODS with_value
      IMPORTING parameters    TYPE abap_parmbind_tab
                name          TYPE string
                value         TYPE any
      RETURNING VALUE(result) TYPE abap_parmbind_tab.

    "! Adds a parameter bound to the caller's variable (IMPORTING/CHANGING)
    CLASS-METHODS with_ref
      IMPORTING parameters    TYPE abap_parmbind_tab
                name          TYPE string
                kind          TYPE abap_parmkind
                ref           TYPE REF TO data
      RETURNING VALUE(result) TYPE abap_parmbind_tab.

    CLASS-METHODS load
      IMPORTING devclass       TYPE devclass
                force_reload   TYPE flag OPTIONAL
      RETURNING VALUE(package) TYPE REF TO if_package
      RAISING   /atrm/cx_exception.

    CLASS-METHODS set_changeable
      IMPORTING package                    TYPE REF TO if_package
                changeable                 TYPE flag
                suppress_dialog            TYPE flag
                suppress_access_permission TYPE flag OPTIONAL
                tolerate                   TYPE string OPTIONAL
      RAISING   /atrm/cx_exception.

    CLASS-METHODS save
      IMPORTING package           TYPE REF TO if_package
      CHANGING  transport_request TYPE trkorr OPTIONAL
      RAISING   /atrm/cx_exception.
    "! Signature of a method on this release
    "! @parameter class_name | Class, for a method that isn't INTF~METHOD
    "! @parameter method_name | Method, interface methods as INTF~METHOD (upper case)
    "! @parameter descr | Method description
    "! @parameter found | X when the method was described
    CLASS-METHODS describe_method
      IMPORTING class_name  TYPE string OPTIONAL
                method_name TYPE string
      EXPORTING descr       TYPE abap_methdescr
                found       TYPE flag.
ENDCLASS.



CLASS /atrm/cl_package IMPLEMENTATION.


  METHOD constructor.
    gv_devclass = devclass.
  ENDMETHOD.


  METHOD create.
    DATA ls_data LIKE data.
    DATA lo_package TYPE REF TO if_package.
    DATA: lt_parameters TYPE abap_parmbind_tab,
          lr_ref        TYPE REF TO data.
    MOVE data TO ls_data.

    IF ls_data-as4user IS INITIAL.
      ls_data-as4user = sy-uname.
    ENDIF.
    IF ls_data-masterlang IS INITIAL.
      ls_data-masterlang = sy-langu.
    ENDIF.

    GET REFERENCE OF lo_package INTO lr_ref.
    lt_parameters = with_ref( parameters = lt_parameters name = 'E_PACKAGE' kind = cl_abap_objectdescr=>importing ref = lr_ref ).
    GET REFERENCE OF ls_data INTO lr_ref.
    lt_parameters = with_ref( parameters = lt_parameters name = 'C_PACKAGE_DATA' kind = cl_abap_objectdescr=>changing ref = lr_ref ).
    lt_parameters = with_value( parameters = lt_parameters name = 'I_SUPPRESS_DIALOG' value = 'X' ).
    IF call_api( class_name  = 'CL_PACKAGE_FACTORY'
                 method_name = 'IF_PACKAGE_FACTORY~CREATE_NEW_PACKAGE'
                 parameters  = lt_parameters ) <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.

    save( package = lo_package ).

    set_changeable( package = lo_package changeable = ' ' suppress_dialog = 'X' ).

    IF ls_data-devclass(1) <> '$'.
      DATA: lv_objname    TYPE sobj_name,
            lv_devclass   TYPE devclass,
            lv_author     TYPE responsibl,
            lv_masterlang TYPE masterlang.
      lv_objname = ls_data-devclass.
      lv_devclass = ls_data-devclass.
      lv_author = ls_data-as4user.
      lv_masterlang = ls_data-masterlang.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_test_modus       = abap_false
          wi_tadir_pgmid      = 'R3TR'
          wi_tadir_object     = 'DEVC'
          wi_tadir_obj_name   = lv_objname
          wi_tadir_devclass   = lv_devclass
          wi_tadir_author     = lv_author
          wi_tadir_masterlang = lv_masterlang
        EXCEPTIONS
          OTHERS              = 0.
    ENDIF.

    CREATE OBJECT package EXPORTING devclass = ls_data-devclass.
  ENDMETHOD.

  METHOD delete_tmp.
    DATA lo_package TYPE REF TO if_package.
    DATA lt_parameters TYPE abap_parmbind_tab.

    IF gv_devclass(1) <> '$'.
      /atrm/cx_exception=>raise( iv_reason  = /atrm/cx_exception=>c_reason-package_not_temporary ).
    ENDIF.

    lo_package = load( devclass = gv_devclass ).

    set_changeable( package = lo_package changeable = 'X' suppress_dialog = 'X' ).

    lt_parameters = with_value( parameters = lt_parameters name = 'I_SUPPRESS_DIALOG' value = 'X' ).
    IF call_api( instance    = lo_package
                 method_name = 'IF_PACKAGE~DELETE'
                 parameters  = lt_parameters ) <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.

    save( package = lo_package ).
  ENDMETHOD.

  METHOD get_objects.
    TYPES: BEGIN OF ty_ddls_gen_view,
             objectname TYPE char40,
             objecttype TYPE char4,
           END OF ty_ddls_gen_view.
    DATA: lt_devclass      TYPE STANDARD TABLE OF devclass,
          lt_subpackages   TYPE cl_pak_package_queries=>tt_subpackage_info,
          ls_subpackage    LIKE LINE OF lt_subpackages,
          lv_devclass      TYPE devclass,
          lt_tadir         LIKE tadir,
          ls_tadir         LIKE LINE OF tadir,
          lt_ddls          TYPE STANDARD TABLE OF char40,
          lt_ddls_gen_view TYPE STANDARD TABLE OF ty_ddls_gen_view,
          ls_ddls_gen_view LIKE LINE OF lt_ddls_gen_view.

    lt_devclass = get_all_packages( ).
    IF incl_sub <> 'X'.
      DELETE lt_devclass WHERE table_line <> gv_devclass.
    ENDIF.
    LOOP AT lt_devclass INTO lv_devclass.
      CLEAR lt_tadir[].
      CALL FUNCTION 'TRINT_SELECT_OBJECTS'
        EXPORTING
          iv_devclass      = lv_devclass
          iv_via_selscreen = ' '
        IMPORTING
          et_objects_tadir = lt_tadir
        EXCEPTIONS
          OTHERS           = 1.
      IF sy-subrc <> 0.
        /atrm/cx_exception=>raise( ).
      ENDIF.
      APPEND LINES OF lt_tadir TO tadir.
    ENDLOOP.
    LOOP AT tadir INTO ls_tadir WHERE pgmid EQ 'R3TR' AND object EQ 'DDLS'.
      APPEND ls_tadir-obj_name TO lt_ddls.
    ENDLOOP.
    IF lt_ddls[] IS NOT INITIAL.
      TRY.
          SELECT DISTINCT objectname objecttype
          FROM ('DDLDEPENDENCY')
          INTO CORRESPONDING FIELDS OF TABLE lt_ddls_gen_view
          FOR ALL ENTRIES IN lt_ddls
          WHERE ddlname EQ lt_ddls-table_line.
        CATCH cx_sy_dynamic_osql_semantics.
      ENDTRY.
      LOOP AT lt_ddls_gen_view INTO ls_ddls_gen_view.
        DELETE tadir WHERE pgmid EQ 'R3TR' AND object EQ ls_ddls_gen_view-objecttype AND obj_name EQ ls_ddls_gen_view-objectname.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD modify_package_data.
    DATA: lo_package TYPE REF TO if_package,
          lx_save    TYPE REF TO /atrm/cx_exception.

* load package
    lo_package = load( devclass = package_data-devclass force_reload = 'X' ).

* lock package
    set_changeable( package                    = lo_package
                    changeable                 = 'X'
                    suppress_dialog            = 'D'
                    suppress_access_permission = 'X'
                    tolerate                   = 'OBJECT_ALREADY_CHANGEABLE' ).

* modify package
    CALL METHOD lo_package->set_all_attributes
      EXPORTING
        i_package_data = package_data
        i_data_sign    = package_data_sign
      EXCEPTIONS
        OTHERS         = 1.
*
    IF sy-subrc <> 0.
*   try to unlock the package, exceptions are tolerated
      TRY.
          set_changeable( package                    = lo_package
                          changeable                 = ' '
                          suppress_dialog            = 'D'
                          suppress_access_permission = 'X' ).
        CATCH /atrm/cx_exception.
      ENDTRY.
      /atrm/cx_exception=>raise( ).
    ENDIF.

* save package
    TRY.
        CALL METHOD save
          EXPORTING
            package           = lo_package
          CHANGING
            transport_request = transport_request.
      CATCH /atrm/cx_exception INTO lx_save.
*   try to undo the changes, exceptions are tolerated
*   (Note: if successful, this also unlocks the package)
        CALL METHOD lo_package->undo_all_changes
          EXCEPTIONS
            OTHERS = 0.
        RAISE EXCEPTION lx_save.
    ENDTRY.

* unlock package
    set_changeable( package                    = lo_package
                    changeable                 = ' '
                    suppress_dialog            = 'D'
                    suppress_access_permission = 'X'
                    tolerate                   = 'OBJECT_ALREADY_UNLOCKED' ).
  ENDMETHOD.


  METHOD interface.
    DATA: ls_modify_sign TYPE scompksign,
          ls_pack_data   TYPE scompkdtln,
          ls_cr          TYPE e070-trkorr.
    IF rm_parentcl EQ 'X'.
      "SAP Note 636704
      ls_modify_sign-parentcl = 'X'.
      ls_pack_data-devclass   = gv_devclass.
      modify_package_data(
        EXPORTING
          package_data_sign = ls_modify_sign
        CHANGING
          package_data      = ls_pack_data
          transport_request = ls_cr
      ).
      CLEAR ls_modify_sign.
      CLEAR ls_pack_data.
      CLEAR ls_cr.
    ELSEIF parentcl IS NOT INITIAL.
      DATA lo_package TYPE REF TO if_package.
      lo_package = load( devclass = gv_devclass ).
      set_changeable( package                    = lo_package
                      changeable                 = 'X'
                      suppress_dialog            = 'D'
                      suppress_access_permission = 'X'
                      tolerate                   = 'OBJECT_ALREADY_CHANGEABLE' ).
      CALL METHOD lo_package->('SET_SUPER_PACKAGE_NAME')
        EXPORTING
          i_super_package_name = parentcl
        EXCEPTIONS
          OTHERS               = 1.
      IF sy-subrc <> 0.
        /atrm/cx_exception=>raise( ).
      ENDIF.
      save( package = lo_package ).
      set_changeable( package                    = lo_package
                      changeable                 = ' '
                      suppress_dialog            = 'D'
                      suppress_access_permission = 'X' ).
    ENDIF.
    IF devlayer IS NOT INITIAL.
      ls_modify_sign-pdevclass = 'X'.
      ls_pack_data-devclass   = gv_devclass.
      ls_pack_data-pdevclass   = devlayer.
      modify_package_data(
        EXPORTING
          package_data_sign = ls_modify_sign
        CHANGING
          package_data      = ls_pack_data
          transport_request = ls_cr
      ).
      CLEAR ls_modify_sign.
      CLEAR ls_pack_data.
      CLEAR ls_cr.
    ENDIF.
  ENDMETHOD.


  METHOD get_subpackages.
    TRY.
        CALL METHOD ('CL_PAK_PACKAGE_QUERIES')=>('GET_ALL_SUBPACKAGES')
          EXPORTING
            im_package                    = gv_devclass
            im_also_local_packages        = 'X'
          IMPORTING
            et_subpackages                = subpackages
          EXCEPTIONS
            no_package_specified          = 1
            package_has_no_tdevc_record   = 2
            package_has_no_tadir_record   = 3
            package_does_not_exist        = 4
            invalid_superpackage          = 5
            no_output_parameter_requested = 6
            OTHERS                        = 7.
      CATCH cx_dynamic_check.
        CALL METHOD ('CL_PAK_PACKAGE_QUERIES')=>('GET_ALL_SUBPACKAGES')
         EXPORTING
           im_package                    = gv_devclass
         IMPORTING
            et_subpackages                = subpackages
          EXCEPTIONS
           no_package_specified          = 1
           package_has_no_tdevc_record   = 2
           package_has_no_tadir_record   = 3
            package_does_not_exist        = 4
            invalid_superpackage          = 5
           no_output_parameter_requested = 6
           OTHERS                        = 7
.
    ENDTRY.
  ENDMETHOD.


  METHOD get_all_packages.
    DATA: subpackages TYPE cl_pak_package_queries=>tt_subpackage_info,
          subpackage  LIKE LINE OF subpackages.
    subpackages = get_subpackages( ).
    APPEND gv_devclass TO packages.
    LOOP AT subpackages INTO subpackage.
      APPEND subpackage-package TO packages.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_dirty_entries.
    TYPES: BEGIN OF transport,
             trkorr   TYPE trkorr,
             strkorr  TYPE strkorr,
             pgmid    TYPE pgmid,
             object   TYPE trobjtype,
             obj_name TYPE trobj_name,
           END OF transport.
    DATA: tadir           TYPE scts_tadir,
          transports      TYPE STANDARD TABLE OF transport,
          transport_line  LIKE LINE OF transports,
          e071            TYPE e071,
          parent          TYPE tadir,
          parent_e071     TYPE e071,
          e071_dirty      TYPE STANDARD TABLE OF transport,
          e071_dirty_line LIKE LINE OF e071_dirty,
          dirty_line      LIKE LINE OF dirty,
          e07t            TYPE STANDARD TABLE OF e07t,
          e07t_line       LIKE LINE OF e07t.
    FIELD-SYMBOLS <dirty_line> TYPE /atrm/dirty.

    get_objects(
      EXPORTING
        incl_sub = 'X'
      IMPORTING
        tadir    = tadir
    ).
    CHECK tadir[] IS NOT INITIAL.

    SELECT e070~trkorr e070~strkorr e071~pgmid e071~object e071~obj_name
      FROM e070
      INNER JOIN e071 ON e071~trkorr = e070~trkorr
      INTO CORRESPONDING FIELDS OF TABLE transports
      WHERE ( e070~trfunction EQ 'K' OR e070~trfunction EQ 'S' OR e070~trfunction EQ 'R' )
        AND ( e070~as4date GT from_date OR (
              e070~as4date EQ from_date AND
              e070~as4time GT from_time
             ) ).
    CHECK transports[] IS NOT INITIAL.

    LOOP AT transports INTO transport_line.
      CLEAR parent.
      CLEAR parent_e071.
      IF transport_line-pgmid EQ 'R3TR'.
        parent-pgmid = transport_line-pgmid.
        parent-object = transport_line-object.
        parent-obj_name = transport_line-obj_name.
      ELSE.
        CLEAR e071.
        MOVE-CORRESPONDING transport_line TO e071.
        CALL FUNCTION 'TR_CHECK_TYPE'
          EXPORTING
            wi_e071  = e071
          IMPORTING
            we_tadir = parent.
      ENDIF.
      parent_e071-pgmid = parent-pgmid.
      parent_e071-object = parent-object.
      parent_e071-obj_name = parent-obj_name.
      READ TABLE tadir TRANSPORTING NO FIELDS WITH KEY pgmid = parent_e071-pgmid object = parent_e071-object obj_name = parent_e071-obj_name.
      CHECK sy-subrc EQ 0.
      APPEND transport_line TO e071_dirty.
    ENDLOOP.

    LOOP AT e071_dirty INTO e071_dirty_line.
      CLEAR dirty_line.
      " report tasks under their request (a modifiable request has no own E071 entries yet)
      IF e071_dirty_line-strkorr IS NOT INITIAL.
        dirty_line-trkorr = e071_dirty_line-strkorr.
      ELSE.
        dirty_line-trkorr = e071_dirty_line-trkorr.
      ENDIF.
      dirty_line-pgmid = e071_dirty_line-pgmid.
      dirty_line-object = e071_dirty_line-object.
      dirty_line-obj_name = e071_dirty_line-obj_name.
      READ TABLE dirty TRANSPORTING NO FIELDS WITH KEY trkorr = dirty_line-trkorr pgmid = dirty_line-pgmid object = dirty_line-object obj_name = dirty_line-obj_name.
      CHECK sy-subrc NE 0.
      APPEND dirty_line TO dirty.
    ENDLOOP.

    CHECK dirty[] IS NOT INITIAL.

    SELECT trkorr langu as4text
    FROM e07t
    INTO CORRESPONDING FIELDS OF TABLE e07t
    FOR ALL ENTRIES IN dirty
    WHERE trkorr EQ dirty-trkorr.

    LOOP AT dirty ASSIGNING <dirty_line>.
      CLEAR e07t_line.
      READ TABLE e07t INTO e07t_line WITH KEY trkorr = <dirty_line>-trkorr langu = sy-langu.
      IF sy-subrc EQ 0.
        <dirty_line>-as4text = e07t_line-as4text.
      ELSE.
        LOOP AT e07t INTO e07t_line WHERE trkorr EQ <dirty_line>-trkorr.
          <dirty_line>-as4text = e07t_line-as4text.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD call_api.
    DATA: ls_methdescr  TYPE abap_methdescr,
          lv_known      TYPE flag,
          lt_parameters TYPE abap_parmbind_tab,
          ls_parameter  LIKE LINE OF lt_parameters,
          lt_exceptions TYPE abap_excpbind_tab,
          ls_exception  LIKE LINE OF lt_exceptions,
          lx_dyn_call   TYPE REF TO cx_sy_dyn_call_error.

    CALL METHOD describe_method
      EXPORTING
        class_name  = class_name
        method_name = method_name
      IMPORTING
        descr       = ls_methdescr
        found       = lv_known.

    "pass only the parameters the method has: a missing one never drops the others
    IF lv_known = 'X'.
      LOOP AT parameters INTO ls_parameter.
        READ TABLE ls_methdescr-parameters TRANSPORTING NO FIELDS WITH KEY name = ls_parameter-name.
        IF sy-subrc = 0.
          INSERT ls_parameter INTO TABLE lt_parameters.
        ENDIF.
      ENDLOOP.
    ELSE.
      lt_parameters = parameters.
    ENDIF.

    IF tolerate IS NOT INITIAL.
      IF lv_known = 'X'.
        READ TABLE ls_methdescr-exceptions TRANSPORTING NO FIELDS WITH KEY name = tolerate.
      ENDIF.
      IF lv_known <> 'X' OR sy-subrc = 0.
        ls_exception-name = tolerate.
        ls_exception-value = 0.
        INSERT ls_exception INTO TABLE lt_exceptions.
      ENDIF.
    ENDIF.
    ls_exception-name = 'OTHERS'.
    ls_exception-value = 1.
    INSERT ls_exception INTO TABLE lt_exceptions.

    TRY.
        IF instance IS BOUND.
          CALL METHOD instance->(method_name)
            PARAMETER-TABLE lt_parameters
            EXCEPTION-TABLE lt_exceptions.
        ELSE.
          CALL METHOD (class_name)=>(method_name)
            PARAMETER-TABLE lt_parameters
            EXCEPTION-TABLE lt_exceptions.
        ENDIF.
        subrc = sy-subrc.
      CATCH cx_sy_dyn_call_error INTO lx_dyn_call.
        /atrm/cx_exception=>raise( iv_reason = /atrm/cx_exception=>c_reason-dyn_call_param_not_found
                                   io_root   = lx_dyn_call ).
    ENDTRY.
  ENDMETHOD.


  METHOD with_value.
    DATA: ls_parameter LIKE LINE OF result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    result = parameters.
    ls_parameter-name = name.
    ls_parameter-kind = cl_abap_objectdescr=>exporting.
    CREATE DATA ls_parameter-value LIKE value.
    ASSIGN ls_parameter-value->* TO <lv_value>.
    <lv_value> = value.
    INSERT ls_parameter INTO TABLE result.
  ENDMETHOD.


  METHOD with_ref.
    DATA: ls_parameter LIKE LINE OF result.

    result = parameters.
    ls_parameter-name = name.
    ls_parameter-kind = kind.
    ls_parameter-value = ref.
    INSERT ls_parameter INTO TABLE result.
  ENDMETHOD.


  METHOD load.
    DATA: lt_parameters TYPE abap_parmbind_tab,
          lr_ref        TYPE REF TO data.

    lt_parameters = with_value( parameters = lt_parameters name = 'I_PACKAGE_NAME' value = devclass ).
    IF force_reload IS NOT INITIAL.
      lt_parameters = with_value( parameters = lt_parameters name = 'I_FORCE_RELOAD' value = force_reload ).
    ENDIF.
    GET REFERENCE OF package INTO lr_ref.
    lt_parameters = with_ref( parameters = lt_parameters name = 'E_PACKAGE' kind = cl_abap_objectdescr=>importing ref = lr_ref ).
    IF call_api( class_name  = 'CL_PACKAGE_FACTORY'
                 method_name = 'IF_PACKAGE_FACTORY~LOAD_PACKAGE'
                 parameters  = lt_parameters ) <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.
  ENDMETHOD.


  METHOD set_changeable.
    DATA: lt_parameters  TYPE abap_parmbind_tab,
          ls_methdescr   TYPE abap_methdescr,
          lv_found       TYPE flag,
          lv_masterlang  TYPE tadir-masterlang,
          lv_logon_langu TYPE sy-langu,
          lv_switched    TYPE flag,
          lv_subrc       TYPE sy-subrc,
          lx_exception   TYPE REF TO /atrm/cx_exception.

    lt_parameters = with_value( parameters = lt_parameters name = 'I_CHANGEABLE' value = changeable ).
    lt_parameters = with_value( parameters = lt_parameters name = 'I_SUPPRESS_DIALOG' value = suppress_dialog ).
    IF suppress_access_permission IS NOT INITIAL.
      lt_parameters = with_value( parameters = lt_parameters name = 'I_SUPPRESS_ACCESS_PERMISSION' value = suppress_access_permission ).
    ENDIF.

    "without I_SUPPRESS_ACCESS_PERMISSION the lock runs RS_ACCESS_PERMISSION, whose master
    "language check (RS_TEXTLOG_MASTER_LANGUAGE) shows a popup when the logon language isn't
    "the package's original language, and I_SUPPRESS_DIALOG doesn't reach it: over RFC that is
    "DYNPRO_SEND_IN_BACKGROUND. Lock in the original language instead
    CALL METHOD describe_method
      EXPORTING
        method_name = 'IF_PACKAGE~SET_CHANGEABLE'
      IMPORTING
        descr       = ls_methdescr
        found       = lv_found.
    IF lv_found = 'X'.
      READ TABLE ls_methdescr-parameters TRANSPORTING NO FIELDS WITH KEY name = 'I_SUPPRESS_ACCESS_PERMISSION'.
    ENDIF.
    IF suppress_access_permission IS INITIAL OR lv_found <> 'X' OR sy-subrc <> 0.
      SELECT SINGLE masterlang FROM tadir INTO lv_masterlang
        WHERE pgmid = 'R3TR' AND object = 'DEVC' AND obj_name = package->package_name.
      IF sy-subrc = 0 AND lv_masterlang IS NOT INITIAL AND lv_masterlang <> sy-langu.
        lv_logon_langu = sy-langu.
        TRY.
            SET LOCALE LANGUAGE lv_masterlang.
            lv_switched = 'X'.
          CATCH cx_sy_localization_error.
            "language not available on this server: keep the logon language
        ENDTRY.
      ENDIF.
    ENDIF.

    TRY.
        lv_subrc = call_api( instance    = package
                             method_name = 'IF_PACKAGE~SET_CHANGEABLE'
                             parameters  = lt_parameters
                             tolerate    = tolerate ).
      CATCH /atrm/cx_exception INTO lx_exception.
    ENDTRY.
    IF lv_switched = 'X'.
      SET LOCALE LANGUAGE lv_logon_langu.
    ENDIF.
    IF lx_exception IS BOUND.
      RAISE EXCEPTION lx_exception.
    ENDIF.
    IF lv_subrc <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.
  ENDMETHOD.


  METHOD save.
    DATA: lt_parameters TYPE abap_parmbind_tab,
          lr_ref        TYPE REF TO data.

    IF transport_request IS SUPPLIED.
      lt_parameters = with_value( parameters = lt_parameters name = 'I_TRANSPORT_REQUEST' value = transport_request ).
      "reference to the caller's variable: the formal parameter is passed by reference
      GET REFERENCE OF transport_request INTO lr_ref.
      lt_parameters = with_ref( parameters = lt_parameters name = 'E_TRANSPORT_REQUEST' kind = cl_abap_objectdescr=>importing ref = lr_ref ).
    ENDIF.
    lt_parameters = with_value( parameters = lt_parameters name = 'I_SUPPRESS_DIALOG' value = 'X' ).
    lt_parameters = with_value( parameters = lt_parameters name = 'I_SUPPRESS_CORR_INSERT' value = 'X' ).
    IF call_api( instance    = package
                 method_name = 'IF_PACKAGE~SAVE'
                 parameters  = lt_parameters ) <> 0.
      /atrm/cx_exception=>raise( ).
    ENDIF.
  ENDMETHOD.

  METHOD describe_method.
    DATA: lv_type     TYPE string,
          lv_method   TYPE string,
          lo_descr    TYPE REF TO cl_abap_typedescr,
          lo_objdescr TYPE REF TO cl_abap_objectdescr.

    CLEAR: descr, found.
    "read the signature from the declaring interface for INTF~METHOD, else from the class
    IF method_name CS '~'.
      SPLIT method_name AT '~' INTO lv_type lv_method.
    ELSE.
      lv_type = class_name.
      lv_method = method_name.
    ENDIF.
    CALL METHOD cl_abap_typedescr=>describe_by_name
      EXPORTING
        p_name         = lv_type
      RECEIVING
        p_descr_ref    = lo_descr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2.
    CHECK sy-subrc = 0.
    TRY.
        lo_objdescr ?= lo_descr.
        READ TABLE lo_objdescr->methods INTO descr WITH KEY name = lv_method.
        IF sy-subrc = 0.
          found = 'X'.
        ENDIF.
      CATCH cx_sy_move_cast_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

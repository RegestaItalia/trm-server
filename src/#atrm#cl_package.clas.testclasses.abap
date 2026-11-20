CLASS ltc_package DEFINITION DEFERRED.
CLASS /atrm/cl_package DEFINITION LOCAL FRIENDS ltc_package.

CLASS ltc_package DEFINITION FOR TESTING
  "#AU Risk_Level Harmless
  "#AU Duration Short
.
  PRIVATE SECTION.
    DATA: mv_root  TYPE devclass,
          mv_child TYPE devclass.
    METHODS setup.
    METHODS subpackages_contain_child FOR TESTING.
    METHODS all_packages_start_with_root FOR TESTING.
    METHODS same_as_package_api FOR TESTING.
    METHODS target_same_as_cts FOR TESTING.
ENDCLASS.

CLASS ltc_package IMPLEMENTATION.

  METHOD setup.
    " Installed package names can differ: derive a parent/child pair from the
    " package holding the object handlers and its parent package
    SELECT SINGLE devclass FROM tadir INTO mv_child
      WHERE pgmid = 'R3TR' AND object = 'CLAS' AND obj_name = '/ATRM/CL_OBJECT'.
    IF mv_child IS NOT INITIAL.
      SELECT SINGLE parentcl FROM tdevc INTO mv_root WHERE devclass = mv_child.
    ENDIF.
  ENDMETHOD.

  METHOD subpackages_contain_child.
    DATA: lo_package     TYPE REF TO /atrm/cl_package,
          lt_subpackages TYPE /atrm/package_devclass_t.
    IF mv_root IS INITIAL.
      RETURN. " handlers not in a subpackage here: nothing to check
    ENDIF.
    CREATE OBJECT lo_package EXPORTING devclass = mv_root.
    lt_subpackages = lo_package->get_subpackages( ).
    READ TABLE lt_subpackages TRANSPORTING NO FIELDS WITH KEY table_line = mv_child.
    cl_aunit_assert=>assert_subrc( act = sy-subrc exp = 0 msg = 'Child package missing' ).
    READ TABLE lt_subpackages TRANSPORTING NO FIELDS WITH KEY table_line = mv_root.
    cl_aunit_assert=>assert_subrc( act = sy-subrc exp = 4 msg = 'Root must not be its own subpackage' ).
  ENDMETHOD.

  METHOD all_packages_start_with_root.
    DATA: lo_package  TYPE REF TO /atrm/cl_package,
          lt_packages TYPE /atrm/package_devclass_t,
          lv_first    TYPE devclass.
    IF mv_root IS INITIAL.
      RETURN.
    ENDIF.
    CREATE OBJECT lo_package EXPORTING devclass = mv_root.
    lt_packages = lo_package->get_all_packages( ).
    READ TABLE lt_packages INTO lv_first INDEX 1.
    cl_aunit_assert=>assert_equals( act = lv_first exp = mv_root ).
    READ TABLE lt_packages TRANSPORTING NO FIELDS WITH KEY table_line = mv_child.
    cl_aunit_assert=>assert_subrc( act = sy-subrc exp = 0 msg = 'Child package missing' ).
  ENDMETHOD.

  METHOD same_as_package_api.
    " Where the package API exists, the TDEVC walk must find the same packages
    DATA: lo_package     TYPE REF TO /atrm/cl_package,
          lt_subpackages TYPE /atrm/package_devclass_t,
          lt_api         TYPE /atrm/package_devclass_t,
          lr_api_result  TYPE REF TO data,
          lv_lines_own   TYPE i,
          lv_lines_api   TYPE i.
    FIELD-SYMBOLS: <lt_api_result> TYPE ANY TABLE,
                   <ls_api_row>    TYPE any,
                   <lv_package>    TYPE any.
    IF mv_root IS INITIAL.
      RETURN.
    ENDIF.
    TRY.
        CREATE DATA lr_api_result TYPE ('CL_PAK_PACKAGE_QUERIES=>TT_SUBPACKAGE_INFO').
      CATCH cx_root.
        RETURN. " no package API on this release (e.g. SAP_BASIS 700)
    ENDTRY.
    ASSIGN lr_api_result->* TO <lt_api_result>.
    TRY.
        CALL METHOD ('CL_PAK_PACKAGE_QUERIES')=>('GET_ALL_SUBPACKAGES')
          EXPORTING
            im_package             = mv_root
            im_also_local_packages = 'X'
          IMPORTING
            et_subpackages         = <lt_api_result>
          EXCEPTIONS
            OTHERS                 = 1.
      CATCH cx_root.
        RETURN.
    ENDTRY.
    CHECK sy-subrc = 0.
    LOOP AT <lt_api_result> ASSIGNING <ls_api_row>.
      ASSIGN COMPONENT 'PACKAGE' OF STRUCTURE <ls_api_row> TO <lv_package>.
      CHECK sy-subrc = 0.
      APPEND <lv_package> TO lt_api.
    ENDLOOP.
    CREATE OBJECT lo_package EXPORTING devclass = mv_root.
    lt_subpackages = lo_package->get_subpackages( ).
    SORT: lt_api, lt_subpackages.
    lv_lines_own = lines( lt_subpackages ).
    lv_lines_api = lines( lt_api ).
    cl_aunit_assert=>assert_equals( act = lv_lines_own exp = lv_lines_api msg = 'Subpackage count differs from package API' ).
    cl_aunit_assert=>assert_equals( act = lt_subpackages exp = lt_api msg = 'Subpackages differ from package API' ).
  ENDMETHOD.

  METHOD target_same_as_cts.
    " The temporary request must have the target CTS checks the package against:
    " get_devclass_target must agree with TR_GET_TRANSPORT_TARGET, $SPL packages included
    DATA: lt_devclass TYPE STANDARD TABLE OF devclass,
          lt_sample   TYPE STANDARD TABLE OF devclass,
          lv_devclass TYPE devclass,
          lv_own      TYPE tr_target,
          lv_cts      TYPE tr_target,
          lv_msg      TYPE string.
    SELECT devclass FROM tdevc INTO TABLE lt_devclass WHERE pdevclass = '$SPL'.
    SELECT devclass FROM tdevc UP TO 50 ROWS INTO TABLE lt_sample
      WHERE pdevclass <> '$SPL' AND devclass NOT LIKE '$%'.
    APPEND LINES OF lt_sample TO lt_devclass.
    LOOP AT lt_devclass INTO lv_devclass.
      CLEAR lv_cts.
      TRY.
          CALL FUNCTION 'TR_GET_TRANSPORT_TARGET'
            EXPORTING
              iv_development_class       = lv_devclass
            IMPORTING
              ev_target                  = lv_cts
            EXCEPTIONS
              wrong_call                 = 1
              invalid_input              = 2
              cts_initialization_failure = 3
              OTHERS                     = 4.
        CATCH cx_sy_dyn_call_error.
          RETURN. " another signature on this release: nothing to compare with
      ENDTRY.
      IF sy-subrc <> 0.
        CLEAR lv_cts.
      ENDIF.
      lv_own = /atrm/cl_package=>get_devclass_target( lv_devclass ).
      CONCATENATE 'Target of package' lv_devclass 'differs from CTS' INTO lv_msg SEPARATED BY space.
      cl_aunit_assert=>assert_equals( act = lv_own exp = lv_cts msg = lv_msg ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

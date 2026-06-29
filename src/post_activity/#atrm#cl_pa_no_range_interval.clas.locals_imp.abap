*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_trm_pa_no_range_interval DEFINITION.

  PUBLIC SECTION.

    DATA: go_instance TYPE REF TO object READ-ONLY.

    CLASS-DATA: tmp_param_tab TYPE abap_parmbind_tab READ-ONLY,
                tmp_param     LIKE LINE OF tmp_param_tab READ-ONLY,
                tmp_exception TYPE REF TO cx_root READ-ONLY.

  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_trm_pa_no_range_interval IMPLEMENTATION.

ENDCLASS.

CLASS lcl_numberrange_intervals DEFINITION INHERITING FROM lcl_trm_pa_no_range_interval.

  PUBLIC SECTION.

    TYPES: BEGIN OF nr_nriv_line,
             subobject  TYPE /atrm/cl_pa_no_range_interval=>ty_nrsobj,
             nrrangenr  TYPE /atrm/cl_pa_no_range_interval=>ty_nrnr,
             toyear     TYPE /atrm/cl_pa_no_range_interval=>ty_nryear,
             fromnumber TYPE /atrm/cl_pa_no_range_interval=>ty_nrfrom,
             tonumber   TYPE /atrm/cl_pa_no_range_interval=>ty_nrto,
             nrlevel    TYPE /atrm/cl_pa_no_range_interval=>ty_nrlevel,
             externind  TYPE /atrm/cl_pa_no_range_interval=>ty_nrind,
             procind    TYPE /atrm/cl_pa_no_range_interval=>ty_procind,
           END   OF nr_nriv_line,
           nr_interval  TYPE STANDARD TABLE OF nr_nriv_line,
           nr_object    TYPE /atrm/cl_pa_no_range_interval=>ty_nrobj,
           nr_subobject TYPE /atrm/cl_pa_no_range_interval=>ty_nrsobj,
           BEGIN OF nr_option,
             transport         TYPE abap_bool,
             transport_request TYPE trkorr,
           END OF nr_option,
           nr_error TYPE char1.

    CLASS-METHODS create
      IMPORTING !interval  TYPE nr_interval
                !object    TYPE nr_object
                !subobject TYPE nr_subobject OPTIONAL
                !option    TYPE nr_option OPTIONAL
      EXPORTING !error     TYPE nr_error
      RAISING   /atrm/cx_exception.

ENDCLASS.

CLASS lcl_numberrange_intervals IMPLEMENTATION.

  METHOD create.
    DATA: lt_interval  TYPE REF TO data,
          lv_object    TYPE REF TO data,
          lv_subobject TYPE REF TO data,
          ls_option    TYPE REF TO data,
          lv_error     TYPE REF TO data,
          lr_interval  TYPE REF TO data.
    FIELD-SYMBOLS:
      <lt_interval> TYPE STANDARD TABLE,
      <ls_src>      TYPE any,
      <ls_interval> TYPE any,
      <ls_option>   TYPE any.

    create_data lt_interval 'CL_NUMBERRANGE_INTERVALS=>NR_INTERVAL'.
    ASSIGN lt_interval->* TO <lt_interval>.
    CREATE DATA lr_interval LIKE LINE OF <lt_interval>.
    ASSIGN lr_interval->* TO <ls_interval>.
    LOOP AT interval ASSIGNING <ls_src>.
      CLEAR <ls_interval>.
      MOVE-CORRESPONDING <ls_src> TO <ls_interval>.
      APPEND <ls_interval> TO <lt_interval>.
    ENDLOOP.
    create_data lv_object 'CL_NUMBERRANGE_INTERVALS=>NR_OBJECT'.
    MOVE object TO lv_object->*.
    create_data lv_subobject 'CL_NUMBERRANGE_INTERVALS=>NR_SUBOBJECT'.
    MOVE subobject TO lv_subobject->*.
    create_data ls_option 'CL_NUMBERRANGE_INTERVALS=>NR_OPTION'.
    ASSIGN ls_option->* TO <ls_option>.
    MOVE-CORRESPONDING option TO <ls_option>.
    add_param 'INTERVAL'  lt_interval  cl_abap_objectdescr=>exporting.
    add_param 'OBJECT'    lv_object    cl_abap_objectdescr=>exporting.
    add_param 'SUBOBJECT' lv_subobject cl_abap_objectdescr=>exporting.
    add_param 'OPTION'    ls_option    cl_abap_objectdescr=>exporting.
    GET REFERENCE OF error INTO lv_error.
    add_param 'ERROR' lv_error cl_abap_objectdescr=>importing.
    call_static_method 'CL_NUMBERRANGE_INTERVALS' 'CREATE'.
  ENDMETHOD.

ENDCLASS.

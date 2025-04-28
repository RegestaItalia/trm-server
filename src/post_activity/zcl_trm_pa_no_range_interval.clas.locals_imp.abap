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
             subobject  TYPE zcl_trm_pa_no_range_interval=>ty_nrsobj,
             nrrangenr  TYPE zcl_trm_pa_no_range_interval=>ty_nrnr,
             toyear     TYPE zcl_trm_pa_no_range_interval=>ty_nryear,
             fromnumber TYPE zcl_trm_pa_no_range_interval=>ty_nrfrom,
             tonumber   TYPE zcl_trm_pa_no_range_interval=>ty_nrto,
             nrlevel    TYPE zcl_trm_pa_no_range_interval=>ty_nrlevel,
             externind  TYPE zcl_trm_pa_no_range_interval=>ty_nrind,
             procind    TYPE zcl_trm_pa_no_range_interval=>ty_procind,
           END   OF nr_nriv_line,
           nr_interval  TYPE STANDARD TABLE OF nr_nriv_line,
           nr_object    TYPE zcl_trm_pa_no_range_interval=>ty_nrobj,
           nr_subobject TYPE zcl_trm_pa_no_range_interval=>ty_nrsobj,
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
      RAISING   zcx_trm_exception.

ENDCLASS.

CLASS lcl_numberrange_intervals IMPLEMENTATION.

  METHOD create.
    DATA: lt_interval  TYPE REF TO data,
          lv_object    TYPE REF TO data,
          lv_subobject TYPE REF TO data,
          ls_option    TYPE REF TO data,
          lv_error     TYPE REF TO data.
    create_data lt_interval 'CL_NUMBERRANGE_INTERVALS=>NR_INTERVAL'.
    MOVE-CORRESPONDING interval TO lt_interval->*.
    create_data lv_object 'CL_NUMBERRANGE_INTERVALS=>NR_OBJECT'.
    MOVE object TO lv_object->*.
    create_data lv_subobject 'CL_NUMBERRANGE_INTERVALS=>NR_SUBOBJECT'.
    MOVE subobject TO lv_subobject->*.
    create_data ls_option 'CL_NUMBERRANGE_INTERVALS=>NR_OPTION'.
    MOVE-CORRESPONDING option TO ls_option->*.
    add_param 'INTERVAL' lt_interval cl_abap_objectdescr=>exporting.
    add_param 'OBJECT' lv_object cl_abap_objectdescr=>exporting.
    add_param 'SUBOBJECT' lv_subobject cl_abap_objectdescr=>exporting.
    add_param 'OPTION' ls_option cl_abap_objectdescr=>exporting.
    GET REFERENCE OF error INTO lv_error.
    add_param 'ERROR' lv_error cl_abap_objectdescr=>importing.
    call_static_method 'CL_NUMBERRANGE_INTERVALS' 'CREATE'.
  ENDMETHOD.

ENDCLASS.

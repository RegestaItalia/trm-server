REPORT ztrm_migrate.

*******************************************************************
*              TRM - Transport Request Manager                    *
*                  https://trmregistry.com                        *
*******************************************************************
*                                                                 *
* MIT License                                                     *
*                                                                 *
* Copyright (c) 2023 RegestaItalia                                *
*                                                                 *
* Permission is hereby granted, free of charge, to any person     *
* obtaining a copy of this software and associated documentation  *
* files (the "Software"), to deal in the Software without         *
* restriction, including without limitation the rights to use,    *
* copy, modify, merge, publish, distribute, sublicense, and/or    *
* sell copies of the Software, and to permit persons to whom the  *
* Software is furnished to do so, subject to the following        *
* conditions:                                                     *
*                                                                 *
* The above copyright notice and this permission notice shall be  *
* included in all copies or substantial portions of the Software. *
*                                                                 *
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, *
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES *
* OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND        *
* NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT     *
* HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,    *
* WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR   *
* OTHER DEALINGS IN THE SOFTWARE.                                 *
*                                                                 *
*******************************************************************

CLASS lcl_report DEFINITION.

  PUBLIC SECTION.
    METHODS run.
  PRIVATE SECTION.
    CLASS-METHODS find_root_devclass
      IMPORTING
        it_tdevc           TYPE /atrm/cl_core=>tyt_tdevc
      RETURNING
        VALUE(rv_devclass) TYPE devclass.
    CLASS-METHODS find_root_for_single
      IMPORTING
        is_tdevc           TYPE tdevc
        it_tdevc           TYPE /atrm/cl_core=>tyt_tdevc
      RETURNING
        VALUE(rv_devclass) TYPE devclass.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
METHOD find_root_devclass.
    DATA: ls_tdevc      TYPE tdevc,
          lv_root       TYPE devclass,
          lv_first_root TYPE devclass.

    CLEAR rv_devclass.

    IF it_tdevc IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT it_tdevc INTO ls_tdevc.
      lv_root = find_root_for_single(
        is_tdevc = ls_tdevc
        it_tdevc = it_tdevc
      ).

      " If one entry has no root, overall result stays blank
      IF lv_root IS INITIAL.
        CLEAR rv_devclass.
        RETURN.
      ENDIF.

      IF lv_first_root IS INITIAL.
        lv_first_root = lv_root.
      ELSEIF lv_first_root <> lv_root.
        " Not the same hierarchy
        CLEAR rv_devclass.
        RETURN.
      ENDIF.
    ENDLOOP.

    rv_devclass = lv_first_root.
  ENDMETHOD.


  METHOD find_root_for_single.
    DATA: ls_current TYPE tdevc.

    CLEAR rv_devclass.
    ls_current = is_tdevc.

    DO.
      " No devclass -> invalid
      IF ls_current-devclass IS INITIAL.
        CLEAR rv_devclass.
        RETURN.
      ENDIF.

      " Root found: no parent
      IF ls_current-parentcl IS INITIAL.
        rv_devclass = ls_current-devclass.
        RETURN.
      ENDIF.

      " Move to parent within provided hierarchy table
      READ TABLE it_tdevc INTO ls_current
        WITH KEY devclass = ls_current-parentcl.

      " Parent not found in provided table -> no root found
      IF sy-subrc <> 0.
        CLEAR rv_devclass.
        RETURN.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD run.
    DATA:
      confirm_message           TYPE string,
      confirm_answer            TYPE c,
      package                   TYPE /atrm/packages,
      package_data              TYPE /atrm/if_core=>trm_package_data,
      legacy_packages           TYPE /atrm/cl_core=>tyt_trm_package_legacy,
      legacy_packages_aux       TYPE STANDARD TABLE OF ztrm_integrity,
      legacy_packages_integrity TYPE STANDARD TABLE OF ztrm_integrity,
      legacy_package_integrity  LIKE LINE OF legacy_packages_integrity,
      legacy_package            LIKE LINE OF legacy_packages.
    FIELD-SYMBOLS <legacy_package_aux> TYPE ztrm_integrity.
    confirm_message = 'This report can be used to migrate trm data from Z'.
    CONCATENATE confirm_message 'to namespace /ATRM/.' INTO confirm_message SEPARATED BY space.
    CONCATENATE confirm_message 'To use it, keep both the old version of' INTO confirm_message SEPARATED BY space.
    CONCATENATE confirm_message 'trm-server (<6.0.0) and trm-server (>=6.0.0).' INTO confirm_message SEPARATED BY space.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = confirm_message
        text_button_1         = 'Continue'
        icon_button_1         = '@0V@'
        text_button_2         = 'Cancel'
        icon_button_2         = '@0W@'
        display_cancel_button = ' '
      IMPORTING
        answer                = confirm_answer.
    IF confirm_answer = '2'.
      WRITE / 'Migration cancelled by user.'.
      RETURN.
    ENDIF.
    legacy_packages = /atrm/cl_core=>get_installed_packages_legacy( ).
    CHECK legacy_packages[] IS NOT INITIAL.
    LOOP AT legacy_packages INTO legacy_package.
      APPEND INITIAL LINE TO legacy_packages_aux ASSIGNING <legacy_package_aux>.
      <legacy_package_aux>-package_name = legacy_package-name.
      IF legacy_package-registry IS INITIAL.
        <legacy_package_aux>-package_registry = 'public'.
      ELSE.
        <legacy_package_aux>-package_registry = legacy_package-registry.
      ENDIF.
    ENDLOOP.
    SELECT package_name package_registry integrity
      FROM ztrm_integrity
      INTO CORRESPONDING FIELDS OF TABLE legacy_packages_integrity
      FOR ALL ENTRIES IN legacy_packages_aux
      WHERE package_name EQ legacy_packages_aux-package_name AND package_registry EQ legacy_packages_aux-package_registry.
    LOOP AT legacy_packages INTO legacy_package WHERE NOT ( name EQ 'trm-server' AND registry IS INITIAL ) AND NOT ( name EQ 'trm-rest' AND registry IS INITIAL ).
      CLEAR package.
      CLEAR package_data.
      CLEAR legacy_package_integrity.
      package-package_name = legacy_package-name.
      package-package_registry = legacy_package-registry.
      package-timestamp = legacy_package-timestamp.
      package_data-trkorr = legacy_package-transport-trkorr.
      package_data-manifest = legacy_package-xmanifest.
      package-devclass = find_root_devclass( legacy_package-tdevc ).
      CALL TRANSFORMATION id
      SOURCE data = package_data
      RESULT XML package-data.
      READ TABLE legacy_packages_integrity INTO legacy_package_integrity WITH KEY package_name = legacy_package-name package_registry = legacy_package-registry.
      IF sy-subrc EQ 0.
        package-integrity = legacy_package_integrity-integrity.
      ENDIF.
      MODIFY /atrm/packages FROM package.
      WRITE: /, 'Migrated', package-package_name, package-package_registry.
    ENDLOOP.
    COMMIT WORK AND WAIT.
  ENDMETHOD.

ENDCLASS.

DATA report TYPE REF TO lcl_report.

INITIALIZATION.
  CREATE OBJECT report.

START-OF-SELECTION.
  report->run( ).

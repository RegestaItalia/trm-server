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
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.

  METHOD run.
    DATA:
      confirm_message TYPE string,
      confirm_answer  TYPE c,
      package         TYPE /atrm/packages,
      package_data    TYPE /atrm/if_core=>trm_package_data,
      legacy_packages TYPE /atrm/cl_core=>tyt_trm_package,
      legacy_package  LIKE LINE OF legacy_packages.
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
    LOOP AT legacy_packages INTO legacy_package WHERE NOT ( name EQ 'trm-server' AND registry IS INITIAL ) AND NOT ( name EQ 'trm-rest' AND registry IS INITIAL ).
      CLEAR package.
      CLEAR package_data.
      package-package_name = legacy_package-name.
      package-package_registry = legacy_package-registry.
      package-timestamp = legacy_package-timestamp.
      package_data-trkorr = legacy_package-transport-trkorr.
      package_data-manifest = legacy_package-xmanifest.
      SELECT pgmid object obj_name FROM e071 INTO CORRESPONDING FIELDS OF TABLE package_data-e071 WHERE trkorr EQ legacy_package-transport-trkorr.
      CALL TRANSFORMATION id
      SOURCE data = package_data
      RESULT XML package-data.
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

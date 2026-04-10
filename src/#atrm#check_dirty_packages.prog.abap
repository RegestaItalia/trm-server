REPORT /atrm/check_dirty_packages.

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

" This report can be used to check and eventually mark
" TRM packages as dirty.
" A TRM package is considered dirty if one or more
" of its workbench objects are locked in a transport request.
" A dirty TRM package means there is no guarantee that
" the current installed release is identical
" to its declared version

CLASS lcl_report DEFINITION.

  PUBLIC SECTION.
    METHODS run.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_report IMPLEMENTATION.

  METHOD run.
    TYPES: BEGIN OF package_object,
             package_name     TYPE /atrm/package_name,
             package_registry TYPE /atrm/package_registry,
             pgmid            TYPE pgmid,
             object           TYPE trobjtype,
             obj_name         TYPE sobj_name,
           END OF package_object.
    DATA: packages             TYPE /atrm/cl_core=>tyt_trm_package,
          package              LIKE LINE OF packages,
          tadir                TYPE /atrm/cl_utilities=>tyt_tadir,
          package_objects_flat TYPE STANDARD TABLE OF package_object,
          package_object_flat  LIKE LINE OF package_objects_flat,
          tadir_c              TYPE i,
          devclass             TYPE REF TO /atrm/cl_package,
          objects              TYPE scts_tadir,
          object               LIKE LINE OF objects,
          locks                TYPE /atrm/object_lock_t,
          locks_c              TYPE i,
          lock                 LIKE LINE OF locks,
          package_dirty        TYPE /atrm/dirty.
    FIELD-SYMBOLS: <tadir>               TYPE tadir,
                   <package_object_flat> TYPE package_object.
    packages = /atrm/cl_singleton=>get( )->get_installed_packages( ).
    LOOP AT packages INTO package WHERE devclass IS NOT INITIAL.
      CLEAR devclass.
      CREATE OBJECT devclass EXPORTING devclass = package-devclass.
      TRY.
          devclass->get_objects(
            EXPORTING
              incl_sub = 'X'
            IMPORTING
              tadir    = objects
          ).
          LOOP AT objects INTO object.
            APPEND INITIAL LINE TO tadir ASSIGNING <tadir>.
            <tadir>-pgmid = object-pgmid.
            <tadir>-object = object-object.
            <tadir>-obj_name = object-obj_name.
            APPEND INITIAL LINE TO package_objects_flat ASSIGNING <package_object_flat>.
            <package_object_flat>-package_name = package-name.
            <package_object_flat>-package_registry = package-registry.
            <package_object_flat>-pgmid = <tadir>-pgmid.
            <package_object_flat>-object = <tadir>-object.
            <package_object_flat>-obj_name = <tadir>-obj_name.
          ENDLOOP.
        CATCH /atrm/cx_exception.
          WRITE: /, 'Exception on', package-name.
      ENDTRY.
    ENDLOOP.
    IF tadir[] IS NOT INITIAL.
      DESCRIBE TABLE tadir LINES tadir_c.
      locks = /atrm/cl_utilities=>get_objs_locks( objects = tadir ).
      IF locks[] IS NOT INITIAL.
        DESCRIBE TABLE locks LINES locks_c.
        WRITE: /, 'Checked', tadir_c, 'objects.', locks_c, 'locks.'.
        LOOP AT locks INTO lock.
          CLEAR package_object_flat.
          READ TABLE package_objects_flat INTO package_object_flat WITH KEY pgmid = lock-pgmid object = lock-object obj_name = lock-obj_name.
          IF sy-subrc EQ 0.
            CLEAR package_dirty.
            package_dirty-package_name = package_object_flat-package_name.
            package_dirty-package_registry = package_object_flat-package_registry.
            package_dirty-object = package_object_flat-object.
            package_dirty-obj_name = package_object_flat-obj_name.
            DELETE /atrm/dirty FROM package_dirty.
            INSERT /atrm/dirty FROM package_dirty.
            UPDATE /atrm/packages SET dirty = 'X' WHERE package_name EQ package_object_flat-package_name AND package_registry EQ package_object_flat-package_registry.
            WRITE: /, 'Added lock', package_dirty-package_name, package_dirty-package_registry, package_dirty-object, package_dirty-obj_name.
          ELSE.
            WRITE: /, 'Cannot add lock', lock-object, lock-obj_name, 'cannot find package'.
          ENDIF.
        ENDLOOP.
        COMMIT WORK.
      ELSE.
        WRITE: /, 'Checked', tadir_c, 'objects. No locks.'.
      ENDIF.
    ELSE.
      WRITE / 'No packages installed.'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

DATA report TYPE REF TO lcl_report.

INITIALIZATION.
  CREATE OBJECT report.

START-OF-SELECTION.
  report->run( ).

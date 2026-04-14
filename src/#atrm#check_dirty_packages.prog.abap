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
" of its workbench objects are found in transport(s)
" after its package publish/install date

CLASS lcl_report DEFINITION.

  PUBLIC SECTION.
    METHODS run.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_report IMPLEMENTATION.

  METHOD run.
    TYPES: BEGIN OF package_object,
             package_name      TYPE /atrm/package_name,
             package_registry  TYPE /atrm/package_registry,
             transport_as4date TYPE as4date,
             transport_as4time TYPE as4time,
             pgmid             TYPE pgmid,
             object            TYPE trobjtype,
             obj_name          TYPE trobj_name,
           END OF package_object,
           BEGIN OF dirty_transport,
             trkorr   TYPE trkorr,
             strkorr  TYPE strkorr,
             pgmid    TYPE pgmid,
             object   TYPE trobjtype,
             obj_name TYPE trobj_name,
           END OF dirty_transport.
    DATA: packages                  TYPE /atrm/packages_t,
          package                   LIKE LINE OF packages,
          devclass                  TYPE REF TO /atrm/cl_package,
          objects                   TYPE scts_tadir,
          object                    LIKE LINE OF objects,
          tadir                     TYPE /atrm/cl_utilities=>tyt_tadir,
          package_objects_flat      TYPE STANDARD TABLE OF package_object,
          package_objects_flat_line LIKE LINE OF package_objects_flat,
          e071_dirty                TYPE STANDARD TABLE OF dirty_transport,
          e071_dirty_line           LIKE LINE OF e071_dirty,
          dirty                     TYPE /atrm/dirty.
    FIELD-SYMBOLS: <tadir>               TYPE tadir,
                   <package_object_flat> TYPE package_object.
    packages = /atrm/cl_singleton=>get( )->get_installed_packages( ).
    IF packages[] IS NOT INITIAL.
      LOOP AT packages INTO package WHERE devclass IS NOT INITIAL.
        CLEAR devclass.
        CLEAR objects[].
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
              <package_object_flat>-package_name = package-package_name.
              <package_object_flat>-package_registry = package-package_registry.
              <package_object_flat>-transport_as4date = package-as4date.
              <package_object_flat>-transport_as4time = package-as4time.
              <package_object_flat>-pgmid = <tadir>-pgmid.
              <package_object_flat>-object = <tadir>-object.
              <package_object_flat>-obj_name = <tadir>-obj_name.
            ENDLOOP.
          CATCH /atrm/cx_exception.
            WRITE: /, 'Exception on', package-package_name, package-package_registry.
        ENDTRY.
      ENDLOOP.
      IF tadir[] IS NOT INITIAL.
        SELECT e070~trkorr e070~strkorr e071~pgmid e071~object e071~obj_name
        FROM e070
        INNER JOIN e071 ON e071~trkorr = e070~trkorr
        INTO CORRESPONDING FIELDS OF TABLE e071_dirty
        FOR ALL ENTRIES IN package_objects_flat
        WHERE ( e070~trfunction EQ 'K' OR e070~trfunction EQ 'S' OR e070~trfunction EQ 'R' )
          AND e071~pgmid EQ package_objects_flat-pgmid
          AND e071~object EQ package_objects_flat-object
          AND e071~obj_name EQ package_objects_flat-obj_name
          AND ( e070~as4date GT package_objects_flat-transport_as4date OR (
                e070~as4date EQ package_objects_flat-transport_as4date AND
                 e070~as4time GT package_objects_flat-transport_as4time
               ) ).
        IF e071_dirty[] IS NOT INITIAL.
          LOOP AT e071_dirty INTO e071_dirty_line.
            " only write header transports
            READ TABLE e071_dirty TRANSPORTING NO FIELDS WITH KEY trkorr = e071_dirty_line-strkorr pgmid = e071_dirty_line-pgmid object = e071_dirty_line-object obj_name = e071_dirty_line-obj_name.
            CHECK sy-subrc NE 0.
            LOOP AT package_objects_flat INTO package_objects_flat_line WHERE pgmid EQ e071_dirty_line-pgmid AND object EQ e071_dirty_line-object AND obj_name EQ e071_dirty_line-obj_name.
              CLEAR dirty.
              dirty-package_name = package_objects_flat_line-package_name.
              dirty-package_registry = package_objects_flat_line-package_registry.
              dirty-trkorr = e071_dirty_line-trkorr.
              dirty-pgmid = e071_dirty_line-pgmid.
              dirty-object = e071_dirty_line-object.
              dirty-obj_name = e071_dirty_line-obj_name.
              DELETE /atrm/dirty FROM dirty.
              INSERT /atrm/dirty FROM dirty.
              UPDATE /atrm/packages SET dirty = 'X' WHERE package_name EQ package_objects_flat_line-package_name AND package_registry EQ package_objects_flat_line-package_registry.
              WRITE: /, 'Added dirty', package_objects_flat_line-package_name, package_objects_flat_line-package_registry, e071_dirty_line-trkorr, e071_dirty_line-object, e071_dirty_line-obj_name.
            ENDLOOP.
          ENDLOOP.
          COMMIT WORK.
        ELSE.
          WRITE / 'No dirty packages.'.
        ENDIF.
      ELSE.
        WRITE / 'No objects to check.'.
      ENDIF.
    ELSE.
      WRITE / 'No TRM packages installed.'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

DATA report TYPE REF TO lcl_report.

INITIALIZATION.
  CREATE OBJECT report.

START-OF-SELECTION.
  report->run( ).

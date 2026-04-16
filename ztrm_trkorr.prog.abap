REPORT ztrm_trkorr.

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
    " According to NOTE 1674286, to change the transport request number we must perform an update to table E070L
    " This report is only meant to be used in DEMOSYSTEM A4H during development, specifically in the publish phase of trm-server and trm-rest
    " For trm-server: run report once
    " For trm-rest: run report twice
    DATA: current_trkorr TYPE trkorr,
          new_number     TYPE trkorr.
    SELECT SINGLE trkorr FROM e070l INTO current_trkorr WHERE lastnum EQ 'TRKORR'.
    IF current_trkorr LT 'A4HK999997'.
      new_number = 'A4HK999997'.
    ELSE.
      new_number = 'A4HK9A0000'.
    ENDIF.
    UPDATE e070l SET trkorr = new_number WHERE lastnum EQ 'TRKORR'.
    COMMIT WORK AND WAIT.
    WRITE: / 'Transport request number updated to:', new_number.
  ENDMETHOD.

ENDCLASS.

DATA report TYPE REF TO lcl_report.

INITIALIZATION.
  CREATE OBJECT report.

START-OF-SELECTION.
  report->run( ).

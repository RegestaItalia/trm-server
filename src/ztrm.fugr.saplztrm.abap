*******************************************************************
*              TRM - Transport Request Manager                    *
*                  https://trmregistry.com                        *
*                                                                 *
* Function module documentation:                                  *
* https://docs.trmregistry.com/#/server/docs/rfcFunctions         *
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
*   System-defined Include-files.                                 *
*******************************************************************
  INCLUDE LZTRMTOP.                          " Global Declarations
  INCLUDE LZTRMUXX.                          " Function Modules

*******************************************************************
*   User-defined Include-files (if necessary).                    *
*******************************************************************
* INCLUDE LZTRMF...                          " Subroutines
* INCLUDE LZTRMO...                          " PBO-Modules
* INCLUDE LZTRMI...                          " PAI-Modules
* INCLUDE LZTRME...                          " Events
* INCLUDE LZTRMP...                          " Local class implement.
* INCLUDE LZTRMT99.                          " ABAP Unit tests
  INCLUDE LZTRMF00                                . " subprograms
  INCLUDE LZTRMI00                                . " PAI modules
  INCLUDE LSVIMFXX                                . " subprograms
  INCLUDE LSVIMOXX                                . " PBO modules
  INCLUDE LSVIMIXX                                . " PAI modules

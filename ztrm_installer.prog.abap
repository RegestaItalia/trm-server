REPORT ztrm_installer.

TABLES sscrfields.

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

TYPES: BEGIN OF ty_vscan_f4,
         profile TYPE vscan_prof-profile,
         text    TYPE vscan_proft-text,
       END OF ty_vscan_f4.
DATA: vscan                        TYPE STANDARD TABLE OF ty_vscan_f4,
      json_supported               TYPE flag,
      vscan_profile_supported      TYPE flag,
      install_certificates_visible TYPE flag,
      suppress_certificate_command TYPE flag.

CONSTANTS: base_url      TYPE string VALUE 'https://trmregistry.com/registry',
           server_trkorr TYPE trkorr VALUE 'TRMK900000',
           rest_trkorr   TYPE trkorr VALUE 'TRMK900001'.

SELECTION-SCREEN BEGIN OF BLOCK sc_header WITH FRAME TITLE sc_titl1.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN COMMENT 1(77) sc_txt1.
  SELECTION-SCREEN COMMENT /1(77) sc_txt2.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN COMMENT /1(77) sc_txt3.
  SELECTION-SCREEN COMMENT /1(77) sc_txt4.
  SELECTION-SCREEN COMMENT /1(77) sc_txt5.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN COMMENT /1(77) sc_txt6.
  SELECTION-SCREEN COMMENT /1(77) sc_txt7.
SELECTION-SCREEN END OF BLOCK sc_header.

SELECTION-SCREEN SKIP.

PARAMETERS:
  p_srv  TYPE c AS CHECKBOX DEFAULT 'X' USER-COMMAND srv,
  p_rest TYPE c AS CHECKBOX DEFAULT ' ' USER-COMMAND rest.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN COMMENT /1(77) json_err MODIF ID njs.
SELECTION-SCREEN COMMENT /1(1) json_spc MODIF ID njs.

SELECTION-SCREEN: BEGIN OF TABBED BLOCK psel FOR 15 LINES,
TAB (20) offline USER-COMMAND tab2 DEFAULT SCREEN 200,
TAB (20) online USER-COMMAND tab1 DEFAULT SCREEN 100 MODIF ID jsn,
END OF BLOCK psel.

SELECTION-SCREEN BEGIN OF SCREEN 100 AS SUBSCREEN.
  SELECTION-SCREEN COMMENT /1(77) cert_err MODIF ID crt.
  SELECTION-SCREEN PUSHBUTTON /1(25) cert_btn USER-COMMAND cert MODIF ID crt.
  SELECTION-SCREEN COMMENT /1(1) cert_spc MODIF ID crt.

  SELECTION-SCREEN BEGIN OF BLOCK sc_serv WITH FRAME TITLE sc_titl2.
    PARAMETERS:
      p_id     TYPE strustssl-applic DEFAULT 'ANONYM',
      p_vscan  TYPE c AS CHECKBOX DEFAULT 'X' USER-COMMAND vscan,
      p_vscanp TYPE vscan_profile DEFAULT '/SIHTTP/HTTP_DOWNLOAD'.
  SELECTION-SCREEN END OF BLOCK sc_serv.

  SELECTION-SCREEN SKIP.

  SELECTION-SCREEN BEGIN OF BLOCK sc_proxy WITH FRAME TITLE sc_titl3.
    PARAMETERS:
      p_proxy TYPE string LOWER CASE,
      p_pport TYPE string LOWER CASE,
      p_puser TYPE string LOWER CASE,
      p_ppwd  TYPE string LOWER CASE.
  SELECTION-SCREEN END OF BLOCK sc_proxy.
SELECTION-SCREEN END OF SCREEN 100.

SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK sc_other WITH FRAME TITLE TEXT-001.
    PARAMETERS:
      p_lserv TYPE rlgrap-filename LOWER CASE,
      p_lrest TYPE rlgrap-filename LOWER CASE.
  SELECTION-SCREEN END OF BLOCK sc_other.
SELECTION-SCREEN END OF SCREEN 200.

CLASS lcl_strust DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS add_certificate
      IMPORTING
        application  TYPE ssfappl
        certificate  TYPE xstring
        password     TYPE string OPTIONAL
      RETURNING
        VALUE(error) TYPE string.
  PRIVATE SECTION.
    CLASS-METHODS get_system_error
      IMPORTING
        fallback     TYPE string
      RETURNING
        VALUE(error) TYPE string.
ENDCLASS.

CLASS lcl_strust IMPLEMENTATION.
  METHOD add_certificate.
    CONSTANTS ssl_client_context TYPE psecontext VALUE 'SSLC'.
    DATA: pse_name        TYPE ssfpsename,
          pse_id          TYPE ssfid,
          profile_file    TYPE localfile,
          pse_profile     TYPE ssfpab,
          pse_password    TYPE ssfpabpw,
          temporary_file  TYPE localfile,
          distribute      TYPE ssfflag,
          credential_name TYPE REF TO data,
          is_locked       TYPE flag,
          authority_error TYPE REF TO cx_root.
    FIELD-SYMBOLS <credential_name> TYPE any.

    IF certificate IS INITIAL.
      error = 'Cannot install an empty certificate'.
      RETURN.
    ENDIF.
    pse_password = password.

    TRY.
        CALL METHOD ('CL_ABAP_PSE')=>('AUTHORITY_CHECK')
          EXPORTING
            iv_context  = ssl_client_context
            iv_applic   = application
            iv_activity = '02'.
      CATCH cx_sy_dyn_call_error.
        " CL_ABAP_PSE is not available on older SAP releases. The
        " SSFPSE_* function modules still enforce their own authorization.
      CATCH cx_root INTO authority_error.
        error = authority_error->get_text( ).
        RETURN.
    ENDTRY.

    CALL FUNCTION 'SSFPSE_FILENAME'
      EXPORTING
        context       = ssl_client_context
        applic        = application
      IMPORTING
        psename       = pse_name
        distrib       = distribute
        profile       = profile_file
      EXCEPTIONS
        pse_not_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      error = get_system_error( 'SSL client PSE was not found' ).
      RETURN.
    ENDIF.
    pse_profile = profile_file.

    CALL FUNCTION 'SSFPSE_ENQUEUE'
      EXPORTING
        psename         = pse_name
      EXCEPTIONS
        database_failed = 1
        foreign_lock    = 2
        internal_error  = 3
        OTHERS          = 4.
    IF sy-subrc <> 0.
      error = get_system_error( 'SSL client PSE could not be locked' ).
      RETURN.
    ENDIF.
    is_locked = 'X'.

    DO 1 TIMES.
      CALL FUNCTION 'SSFPSE_LOAD'
        EXPORTING
          psename           = pse_name
        IMPORTING
          id                = pse_id
          fname             = temporary_file
        EXCEPTIONS
          authority_missing = 1
          database_failed   = 2
          file_write_failed = 3
          OTHERS            = 4.
      IF sy-subrc <> 0.
        error = get_system_error( 'SSL client PSE could not be loaded' ).
        EXIT.
      ENDIF.

      IF temporary_file IS NOT INITIAL.
        pse_profile = temporary_file.
      ENDIF.

      CALL FUNCTION 'SSFC_PUT_CERTIFICATE'
        EXPORTING
          profile             = pse_profile
          profilepw           = pse_password
          certificate         = certificate
        EXCEPTIONS
          ssf_krn_error       = 1
          ssf_krn_nomemory    = 2
          ssf_krn_nossflib    = 3
          ssf_krn_invalid_par = 4
          ssf_krn_certexists  = 5
          OTHERS              = 6.
      IF sy-subrc = 5.
        CLEAR error.
        EXIT.
      ELSEIF sy-subrc <> 0.
        error = get_system_error( 'Certificate could not be added to the PSE' ).
        EXIT.
      ENDIF.

      CALL FUNCTION 'SSFPSE_STORE'
        EXPORTING
          fname             = temporary_file
          psepin            = pse_password
          psename           = pse_name
          id                = pse_id
          b_newdn           = abap_false
          b_distribute      = distribute
        EXCEPTIONS
          file_load_failed  = 1
          storing_failed    = 2
          authority_missing = 3
          OTHERS            = 4.
      IF sy-subrc <> 0.
        error = get_system_error( 'SSL client PSE could not be saved' ).
        EXIT.
      ENDIF.

      " ICM_CREDNAME (and the CRED_NAME parameter) don't exist on older
      " releases: there, refresh all SSL credentials (GLOBAL = 1) without a name
      TRY.
          CREATE DATA credential_name TYPE ('ICM_CREDNAME').
          ASSIGN credential_name->* TO <credential_name>.
          <credential_name> = pse_name.
          CALL FUNCTION 'ICM_SSL_PSE_CHANGED'
            EXPORTING
              global              = 1
              cred_name           = <credential_name>
            EXCEPTIONS
              icm_op_failed       = 1
              icm_get_serv_failed = 2
              icm_auth_failed     = 3
              OTHERS              = 4.
        CATCH cx_sy_create_data_error cx_sy_dyn_call_error.
          CALL FUNCTION 'ICM_SSL_PSE_CHANGED'
            EXPORTING
              global              = 1
            EXCEPTIONS
              icm_op_failed       = 1
              icm_get_serv_failed = 2
              icm_auth_failed     = 3
              OTHERS              = 4.
      ENDTRY.
      IF sy-subrc <> 0.
        error = get_system_error( 'Certificate was saved, but ICM could not be refreshed' ).
      ENDIF.
    ENDDO.

    IF temporary_file IS NOT INITIAL.
      TRY.
          DELETE DATASET temporary_file.
        CATCH cx_sy_file_open cx_sy_file_authority.
          IF error IS INITIAL.
            error = 'Certificate was saved, but the temporary PSE file could not be deleted'.
          ENDIF.
      ENDTRY.
    ENDIF.

    IF is_locked = 'X'.
      CALL FUNCTION 'SSFPSE_DEQUEUE'
        EXPORTING
          psename         = pse_name
        EXCEPTIONS
          database_failed = 1
          foreign_lock    = 2
          internal_error  = 3
          OTHERS          = 4.
      IF sy-subrc <> 0 AND error IS INITIAL.
        error = get_system_error( 'SSL client PSE could not be unlocked' ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_system_error.
    IF sy-msgid IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO error.
    ENDIF.
    IF error IS INITIAL.
      error = fallback.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

* SHA-512 in plain ABAP, for releases without CL_ABAP_MESSAGE_DIGEST (e.g. 7.0).
* 64-bit words are kept as x LENGTH 8 for bit operations; rotations and
* additions work on four 16-bit limbs held in type i (no int8 before 7.50).
CLASS lcl_sha512 DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES ty_word TYPE x LENGTH 8.
    CLASS-METHODS hash_b64
      IMPORTING
        data        TYPE xstring
      RETURNING
        VALUE(hash) TYPE string.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_limbs,
             l0 TYPE i,
             l1 TYPE i,
             l2 TYPE i,
             l3 TYPE i,
           END OF ty_limbs.
    CLASS-DATA constants TYPE xstring.
    CLASS-METHODS split
      IMPORTING
        word         TYPE ty_word
      RETURNING
        VALUE(limbs) TYPE ty_limbs.
    CLASS-METHODS join
      IMPORTING
        limbs       TYPE ty_limbs
      RETURNING
        VALUE(word) TYPE ty_word.
    CLASS-METHODS accumulate
      IMPORTING
        word TYPE ty_word
      CHANGING
        sum  TYPE ty_limbs.
    CLASS-METHODS rotate
      IMPORTING
        limbs       TYPE ty_limbs
        bits        TYPE i
        shift_only  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(word) TYPE ty_word.
    CLASS-METHODS sigma
      IMPORTING
        word         TYPE ty_word
        r1           TYPE i
        r2           TYPE i
        r3           TYPE i
        r3_is_shift  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE ty_word.
    CLASS-METHODS base64
      IMPORTING
        data           TYPE xstring
      RETURNING
        VALUE(encoded) TYPE string.
    CLASS-METHODS load_constants.
ENDCLASS.

CLASS lcl_sha512 IMPLEMENTATION.
  METHOD hash_b64.
    DATA: message    TYPE xstring,
          padding    TYPE xstring,
          bit_length TYPE x LENGTH 4,
          byte_80    TYPE x LENGTH 1 VALUE '80',
          byte_00    TYPE x LENGTH 1 VALUE '00',
          length     TYPE i,
          zeros      TYPE i,
          blocks     TYPE i,
          offset     TYPE i,
          t          TYPE i,
          w_off      TYPE i,
          schedule   TYPE x LENGTH 640,
          h          TYPE x LENGTH 64,
          digest     TYPE xstring,
          a          TYPE ty_word,
          b          TYPE ty_word,
          c          TYPE ty_word,
          d          TYPE ty_word,
          e          TYPE ty_word,
          f          TYPE ty_word,
          g          TYPE ty_word,
          hh         TYPE ty_word,
          w1         TYPE ty_word,
          w2         TYPE ty_word,
          s0         TYPE ty_word,
          s1         TYPE ty_word,
          ch         TYPE ty_word,
          maj        TYPE ty_word,
          not_e      TYPE ty_word,
          k          TYPE ty_word,
          wt         TYPE ty_word,
          t1         TYPE ty_word,
          sum        TYPE ty_limbs,
          i          TYPE i,
          block      TYPE i,
          percentage TYPE i,
          shown      TYPE i VALUE -1,
          percent_c  TYPE c LENGTH 3,
          progress   TYPE string.

    IF constants IS INITIAL.
      load_constants( ).
    ENDIF.

    " padding: 0x80, zeros up to 112 mod 128, 128-bit big-endian bit length
    length = xstrlen( data ).
    zeros = ( 111 - length MOD 128 ) MOD 128.
    CONCATENATE padding byte_80 INTO padding IN BYTE MODE.
    DO zeros TIMES.
      CONCATENATE padding byte_00 INTO padding IN BYTE MODE.
    ENDDO.
    DO 12 TIMES.
      CONCATENATE padding byte_00 INTO padding IN BYTE MODE.
    ENDDO.
    bit_length = length * 8.
    CONCATENATE data padding bit_length INTO message IN BYTE MODE.

    h = constants+640(64).
    blocks = xstrlen( message ) / 128.
    offset = 0.
    DO blocks TIMES.
      " this is slow (no 64-bit integers before 7.50): show progress, at most once per percent
      block = sy-index.
      percentage = ( block * 100 ) DIV blocks.
      IF percentage <> shown.
        shown = percentage.
        percent_c = percentage.
        CONDENSE percent_c.
        CONCATENATE 'Calculating release checksum (SHA512 in ABAP):' percent_c INTO progress SEPARATED BY space.
        CONCATENATE progress '%' INTO progress.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = percentage
            text       = progress.
      ENDIF.
      schedule+0(128) = message+offset(128).
      t = 16.
      WHILE t < 80.
        w_off = ( t - 2 ) * 8.
        w1 = schedule+w_off(8).
        w_off = ( t - 15 ) * 8.
        w2 = schedule+w_off(8).
        s1 = sigma( word = w1 r1 = 19 r2 = 61 r3 = 6 r3_is_shift = abap_true ).
        s0 = sigma( word = w2 r1 = 1 r2 = 8 r3 = 7 r3_is_shift = abap_true ).
        CLEAR sum.
        accumulate( EXPORTING word = s1 CHANGING sum = sum ).
        accumulate( EXPORTING word = s0 CHANGING sum = sum ).
        w_off = ( t - 7 ) * 8.
        w1 = schedule+w_off(8).
        accumulate( EXPORTING word = w1 CHANGING sum = sum ).
        w_off = ( t - 16 ) * 8.
        w1 = schedule+w_off(8).
        accumulate( EXPORTING word = w1 CHANGING sum = sum ).
        w_off = t * 8.
        schedule+w_off(8) = join( sum ).
        t = t + 1.
      ENDWHILE.

      a = h+0(8).
      b = h+8(8).
      c = h+16(8).
      d = h+24(8).
      e = h+32(8).
      f = h+40(8).
      g = h+48(8).
      hh = h+56(8).

      t = 0.
      WHILE t < 80.
        w_off = t * 8.
        k = constants+w_off(8).
        wt = schedule+w_off(8).
        s1 = sigma( word = e r1 = 14 r2 = 18 r3 = 41 ).
        not_e = BIT-NOT e.
        ch = ( e BIT-AND f ) BIT-XOR ( not_e BIT-AND g ).
        CLEAR sum.
        accumulate( EXPORTING word = hh CHANGING sum = sum ).
        accumulate( EXPORTING word = s1 CHANGING sum = sum ).
        accumulate( EXPORTING word = ch CHANGING sum = sum ).
        accumulate( EXPORTING word = k CHANGING sum = sum ).
        accumulate( EXPORTING word = wt CHANGING sum = sum ).
        t1 = join( sum ).
        s0 = sigma( word = a r1 = 28 r2 = 34 r3 = 39 ).
        maj = ( a BIT-AND b ) BIT-XOR ( a BIT-AND c ) BIT-XOR ( b BIT-AND c ).
        hh = g.
        g = f.
        f = e.
        CLEAR sum.
        accumulate( EXPORTING word = d CHANGING sum = sum ).
        accumulate( EXPORTING word = t1 CHANGING sum = sum ).
        e = join( sum ).
        d = c.
        c = b.
        b = a.
        CLEAR sum.
        accumulate( EXPORTING word = t1 CHANGING sum = sum ).
        accumulate( EXPORTING word = s0 CHANGING sum = sum ).
        accumulate( EXPORTING word = maj CHANGING sum = sum ).
        a = join( sum ).
        t = t + 1.
      ENDWHILE.

      i = 0.
      DO 8 TIMES.
        w_off = i * 8.
        CASE i.
          WHEN 0. w1 = a.
          WHEN 1. w1 = b.
          WHEN 2. w1 = c.
          WHEN 3. w1 = d.
          WHEN 4. w1 = e.
          WHEN 5. w1 = f.
          WHEN 6. w1 = g.
          WHEN 7. w1 = hh.
        ENDCASE.
        w2 = h+w_off(8).
        CLEAR sum.
        accumulate( EXPORTING word = w1 CHANGING sum = sum ).
        accumulate( EXPORTING word = w2 CHANGING sum = sum ).
        h+w_off(8) = join( sum ).
        i = i + 1.
      ENDDO.

      offset = offset + 128.
    ENDDO.

    digest = h.
    hash = base64( digest ).
  ENDMETHOD.

  METHOD split.
    DATA half TYPE x LENGTH 2.
    half = word+0(2).
    limbs-l0 = half.
    half = word+2(2).
    limbs-l1 = half.
    half = word+4(2).
    limbs-l2 = half.
    half = word+6(2).
    limbs-l3 = half.
  ENDMETHOD.

  METHOD join.
    DATA: normalized TYPE ty_limbs,
          carry      TYPE i,
          half       TYPE x LENGTH 2.
    normalized = limbs.
    carry = normalized-l3 DIV 65536.
    normalized-l3 = normalized-l3 MOD 65536.
    normalized-l2 = normalized-l2 + carry.
    carry = normalized-l2 DIV 65536.
    normalized-l2 = normalized-l2 MOD 65536.
    normalized-l1 = normalized-l1 + carry.
    carry = normalized-l1 DIV 65536.
    normalized-l1 = normalized-l1 MOD 65536.
    normalized-l0 = ( normalized-l0 + carry ) MOD 65536.
    half = normalized-l0.
    word+0(2) = half.
    half = normalized-l1.
    word+2(2) = half.
    half = normalized-l2.
    word+4(2) = half.
    half = normalized-l3.
    word+6(2) = half.
  ENDMETHOD.

  METHOD accumulate.
    DATA limbs TYPE ty_limbs.
    limbs = split( word ).
    sum-l0 = sum-l0 + limbs-l0.
    sum-l1 = sum-l1 + limbs-l1.
    sum-l2 = sum-l2 + limbs-l2.
    sum-l3 = sum-l3 + limbs-l3.
  ENDMETHOD.

  METHOD rotate.
    " rotate (or shift) right a 64-bit word given as limbs, l0 most significant
    DATA: moved    TYPE ty_limbs,
          result   TYPE ty_limbs,
          wrap     TYPE i,
          divisor  TYPE i,
          factor   TYPE i,
          bit_part TYPE i,
          quarter  TYPE i.

    quarter = bits DIV 16.
    CASE quarter.
      WHEN 0.
        moved = limbs.
      WHEN 1.
        moved-l0 = limbs-l3.
        moved-l1 = limbs-l0.
        moved-l2 = limbs-l1.
        moved-l3 = limbs-l2.
      WHEN 2.
        moved-l0 = limbs-l2.
        moved-l1 = limbs-l3.
        moved-l2 = limbs-l0.
        moved-l3 = limbs-l1.
      WHEN 3.
        moved-l0 = limbs-l1.
        moved-l1 = limbs-l2.
        moved-l2 = limbs-l3.
        moved-l3 = limbs-l0.
    ENDCASE.

    " shift_only is used with bits < 16 only (SHA-512 shifts by 6 and 7)
    IF shift_only = abap_true.
      wrap = 0.
    ELSE.
      wrap = moved-l3.
    ENDIF.

    bit_part = bits MOD 16.
    IF bit_part = 0.
      result = moved.
      IF shift_only = abap_true.
        CLEAR result-l0.
      ENDIF.
    ELSE.
      divisor = 1.
      DO bit_part TIMES.
        divisor = divisor * 2.
      ENDDO.
      factor = 65536 DIV divisor.
      result-l0 = moved-l0 DIV divisor + ( wrap MOD divisor ) * factor.
      result-l1 = moved-l1 DIV divisor + ( moved-l0 MOD divisor ) * factor.
      result-l2 = moved-l2 DIV divisor + ( moved-l1 MOD divisor ) * factor.
      result-l3 = moved-l3 DIV divisor + ( moved-l2 MOD divisor ) * factor.
    ENDIF.
    word = join( result ).
  ENDMETHOD.

  METHOD sigma.
    DATA: limbs TYPE ty_limbs,
          x1    TYPE ty_word,
          x2    TYPE ty_word,
          x3    TYPE ty_word.
    limbs = split( word ).
    x1 = rotate( limbs = limbs bits = r1 ).
    x2 = rotate( limbs = limbs bits = r2 ).
    x3 = rotate( limbs = limbs bits = r3 shift_only = r3_is_shift ).
    result = x1 BIT-XOR x2 BIT-XOR x3.
  ENDMETHOD.

  METHOD base64.
    DATA alphabet TYPE c LENGTH 64
      VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'.
    DATA: length TYPE i,
          offset TYPE i,
          rest   TYPE i,
          group  TYPE x LENGTH 3,
          value  TYPE i,
          index  TYPE i,
          chars  TYPE c LENGTH 4.

    length = xstrlen( data ).
    offset = 0.
    WHILE offset < length.
      rest = length - offset.
      CLEAR group.
      IF rest >= 3.
        group = data+offset(3).
      ELSE.
        group+0(rest) = data+offset(rest).
      ENDIF.
      value = group.
      index = value DIV 262144.
      chars+0(1) = alphabet+index(1).
      index = ( value DIV 4096 ) MOD 64.
      chars+1(1) = alphabet+index(1).
      index = ( value DIV 64 ) MOD 64.
      chars+2(1) = alphabet+index(1).
      index = value MOD 64.
      chars+3(1) = alphabet+index(1).
      IF rest = 1.
        chars+2(2) = '=='.
      ELSEIF rest = 2.
        chars+3(1) = '='.
      ENDIF.
      CONCATENATE encoded chars INTO encoded.
      offset = offset + 3.
    ENDWHILE.
  ENDMETHOD.

  METHOD load_constants.
    DATA hex TYPE string.
    " 80 round constants (640 bytes), then the 8 initial hash values (64 bytes)
    CONCATENATE
      '428A2F98D728AE227137449123EF65CDB5C0FBCFEC4D3B2FE9B5DBA58189DBBC'
      '3956C25BF348B53859F111F1B605D019923F82A4AF194F9BAB1C5ED5DA6D8118'
      'D807AA98A303024212835B0145706FBE243185BE4EE4B28C550C7DC3D5FFB4E2'
      '72BE5D74F27B896F80DEB1FE3B1696B19BDC06A725C71235C19BF174CF692694'
      'E49B69C19EF14AD2EFBE4786384F25E30FC19DC68B8CD5B5240CA1CC77AC9C65'
      '2DE92C6F592B02754A7484AA6EA6E4835CB0A9DCBD41FBD476F988DA831153B5'
      '983E5152EE66DFABA831C66D2DB43210B00327C898FB213FBF597FC7BEEF0EE4'
      'C6E00BF33DA88FC2D5A79147930AA72506CA6351E003826F142929670A0E6E70'
      '27B70A8546D22FFC2E1B21385C26C9264D2C6DFC5AC42AED53380D139D95B3DF'
      '650A73548BAF63DE766A0ABB3C77B2A881C2C92E47EDAEE692722C851482353B'
      'A2BFE8A14CF10364A81A664BBC423001C24B8B70D0F89791C76C51A30654BE30'
      'D192E819D6EF5218D69906245565A910F40E35855771202A106AA07032BBD1B8'
      '19A4C116B8D2D0C81E376C085141AB532748774CDF8EEB9934B0BCB5E19B48A8'
      '391C0CB3C5C95A634ED8AA4AE3418ACB5B9CCA4F7763E373682E6FF3D6B2B8A3'
      '748F82EE5DEFB2FC78A5636F43172F6084C87814A1F0AB728CC702081A6439EC'
      '90BEFFFA23631E28A4506CEBDE82BDE9BEF9A3F7B2C67915C67178F2E372532B'
      'CA273ECEEA26619CD186B8C721C0C207EADA7DD6CDE0EB1EF57D4F7FEE6ED178'
      '06F067AA72176FBA0A637DC5A2C898A6113F9804BEF90DAE1B710B35131C471B'
      '28DB77F523047D8432CAAB7B40C724933C9EBE0A15C9BEBC431D67C49C100D4C'
      '4CC5D4BECB3E42B6597F299CFC657E2A5FCB6FAB3AD6FAEC6C44198C4A475817'
      '6A09E667F3BCC908BB67AE8584CAA73B3C6EF372FE94F82BA54FF53A5F1D36F1'
      '510E527FADE682D19B05688C2B3E6C1F1F83D9ABFB41BD6B5BE0CD19137E2179'
      INTO hex.
    constants = hex.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_report DEFINITION.

  PUBLIC SECTION.
    METHODS run.
    CLASS-METHODS raise_error
      IMPORTING
        message TYPE string.
    CLASS-METHODS get_versions
      EXPORTING server TYPE string
                rest   TYPE string.
    CLASS-METHODS check_json_parser
      RETURNING VALUE(exists) TYPE flag.
    CLASS-METHODS check_vscan_profile
      RETURNING VALUE(exists) TYPE abap_bool.
    CLASS-METHODS check_registry_certificates
      EXPORTING
        missing         TYPE flag
        technical_error TYPE string.
    CLASS-METHODS install_registry_certificates
      RETURNING VALUE(installed) TYPE flag.
  PRIVATE SECTION.
    TYPES: ty_package_name TYPE devclass,
           ty_package_tab  TYPE STANDARD TABLE OF ty_package_name WITH DEFAULT KEY,
           BEGIN OF ty_pkg_node,
             original TYPE devclass,
             package  TYPE devclass,
             parent   TYPE devclass,
             level    TYPE i,
           END OF ty_pkg_node,
           ty_pkg_node_tab TYPE STANDARD TABLE OF ty_pkg_node WITH DEFAULT KEY,
           BEGIN OF ty_registry_release,
             download_link TYPE string,
             checksum      TYPE string,
           END OF ty_registry_release,
           t_tpstdout TYPE STANDARD TABLE OF tpstdout WITH DEFAULT KEY.
    METHODS get_client
      IMPORTING
        with_base_url TYPE c DEFAULT 'X'
        url           TYPE string OPTIONAL
          PREFERRED PARAMETER url
      RETURNING
        VALUE(client) TYPE REF TO if_http_client.
    METHODS execute_http_request
      IMPORTING
        client TYPE REF TO if_http_client
      EXPORTING
        ok     TYPE flag
        error  TYPE string.
    METHODS download_release
      IMPORTING
        name     TYPE string
      EXPORTING
        file     TYPE xstring
        checksum TYPE string
        ok       TYPE flag.
    METHODS load_release_file
      IMPORTING
        filename TYPE rlgrap-filename
      EXPORTING
        file     TYPE xstring
        ok       TYPE flag.
    METHODS install_component
      IMPORTING
        name      TYPE string
        release   TYPE xstring
        checksum  TYPE string OPTIONAL
      EXPORTING
        installed TYPE flag.
    METHODS write_log
      IMPORTING
        iv_response TYPE string.
    METHODS log_missing_zip_entry
      IMPORTING
        zip      TYPE REF TO cl_abap_zip
        expected TYPE string
        context  TYPE string
        prefix   TYPE string OPTIONAL.
    METHODS run_offline
      EXPORTING
        ok_server TYPE flag
        ok_rest   TYPE flag.
    METHODS run_online
      EXPORTING
        ok_server TYPE flag
        ok_rest   TYPE flag.
    METHODS validate_installation
      RETURNING VALUE(valid) TYPE flag.
    METHODS confirm_installation
      RETURNING VALUE(confirmed) TYPE flag.
    METHODS confirm_transport_overwrite
      IMPORTING
        transport_request TYPE trkorr
        component_name    TYPE string
      RETURNING
        VALUE(confirmed)  TYPE flag.
    METHODS handle_release
      IMPORTING
        name      TYPE string
        release   TYPE xstring
        trkorr    TYPE trkorr
        checksum  TYPE string OPTIONAL
      EXPORTING
        installed TYPE flag
        integrity TYPE string
        manifest  TYPE xstring.

    CLASS-METHODS display_error
      IMPORTING
        iv_text TYPE string.
    CLASS-METHODS get_dir_trans
      EXPORTING dir_trans TYPE pfevalue.
    CLASS-METHODS write_binary_file
      IMPORTING file_path      TYPE string
                file           TYPE xstring
      EXPORTING written        TYPE flag
                error          TYPE string.
    CLASS-METHODS get_file_sys
      EXPORTING file_sys TYPE filesys.
    CLASS-METHODS delete_from_tms_queue
      IMPORTING trkorr TYPE trkorr
                system TYPE tmssysnam
      EXPORTING subrc  TYPE i.
    CLASS-METHODS forward
      IMPORTING trkorr       TYPE trkorr
                target       TYPE tmssysnam
                source       TYPE tmssysnam
                import_again TYPE flag
      EXPORTING subrc        TYPE i.
    CLASS-METHODS import
      IMPORTING trkorr   TYPE trkorr
                system   TYPE tmssysnam
      EXPORTING subrc    TYPE i
                tpstdout TYPE t_tpstdout.
    CLASS-METHODS add_fm_value
      IMPORTING name   TYPE abap_parmname
                value  TYPE any
      CHANGING  params TYPE abap_func_parmbind_tab.
    CLASS-METHODS fm_has_parameter
      IMPORTING funcname     TYPE rs38l_fnam
                parameter    TYPE rs38l_par_
      RETURNING VALUE(found) TYPE flag.
    CLASS-METHODS rebuild_hierarchy
      IMPORTING
        it_packages         TYPE ty_package_tab
      RETURNING
        VALUE(rt_hierarchy) TYPE ty_pkg_node_tab.
    CLASS-METHODS get_parent
      IMPORTING
        iv_package       TYPE devclass
        it_packages      TYPE ty_package_tab
      RETURNING
        VALUE(rv_parent) TYPE string.
    CLASS-METHODS get_transport_manifest
      IMPORTING trkorr          TYPE trkorr
      RETURNING VALUE(manifest) TYPE xstring.
    CLASS-METHODS update_packages_table
      IMPORTING
        name      TYPE string
        integrity TYPE string
        devclass  TYPE devclass.
    CLASS-METHODS execute_post_activities
      IMPORTING
        manifest TYPE xstring.
    CLASS-METHODS activate_rest_sicf.

ENDCLASS.

CLASS lcl_report IMPLEMENTATION.

  METHOD raise_error.
    cl_message_helper=>set_msg_vars_for_clike( message ).
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
  ENDMETHOD.

  METHOD check_json_parser.
    DATA test TYPE REF TO object.
    TRY.
        CREATE OBJECT test TYPE ('/UI2/CL_JSON').
        exists = 'X'.
      CATCH cx_root.
        CLEAR exists.
    ENDTRY.
  ENDMETHOD.

  METHOD check_vscan_profile.
    DATA: lo_intf   TYPE REF TO cl_abap_intfdescr,
          ls_method TYPE abap_methdescr.
    lo_intf ?= cl_abap_typedescr=>describe_by_name( 'IF_HTTP_RESPONSE' ).
    READ TABLE lo_intf->methods WITH KEY name = 'GET_DATA' INTO ls_method.
    IF sy-subrc EQ 0.
      READ TABLE ls_method-parameters WITH KEY name = 'VIRUS_SCAN_PROFILE' parm_kind = cl_abap_objectdescr=>importing TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        exists = 'X'.
      ELSE.
        CLEAR exists.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_versions.
    FIELD-SYMBOLS: <trm_server_version>  TYPE string,
                   <fs_trm_rest_version> TYPE string.
    ASSIGN ('/ATRM/IF_SERVER')=>('VERSION') TO <trm_server_version>.
    IF <trm_server_version> IS ASSIGNED.
      server = <trm_server_version>.
    ENDIF.
    ASSIGN ('/ATRM/IF_REST')=>('VERSION') TO <fs_trm_rest_version>.
    IF <fs_trm_rest_version> IS ASSIGNED.
      rest = <fs_trm_rest_version>.
    ENDIF.
  ENDMETHOD.

  METHOD check_registry_certificates.
    DATA: client TYPE REF TO if_http_client,
          code   TYPE i,
          reason TYPE string.

    CLEAR: missing, technical_error.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = 'Reaching registry'.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = base_url
        ssl_id             = p_id
      IMPORTING
        client             = client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
    client->send(
      EXPORTING
        timeout                    = 3
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc = 0.
      client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
    ENDIF.
    client->get_last_error(
        IMPORTING
          code           = code
          message        = reason
      ).
    IF code = 421.
      missing = 'X'.
      CONCATENATE '@0A@ HTTP 421' reason INTO technical_error SEPARATED BY space.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN technical_error WITH space.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN technical_error WITH space.
    ENDIF.
    client->close( ).
  ENDMETHOD.

  METHOD install_registry_certificates.
    DATA: encoded_certificates TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          encoded_certificate  TYPE string,
          certificate          TYPE xstring,
          install_error        TYPE string.

* BEGIN AUTO-GENERATED REGISTRY CERTIFICATES
* Certificate 1 of 3 from trmregistry.com:443
    CONCATENATE
      'MIIF1DCCBLygAwIBAgIQAnavQl3+3mYrUCaS9F/mrzANBgkqhkiG9w0BAQsFADA8'
      'MQswCQYDVQQGEwJVUzEPMA0GA1UEChMGQW1hem9uMRwwGgYDVQQDExNBbWF6b24g'
      'UlNBIDIwNDggTTAxMB4XDTI2MDgzMDAwMDAwMFoXDTI3MDMxNTIzNTk1OVowGjEY'
      'MBYGA1UEAxMPdHJtcmVnaXN0cnkuY29tMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8A'
      'MIIBCgKCAQEA3Yb6MpZu9fvTufFqoUggfC4ziWBc2G7iPLV+adsLYMplHSLfORF4'
      'aH5akdIFwf07iV3f5UWJKIdE67JxI9RgWN0EP5j7/hSF5j9vWE89oMSfWBp5nQwM'
      '4lY6xQUeNF0xQY5ZV9ZAz21A+eCJDBOPbCTVwePSwaC/+2rmitmfk9Z2CLXHYm/m'
      'hj/qmexQMj/Q/qQuHvLrsydetvBoUJ+mJ51LwKFMSD+anf+t5BQNxZIXCZ+h0qcN'
      'njelscw456ba2ocx7QKmOSREXR+mibJ0oc9nX4Av1qXV5gT40uedU+U/nzKlZKwS'
      'PjB2+gvzgx55e4+GqKLrsbVcdR46HH5suQIDAQABo4IC8jCCAu4wHwYDVR0jBBgw'
      'FoAUgbgOY4qJEhjl+js7UJWf5uWQE4UwHQYDVR0OBBYEFBQxv3cJ9xTpJysrAk4D'
      'AUVkhNlZMC8GA1UdEQQoMCaCD3RybXJlZ2lzdHJ5LmNvbYITd3d3LnRybXJlZ2lz'
      'dHJ5LmNvbTATBgNVHSAEDDAKMAgGBmeBDAECATAOBgNVHQ8BAf8EBAMCBaAwEwYD'
      'VR0lBAwwCgYIKwYBBQUHAwEwOwYDVR0fBDQwMjAwoC6gLIYqaHR0cDovL2NybC5y'
      'Mm0wMS5hbWF6b250cnVzdC5jb20vcjJtMDEuY3JsMHUGCCsGAQUFBwEBBGkwZzAt'
      'BggrBgEFBQcwAYYhaHR0cDovL29jc3AucjJtMDEuYW1hem9udHJ1c3QuY29tMDYG'
      'CCsGAQUFBzAChipodHRwOi8vY3J0LnIybTAxLmFtYXpvbnRydXN0LmNvbS9yMm0w'
      'MS5jZXIwDAYDVR0TAQH/BAIwADCCAX0GCisGAQQB1nkCBAIEggFtBIIBaQFnAHUA'
      'TGPcmOWcHauI9h6KPd6uj6tEozd7X5uUw/uhnPzBviYAAAGgUJYtVgAABAMARjBE'
      'AiB+nUDxu81yZxe/pYllupxzURPjr6t/IH7kSZwldeMkQQIgG3S9H07oEOIpTZc/'
      'h8sckenSw3/gzOCh6wE+3LVYTboAdgDW1Y2p0BdT82pKoMdXSQKv68fcLNOM2fdk'
      'yAyJGR6fAgAAAaBQli0cAAAEAwBHMEUCICSUeJ8vbFjjGC4+gniSQwzLK1W6w06Y'
      'ouThfj6PhBxNAiEA2WbqFAWKzzaQTuhFFXnGgsj4CDImVN0CLm8d0bF3dCkAdgBE'
      'wr0M6RQOZKXJSgGTClqhuzWXDgDuERaJaCocRNe1ZgAAAaBQli1fAAAEAwBHMEUC'
      'IQDU79fU01e7mnfm7Plxtp3dZDZ4hRJKLmVdbolpQc+KvwIgeS1a4uQ/cZ90+2j6'
      'bHVuHD40K82fHWla+Drt0rTkonYwDQYJKoZIhvcNAQELBQADggEBAHnR310jc6VX'
      'GS1Vh53PW+3hbnhXGYOjMNh/9e+z8ZzInB2ytR8del+XlWMkfL1eYAUpVxDDUpog'
      's4ooOHTOkvp3TB0zfJd3g3P8zUvzyNCrPy7hMx5lJv/kgTfXLoxSAoWYdWpOFGpU'
      'YGnZQIjzT7VfXk7aiZSoScxmZTBs1Io0k4RB+xCL/M6WgV3VbDGrFhpJmmjppG/R'
      'tUDjmK9q6R394Du7lEm5mXdMHQ/KMSP7EfNFm7d2bscsVK3OMtQ0qouWCiVq3C9B'
      'rNtxfVvqgvbs6beEMU3mY0SnPE297Ml0urf3OPQ19gqHKWEnDiSraBu3nMk4cR/u'
      'u0nC4wOYQTM='
      INTO encoded_certificate.
    APPEND encoded_certificate TO encoded_certificates.

* Certificate 2 of 3 from trmregistry.com:443
    CONCATENATE
      'MIIEXjCCA0agAwIBAgITB3MSOAudZoijOx7Zv5zNpo4ODzANBgkqhkiG9w0BAQsF'
      'ADA5MQswCQYDVQQGEwJVUzEPMA0GA1UEChMGQW1hem9uMRkwFwYDVQQDExBBbWF6'
      'b24gUm9vdCBDQSAxMB4XDTIyMDgyMzIyMjEyOFoXDTMwMDgyMzIyMjEyOFowPDEL'
      'MAkGA1UEBhMCVVMxDzANBgNVBAoTBkFtYXpvbjEcMBoGA1UEAxMTQW1hem9uIFJT'
      'QSAyMDQ4IE0wMTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAOtxLKnL'
      'H4gokjIwr4pXD3i3NyWVVYesZ1yX0yLI2qIUZ2t88Gfa4gMqs1YSXca1R/lnCKeT'
      'epWSGA+0+fkQNpp/L4C2T7oTTsddUx7g3ZYzByDTlrwS5HRQQqEFE3O1T5tEJP4t'
      'f+28IoXsNiEzl3UGzicYgtzj2cWCB41eJgEmJmcf2T8TzzK6a614ZPyq/w4CPAff'
      'nAV4coz96nW3AyiE2uhuB4zQUIXvgVSycW7sbWLvj5TDXunEpNCRwC4kkZjK7rol'
      'jtT2cbb7W2s4Bkg3R42G3PLqBvt2N32e/0JOTViCk8/iccJ4sXqrS1uUN4iB5Nmv'
      'JK74csVl+0u0UecCAwEAAaOCAVowggFWMBIGA1UdEwEB/wQIMAYBAf8CAQAwDgYD'
      'VR0PAQH/BAQDAgGGMB0GA1UdJQQWMBQGCCsGAQUFBwMBBggrBgEFBQcDAjAdBgNV'
      'HQ4EFgQUgbgOY4qJEhjl+js7UJWf5uWQE4UwHwYDVR0jBBgwFoAUhBjMhTTsvAyU'
      'lC4IWZzHshBOCggwewYIKwYBBQUHAQEEbzBtMC8GCCsGAQUFBzABhiNodHRwOi8v'
      'b2NzcC5yb290Y2ExLmFtYXpvbnRydXN0LmNvbTA6BggrBgEFBQcwAoYuaHR0cDov'
      'L2NydC5yb290Y2ExLmFtYXpvbnRydXN0LmNvbS9yb290Y2ExLmNlcjA/BgNVHR8E'
      'ODA2MDSgMqAwhi5odHRwOi8vY3JsLnJvb3RjYTEuYW1hem9udHJ1c3QuY29tL3Jv'
      'b3RjYTEuY3JsMBMGA1UdIAQMMAowCAYGZ4EMAQIBMA0GCSqGSIb3DQEBCwUAA4IB'
      'AQCtAN4CBSMuBjJitGuxlBbkEUDeK/pZwTXv4KqPK0G50fOHOQAd8j21p0cMBgbG'
      'kfMHVwLU7b0XwZCav0h1ogdPMN1KakK1DT0VwA/+hFvGPJnMV1Kx2G4S1ZaSk0uU'
      '5QfoiYIIano01J5k4T2HapKQmmOhS/iPtuo00wW+IMLeBuKMn3OLn005hcrOGTad'
      'hcmeyfhQP7Z+iKHvyoQGi1C0ClymHETx/chhQGDyYSWqB/THwnN15AwLQo0E5V9E'
      'SJlbe4mBlqeInUsNYugExNf+tOiybcrswBy8OFsd34XOW3rjSUtsuafd9AWySa3h'
      'xRRrwszrzX/WWGm6wyB+f7C4'
      INTO encoded_certificate.
    APPEND encoded_certificate TO encoded_certificates.

* Certificate 3 of 3 from trmregistry.com:443
    CONCATENATE
      'MIIEkjCCA3qgAwIBAgITBn+USionzfP6wq4rAfkI7rnExjANBgkqhkiG9w0BAQsF'
      'ADCBmDELMAkGA1UEBhMCVVMxEDAOBgNVBAgTB0FyaXpvbmExEzARBgNVBAcTClNj'
      'b3R0c2RhbGUxJTAjBgNVBAoTHFN0YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4x'
      'OzA5BgNVBAMTMlN0YXJmaWVsZCBTZXJ2aWNlcyBSb290IENlcnRpZmljYXRlIEF1'
      'dGhvcml0eSAtIEcyMB4XDTE1MDUyNTEyMDAwMFoXDTM3MTIzMTAxMDAwMFowOTEL'
      'MAkGA1UEBhMCVVMxDzANBgNVBAoTBkFtYXpvbjEZMBcGA1UEAxMQQW1hem9uIFJv'
      'b3QgQ0EgMTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBALJ4gHHKeNXj'
      'ca9HgFB0fW7Y14h29Jlo91ghYPl0hAEvrAIthtOgQ3pOsqTQNroBvo3bSMgHFzZM'
      '9O6II8c+6zf1tRn4SWiw3te5djgdYZ6k/oI2peVKVuRF4fn9tBb6dNqcmzU5L/qw'
      'IFAGbHrQgLKm+a/sRxmPUDgH3KKHOVj4utWp+UhnMJbulHheb4mjUcAwhmahRWa6'
      'VOujw5H5SNz/0egwLX0tdHA114gk957EWW67c4cX8jJGKLhD+rcdqsq08p8kDi1L'
      '93FcXmn/6pUCyziKrlA4b9v7LWIbxcceVOF34GfID5yHI9Y/QCB/IIDEgEw+OyQm'
      'jgSubJrIqg0CAwEAAaOCATEwggEtMA8GA1UdEwEB/wQFMAMBAf8wDgYDVR0PAQH/'
      'BAQDAgGGMB0GA1UdDgQWBBSEGMyFNOy8DJSULghZnMeyEE4KCDAfBgNVHSMEGDAW'
      'gBScXwDfqgHXMCs4iKK4bUqc8hGRgzB4BggrBgEFBQcBAQRsMGowLgYIKwYBBQUH'
      'MAGGImh0dHA6Ly9vY3NwLnJvb3RnMi5hbWF6b250cnVzdC5jb20wOAYIKwYBBQUH'
      'MAKGLGh0dHA6Ly9jcnQucm9vdGcyLmFtYXpvbnRydXN0LmNvbS9yb290ZzIuY2Vy'
      'MD0GA1UdHwQ2MDQwMqAwoC6GLGh0dHA6Ly9jcmwucm9vdGcyLmFtYXpvbnRydXN0'
      'LmNvbS9yb290ZzIuY3JsMBEGA1UdIAQKMAgwBgYEVR0gADANBgkqhkiG9w0BAQsF'
      'AAOCAQEAYjdCXLwQtT6LLOkMm2xF4gcAevnFWAu5CIw+7bMlPLVvUOTNNWqnkzSW'
      'MiGpSESrnO09tKpzbeR/FoCJbM8oAxiDR3mjEH4wW6w7sGDgd9QIpuEdfF7Au/ma'
      'eyKdpwAJfqxGF4PcnCZXmTA5YpaP7dreqsXMGz7KQ2hsVxa81Q4gLv7/wmpdLqBK'
      'bRRYh5TmOTFffHPLkIhqhBGWJ6bt2YFGpn6jcgAKUj6DiAdjd4lpFw85hdKrCEVN'
      '0FE6/V1dN2RMfjCyVSRCnTawXZwXgWHxyvkQAiSr6w10kY17RSlQOYiypok1JR4U'
      'akcjMS9cmvqtmg5iUaQqqcT5NJ0hGA=='
      INTO encoded_certificate.
    APPEND encoded_certificate TO encoded_certificates.
* END AUTO-GENERATED REGISTRY CERTIFICATES

    LOOP AT encoded_certificates INTO encoded_certificate.
      " CL_ABAP_X509_CERTIFICATE doesn't exist on older releases (e.g. 7.0):
      " the certificates are base64 DER, so decoding them is all that's needed
      CLEAR certificate.
      CALL FUNCTION 'SSFC_BASE64_DECODE'
        EXPORTING
          b64data = encoded_certificate
        IMPORTING
          bindata = certificate
        EXCEPTIONS
          OTHERS  = 1.
      IF sy-subrc <> 0 OR certificate IS INITIAL.
        install_error = 'Registry certificate could not be decoded'.
        MESSAGE install_error TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      install_error = lcl_strust=>add_certificate(
        application = p_id
        certificate = certificate ).
      IF install_error IS NOT INITIAL.
        MESSAGE install_error TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDLOOP.

    installed = 'X'.
  ENDMETHOD.

  METHOD get_client.
    DATA client_url TYPE string.
    IF with_base_url EQ 'X'.
      client_url = base_url.
    ENDIF.
    IF url IS NOT INITIAL.
      CONCATENATE client_url url INTO client_url.
    ENDIF.
    cl_http_client=>create_by_url(
      EXPORTING
        url                 = client_url
        ssl_id              = p_id
        proxy_host          = p_proxy
        proxy_service       = p_pport
      IMPORTING
        client              = client
      EXCEPTIONS
        argument_not_found  = 1
        plugin_not_active   = 2
        internal_error      = 3
        OTHERS              = 4 ).

    IF sy-subrc <> 0.
      display_error( 'Error in HTTP Client Create' ).
      RETURN.
    ENDIF.

    IF p_puser IS NOT INITIAL.
      client->authenticate(
        proxy_authentication = abap_true
        username             = p_puser
        password             = p_ppwd ).
    ENDIF.

    client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
  ENDMETHOD.

  METHOD execute_http_request.
    DATA: code      TYPE i,
          code_text TYPE c LENGTH 3,
          reason    TYPE string.

    CLEAR: ok, error.
    IF client IS NOT BOUND.
      error = 'HTTP client could not be created'.
      RETURN.
    ENDIF.
    client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc = 0.
      client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
    ENDIF.
    IF sy-subrc <> 0.
      client->get_last_error( IMPORTING message = error ).
      IF error IS INITIAL.
        error = 'HTTP request failed'.
      ENDIF.
      RETURN.
    ENDIF.

    client->response->get_status(
      IMPORTING
        code   = code
        reason = reason ).
    IF code < 200 OR code >= 300.
      WRITE code TO code_text LEFT-JUSTIFIED.
      CONCATENATE 'HTTP' code_text reason INTO error SEPARATED BY space.
      RETURN.
    ENDIF.
    ok = 'X'.
  ENDMETHOD.

  METHOD download_release.
    DATA: client      TYPE REF TO if_http_client,
          endpoint    TYPE string,
          error       TYPE string,
          request_ok  TYPE flag,
          release     TYPE ty_registry_release,
          response    TYPE string,
          vscan_check TYPE c.

    CLEAR: file, checksum, ok.
    IF p_vscan = 'X'.
      vscan_check = 'A'.
    ELSE.
      vscan_check = 'N'.
    ENDIF.

    CONCATENATE '/package/' name INTO endpoint.
    client = get_client( endpoint ).
    IF client IS NOT BOUND.
      RETURN.
    ENDIF.
    client->request->set_method( if_http_request=>co_request_method_get ).
    execute_http_request(
      EXPORTING client = client
      IMPORTING ok = request_ok error = error ).
    IF request_ok <> 'X'.
      display_error( error ).
      write_log( error ).
      client->close( ).
      RETURN.
    ENDIF.
    response = client->response->get_cdata( ).
    client->close( ).

    TRY.
        CALL METHOD ('/UI2/CL_JSON')=>deserialize
          EXPORTING
            json = response
          CHANGING
            data = release.
      CATCH cx_sy_dyn_call_error.
        display_error( 'Invalid response from TRM Registry.' ).
        write_log( response ).
        RETURN.
    ENDTRY.
    IF release-download_link IS INITIAL OR release-checksum IS INITIAL.
      display_error( 'Incomplete release information from TRM Registry.' ).
      write_log( response ).
      RETURN.
    ENDIF.

    client = get_client(
      with_base_url = ' '
      url           = release-download_link ).
    IF client IS NOT BOUND.
      RETURN.
    ENDIF.
    client->request->set_method( if_http_request=>co_request_method_get ).
    client->request->set_header_field(
      name  = 'Accept'
      value = 'application/octet-stream' ).
    execute_http_request(
      EXPORTING client = client
      IMPORTING ok = request_ok error = error ).
    IF request_ok <> 'X'.
      display_error( error ).
      write_log( error ).
      client->close( ).
      RETURN.
    ENDIF.

    TRY.
        CALL METHOD client->response->('GET_DATA')
          EXPORTING
            virus_scan_profile = p_vscanp
            vscan_scan_always  = vscan_check
          RECEIVING
            data               = file.
      CATCH cx_sy_dyn_call_error.
        " older releases: GET_DATA has no VIRUS_SCAN_PROFILE parameter
        TRY.
            CALL METHOD client->response->('GET_DATA')
              EXPORTING
                vscan_scan_always = vscan_check
              RECEIVING
                data              = file.
          CATCH cx_sy_dyn_call_error.
            " even older releases: no virus scan parameters at all
            file = client->response->get_data( ).
        ENDTRY.
    ENDTRY.
    client->close( ).
    IF file IS INITIAL.
      display_error( 'Downloaded release is empty.' ).
      RETURN.
    ENDIF.

    checksum = release-checksum.
    ok = 'X'.
  ENDMETHOD.

  METHOD load_release_file.
    DATA: filename_string TYPE string,
          binary_data     TYPE STANDARD TABLE OF x255,
          file_length     TYPE i.

    CLEAR: file, ok.
    filename_string = filename.
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = filename_string
        filetype                = 'BIN'
      IMPORTING
        filelength              = file_length
      TABLES
        data_tab                = binary_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc <> 0.
      write_log( 'Error during binary upload.' ).
      raise_error( 'Error during binary upload.' ).
      RETURN.
    ENDIF.
    IF file_length <= 0.
      raise_error( 'The selected release file is empty.' ).
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = file_length
      IMPORTING
        buffer       = file
      TABLES
        binary_tab   = binary_data
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0 OR file IS INITIAL.
      write_log( 'Error converting binary table.' ).
      raise_error( 'Error converting binary table.' ).
      RETURN.
    ENDIF.
    ok = 'X'.
  ENDMETHOD.

  METHOD install_component.
    DATA: devclass  TYPE devclass,
          integrity TYPE string,
          manifest  TYPE xstring,
          trkorr    TYPE trkorr.

    CLEAR installed.
    CASE name.
      WHEN 'trm-server'.
        trkorr = server_trkorr.
        devclass = '$TRM'.
      WHEN 'trm-rest'.
        trkorr = rest_trkorr.
        devclass = '$TRM_REST'.
      WHEN OTHERS.
        display_error( 'Unknown component selected for installation.' ).
        RETURN.
    ENDCASE.

    CONCATENATE 'Starting installation of' name '...' INTO integrity SEPARATED BY space.
    write_log( integrity ).
    CLEAR integrity.
    handle_release(
      EXPORTING
        name      = name
        release   = release
        trkorr    = trkorr
        checksum  = checksum
      IMPORTING
        installed = installed
        integrity = integrity
        manifest  = manifest ).
    IF installed <> 'X'.
      RETURN.
    ENDIF.

    update_packages_table(
      name      = name
      integrity = integrity
      devclass  = devclass ).
    IF name = 'trm-rest'.
      activate_rest_sicf( ).
    ENDIF.
    execute_post_activities(
      manifest = manifest ).
  ENDMETHOD.

  METHOD validate_installation.
    CLEAR valid.
    IF p_srv <> 'X' AND p_rest <> 'X'.
      write_log( 'Please select at least one component to install.' ).
      raise_error( 'Please select at least one component to install.' ).
      RETURN.
    ENDIF.
    IF psel-activetab = 'TAB2'.
      IF p_srv = 'X' AND p_lserv IS INITIAL.
        write_log( 'Missing trm-server release file!' ).
        raise_error( 'Missing trm-server release file!' ).
        RETURN.
      ENDIF.
      IF p_rest = 'X' AND p_lrest IS INITIAL.
        write_log( 'Missing trm-rest release file!' ).
        raise_error( 'Missing trm-rest release file!' ).
        RETURN.
      ENDIF.
    ENDIF.
    valid = 'X'.
  ENDMETHOD.

  METHOD confirm_installation.
    DATA: answer   TYPE c,
          question TYPE string,
          source   TYPE string.

    IF p_srv = 'X' AND p_rest = 'X'.
      question = 'trm-server and trm-rest'.
    ELSEIF p_srv = 'X'.
      question = 'trm-server'.
    ELSE.
      question = 'trm-rest'.
    ENDIF.
    IF psel-activetab = 'TAB1'.
      source = 'downloaded from the registry'.
    ELSE.
      source = 'loaded from the selected files'.
    ENDIF.
    CONCATENATE question 'will be' source 'and imported. Do you want to proceed?'
      INTO question SEPARATED BY space.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = question
        text_button_1         = 'Continue'
        icon_button_1         = '@0V@'
        text_button_2         = 'Cancel'
        icon_button_2         = '@0W@'
        display_cancel_button = ' '
      IMPORTING
        answer                = answer.
    IF answer = '1'.
      confirmed = 'X'.
    ENDIF.
  ENDMETHOD.

  METHOD confirm_transport_overwrite.
    DATA: answer           TYPE c,
          existing_request TYPE trkorr,
          question         TYPE string.

    confirmed = 'X'.
    SELECT SINGLE trkorr FROM e070 INTO existing_request
      WHERE trkorr = transport_request.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CONCATENATE 'Transport' transport_request '(' component_name 'transport number) already exists in'
      sy-sysid 'Do you want to overwrite?' INTO question SEPARATED BY space.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = question
        text_button_1         = 'Continue'
        icon_button_1         = '@0V@'
        text_button_2         = 'Cancel'
        icon_button_2         = '@0W@'
        display_cancel_button = ' '
      IMPORTING
        answer                = answer.
    IF answer <> '1'.
      CLEAR confirmed.
    ENDIF.
  ENDMETHOD.

  METHOD run.
    DATA:
      server_version   TYPE string,
      rest_version     TYPE string,
      install_versions TYPE string,
      ok_server        TYPE flag,
      ok_rest          TYPE flag.

    IF validate_installation( ) <> 'X'.
      RETURN.
    ENDIF.
    IF p_srv = 'X'.
      IF confirm_transport_overwrite(
           transport_request = server_trkorr
           component_name    = 'trm-server' ) <> 'X'.
        write_log( 'Installation cancelled by user.' ).
        RETURN.
      ENDIF.
    ENDIF.
    IF p_rest = 'X'.
      IF confirm_transport_overwrite(
           transport_request = rest_trkorr
           component_name    = 'trm-rest' ) <> 'X'.
        write_log( 'Installation cancelled by user.' ).
        RETURN.
      ENDIF.
    ENDIF.
    IF confirm_installation( ) <> 'X'.
      write_log( 'Installation cancelled by user.' ).
      RETURN.
    ENDIF.

    IF psel-activetab = 'TAB1'.
      run_online(
        IMPORTING
          ok_server = ok_server
          ok_rest   = ok_rest
      ).
    ELSEIF psel-activetab = 'TAB2'.
      run_offline(
        IMPORTING
          ok_server = ok_server
          ok_rest   = ok_rest
      ).
    ENDIF.
    get_versions( IMPORTING server = server_version rest = rest_version ).
    IF ok_server EQ 'X' OR ok_rest EQ 'X'.
      install_versions = 'Successfully installed'.
      IF p_srv EQ 'X' AND ok_server EQ 'X'.
        CONCATENATE install_versions 'trm-server' server_version INTO install_versions SEPARATED BY space.
      ENDIF.
      IF p_rest EQ 'X' AND ok_rest EQ 'X'.
        CONCATENATE install_versions 'trm-rest' rest_version INTO install_versions SEPARATED BY space.
      ENDIF.
      MESSAGE install_versions TYPE 'I'.
      WRITE: /, install_versions.
    ENDIF.
  ENDMETHOD.

  METHOD display_error.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Error'
        txt1  = iv_text
        txt2  = ''.
  ENDMETHOD.

  METHOD log_missing_zip_entry.
    DATA: file     TYPE cl_abap_zip=>t_file,
          log_text TYPE string,
          size     TYPE string,
          found    TYPE flag,
          pattern  TYPE string.
    CONCATENATE prefix '*' INTO pattern.
    CONCATENATE 'Expected entry' expected 'in' context 'but it was not found.'
      INTO log_text SEPARATED BY space.
    write_log( log_text ).
    IF prefix IS INITIAL.
      write_log( 'Entries found:' ).
    ELSE.
      CONCATENATE 'Entries found in' prefix INTO log_text SEPARATED BY space.
      CONCATENATE log_text ':' INTO log_text.
      write_log( log_text ).
    ENDIF.
    LOOP AT zip->files INTO file.
      IF prefix IS NOT INITIAL AND file-name NP pattern.
        CONTINUE.
      ENDIF.
      found = 'X'.
      size = file-size.
      CONDENSE size.
      CONCATENATE '  -' file-name '(' INTO log_text SEPARATED BY space.
      CONCATENATE log_text size ' bytes)' INTO log_text.
      write_log( log_text ).
    ENDLOOP.
    IF found IS INITIAL.
      write_log( '  (none)' ).
    ENDIF.
  ENDMETHOD.

  METHOD write_log.
    DATA:
      lt_lines TYPE TABLE OF string,
      lv_line  TYPE string.

    SPLIT iv_response AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    LOOP AT lt_lines INTO lv_line.
      WRITE / lv_line.
    ENDLOOP.
    SKIP.
  ENDMETHOD.

  METHOD run_offline.
    DATA: file    TYPE xstring,
          file_ok TYPE flag.

    IF p_srv EQ 'X'.
      load_release_file(
        EXPORTING filename = p_lserv
        IMPORTING file = file ok = file_ok ).
      IF file_ok <> 'X'.
        RETURN.
      ENDIF.
      install_component(
        EXPORTING name = 'trm-server' release = file
        IMPORTING installed = ok_server ).
      IF ok_server <> 'X'.
        RETURN.
      ENDIF.
    ENDIF.

    IF p_rest EQ 'X'.
      CLEAR: file, file_ok.
      load_release_file(
        EXPORTING filename = p_lrest
        IMPORTING file = file ok = file_ok ).
      IF file_ok <> 'X'.
        RETURN.
      ENDIF.
      install_component(
        EXPORTING name = 'trm-rest' release = file
        IMPORTING installed = ok_rest ).
    ENDIF.
  ENDMETHOD.

  METHOD run_online.
    DATA:
      code        TYPE i,
      code_text   TYPE string,
      log_text    TYPE string,
      request_ok  TYPE flag,
      download_ok TYPE flag,
      client      TYPE REF TO if_http_client,
      reason      TYPE string,
      response    TYPE string,
      file        TYPE xstring,
      checksum    TYPE string.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = 'Reaching registry'.

    client = get_client( ).
    execute_http_request(
      EXPORTING client = client
      IMPORTING ok = request_ok error = response ).
    IF request_ok <> 'X'.
      display_error( response ).
      write_log( response ).
      write_log( 'Also check transaction SMICM -> Goto -> Trace File -> Display End' ).
      IF client IS BOUND.
        client->close( ).
      ENDIF.
      RETURN.
    ENDIF.
* if SSL Handshake fails, make sure to also check https://launchpad.support.sap.com/#/notes/510007
    client->response->get_status(
      IMPORTING
        code   = code
        reason = reason ).
    IF code <> 200.
      code_text = code.
      CONCATENATE 'TRM Registry is unreachable! Code' code_text INTO log_text SEPARATED BY space.
      write_log( log_text ).
      display_error( 'TRM Registry is unreachable!' ).
      client->close( ).
      RETURN.
    ENDIF.
    client->close( ).

    write_log( 'Successfully connected to TRM Registry.' ).

    IF p_srv EQ 'X'.
      write_log( 'Starting installation of trm-server...' ).

      write_log( 'Downloading trm-server latest release from TRM Registry...' ).

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = 'Downloading trm-server latest release'.

      download_release(
        EXPORTING name = 'trm-server'
        IMPORTING file = file checksum = checksum ok = download_ok ).
      IF download_ok <> 'X'.
        RETURN.
      ENDIF.
      install_component(
        EXPORTING name = 'trm-server' release = file checksum = checksum
        IMPORTING installed = ok_server ).
      IF ok_server <> 'X'.
        RETURN.
      ENDIF.
    ENDIF.

    IF p_rest EQ 'X'.
      write_log( 'Starting installation of trm-rest...' ).

      write_log( 'Downloading trm-rest latest release from TRM Registry...' ).

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = 'Downloading trm-rest latest release'.

      CLEAR: file, checksum, download_ok.
      download_release(
        EXPORTING name = 'trm-rest'
        IMPORTING file = file checksum = checksum ok = download_ok ).
      IF download_ok <> 'X'.
        RETURN.
      ENDIF.
      install_component(
        EXPORTING name = 'trm-rest' release = file checksum = checksum
        IMPORTING installed = ok_rest ).
    ENDIF.
  ENDMETHOD.

  METHOD get_dir_trans.
    DATA lv_param_name TYPE pfeparname.
    lv_param_name = 'DIR_TRANS'.
    CALL FUNCTION 'SXPG_PROFILE_PARAMETER_GET'
      EXPORTING
        parameter_name  = lv_param_name
      IMPORTING
        parameter_value = dir_trans
      EXCEPTIONS
        OTHERS          = 1.
  ENDMETHOD.
  METHOD write_binary_file.
    DATA: os_message TYPE c LENGTH 255,
          exception  TYPE REF TO cx_root.
    CLEAR: written, error.
    TRY.
        OPEN DATASET file_path FOR OUTPUT IN BINARY MODE MESSAGE os_message.
        IF sy-subrc <> 0.
          " os_message holds the operating system error (e.g. path not found, access denied)
          error = os_message.
          IF error IS INITIAL.
            error = 'OPEN DATASET failed (no operating system message)'.
          ENDIF.
          RETURN.
        ENDIF.
        TRANSFER file TO file_path.
        written = 'X'.
        CLOSE DATASET file_path.
      CATCH cx_root INTO exception.
        " e.g. CX_SY_FILE_AUTHORITY (missing S_DATASET), CX_SY_FILE_OPEN, CX_SY_FILE_IO
        error = exception->get_text( ).
        CLEAR written.
        CLOSE DATASET file_path.
    ENDTRY.
  ENDMETHOD.
  METHOD get_file_sys.
    SELECT SINGLE filesys INTO file_sys FROM opsystem WHERE opsys = sy-opsys.
  ENDMETHOD.
  METHOD delete_from_tms_queue.
    " STMS_TP_MAINTAINS / ET_TP_MAINTAINS don't exist on older releases (e.g. 7.0):
    " for a single request TT_STDOUT holds the same tp output
    DATA: ls_tmsbuffer TYPE tmsbuffer,
          lt_tpstdout  TYPE t_tpstdout,
          ls_tpstdout  TYPE tpstdout,
          ls_exception TYPE stmscalert.

    SELECT SINGLE * FROM tmsbuffer INTO ls_tmsbuffer WHERE sysnam EQ system AND trkorr EQ trkorr.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'TMS_MGR_MAINTAIN_TR_QUEUE'
      EXPORTING
        iv_command                 = 'DELFROMBUFFER'
        iv_system                  = ls_tmsbuffer-sysnam
        iv_domain                  = ls_tmsbuffer-domnam
        iv_request                 = ls_tmsbuffer-trkorr
        iv_tarcli                  = ls_tmsbuffer-tarcli
        iv_monitor                 = ' '
        iv_verbose                 = ' '
      IMPORTING
        es_exception               = ls_exception
      TABLES
        tt_stdout                  = lt_tpstdout
      EXCEPTIONS
        read_config_failed         = 1
        table_of_requests_is_empty = 2
        OTHERS                     = 3.

    IF sy-subrc <> 0 OR ( ls_exception-error <> 'OK' AND ls_exception-error <> space ).
      LOOP AT lt_tpstdout INTO ls_tpstdout.
        WRITE: /, ls_tpstdout-line.
      ENDLOOP.
      subrc = 1.
    ELSE.
      subrc = 0.
    ENDIF.
  ENDMETHOD.
  METHOD forward.
    DATA: lt_stdout    TYPE STANDARD TABLE OF tpstdout,
          ls_stdout    LIKE LINE OF lt_stdout,
          ls_exception TYPE stmscalert.
    CALL FUNCTION 'TMS_MGR_FORWARD_TR_REQUEST'
      EXPORTING
        iv_request      = trkorr
        iv_target       = target
        iv_source       = source
        iv_import_again = import_again
        iv_monitor      = space
      IMPORTING
        es_exception    = ls_exception
      TABLES
        tt_stdout       = lt_stdout
      EXCEPTIONS
        OTHERS          = 1.
    IF sy-subrc <> 0.
      subrc = 1.
    ELSEIF ls_exception-msgty EQ 'E' OR ls_exception-msgty EQ 'A'.
      subrc = 1.
      LOOP AT lt_stdout INTO ls_stdout.
        WRITE: /, ls_stdout-line.
      ENDLOOP.
    ELSE.
      subrc = 0.
    ENDIF.
  ENDMETHOD.
  METHOD add_fm_value.
    " Bind a by-value input of a dynamic function call (copied, so literals are safe)
    CONSTANTS lc_kind_exporting TYPE i VALUE 10. " = abap_func_exporting
    DATA ls_param TYPE abap_func_parmbind.
    FIELD-SYMBOLS <value> TYPE any.
    ls_param-name = name.
    ls_param-kind = lc_kind_exporting.
    CREATE DATA ls_param-value LIKE value.
    ASSIGN ls_param-value->* TO <value>.
    <value> = value.
    INSERT ls_param INTO TABLE params.
  ENDMETHOD.
  METHOD fm_has_parameter.
    DATA lv_parameter TYPE fupararef-parameter.
    SELECT SINGLE parameter FROM fupararef INTO lv_parameter
      WHERE funcname = funcname
        AND r3state  = 'A'
        AND parameter = parameter.
    IF sy-subrc = 0.
      found = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD import.
    DATA: msgid   TYPE sy-msgid,
          msgno   TYPE sy-msgno,
          msgty   TYPE sy-msgty,
          msgv1   TYPE sy-msgv1,
          msgv2   TYPE sy-msgv2,
          msgv3   TYPE sy-msgv3,
          msgv4     TYPE sy-msgv4,
          exception TYPE stmscalert,
          lt_params TYPE abap_func_parmbind_tab,
          ls_param  TYPE abap_func_parmbind,
          lt_excps  TYPE abap_func_excpbind_tab,
          ls_excp   TYPE abap_func_excpbind.
    CONSTANTS: lc_kind_importing TYPE i VALUE 20, " = abap_func_importing
               lc_kind_tables    TYPE i VALUE 30. " = abap_func_tables
    " ES_EXCEPTION and TT_STDOUT are used instead of ET_TP_IMPORTS: for a single
    " request they hold the same alert and tp output, and are present on every release
    CLEAR tpstdout.
    " Parameters are bound dynamically because the signature differs by release:
    " IV_IGNORE_CVERS does not exist on 7.00, and a static call passing it dumps
    CALL METHOD add_fm_value EXPORTING name = 'IV_SYSTEM' value = system CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_REQUEST' value = trkorr CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_CLIENT' value = sy-mandt CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_CTC_ACTIVE' value = ' ' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_OVERTAKE' value = ' ' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_IMPORT_AGAIN' value = 'X' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_IGNORE_ORIGINALITY' value = 'X' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_IGNORE_REPAIRS' value = ' ' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_IGNORE_TRANSTYPE' value = 'X' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_IGNORE_TABLETYPE' value = 'X' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_IGNORE_PREDEC' value = 'X' CHANGING params = lt_params.
    IF fm_has_parameter( funcname = 'TMS_MGR_IMPORT_TR_REQUEST' parameter = 'IV_IGNORE_CVERS' ) = 'X'.
      CALL METHOD add_fm_value EXPORTING name = 'IV_IGNORE_CVERS' value = 'X' CHANGING params = lt_params.
    ENDIF.
    CALL METHOD add_fm_value EXPORTING name = 'IV_TEST_IMPORT' value = ' ' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_SUBSET' value = ' ' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_OFFLINE' value = ' ' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_MONITOR' value = 'X' CHANGING params = lt_params.
    CALL METHOD add_fm_value EXPORTING name = 'IV_VERBOSE' value = 'X' CHANGING params = lt_params.
    ls_param-name = 'ES_EXCEPTION'.
    ls_param-kind = lc_kind_importing.
    GET REFERENCE OF exception INTO ls_param-value.
    INSERT ls_param INTO TABLE lt_params.
    ls_param-name = 'TT_STDOUT'.
    ls_param-kind = lc_kind_tables.
    GET REFERENCE OF tpstdout INTO ls_param-value.
    INSERT ls_param INTO TABLE lt_params.
    ls_excp-name = 'ERROR_MESSAGE'.
    ls_excp-value = 1.
    INSERT ls_excp INTO TABLE lt_excps.
    ls_excp-name = 'OTHERS'.
    ls_excp-value = 2.
    INSERT ls_excp INTO TABLE lt_excps.
    CALL FUNCTION 'TMS_MGR_IMPORT_TR_REQUEST'
      PARAMETER-TABLE lt_params
      EXCEPTION-TABLE lt_excps.
    subrc = sy-subrc.
    msgty = sy-msgty.
    msgid = sy-msgid.
    msgno = sy-msgno.
    msgv1 = sy-msgv1.
    msgv2 = sy-msgv2.
    msgv3 = sy-msgv3.
    msgv4 = sy-msgv4.

    IF exception-severity = 'E'
        OR exception-msgty = 'E'
        OR exception-msgty = 'A'.
      IF exception-msgid IS NOT INITIAL
        AND exception-msgno IS NOT INITIAL.
        sy-msgid = exception-msgid.
        sy-msgty = exception-msgty.
        sy-msgno = exception-msgno.
        sy-msgv1 = exception-msgv1.
        sy-msgv2 = exception-msgv2.
        sy-msgv3 = exception-msgv3.
        sy-msgv4 = exception-msgv4.
        subrc = 1.
        RETURN.
      ENDIF.
    ENDIF.
    IF subrc <> 0.
      sy-msgid = msgid.
      sy-msgty = msgty.
      sy-msgno = msgno.
      sy-msgv1 = msgv1.
      sy-msgv2 = msgv2.
      sy-msgv3 = msgv3.
      sy-msgv4 = msgv4.
      RETURN.
    ENDIF.
  ENDMETHOD.
  METHOD rebuild_hierarchy.
    DATA: lt_packages    TYPE ty_package_tab,
          lv_package     TYPE ty_package_name,
          lv_parent      TYPE devclass,
          lv_level       TYPE i,
          lv_walk_parent TYPE devclass.
    FIELD-SYMBOLS <hierarchy> TYPE ty_pkg_node.
    lt_packages = it_packages.

    LOOP AT lt_packages INTO lv_package.
      lv_parent = get_parent(
        iv_package  = lv_package
        it_packages = lt_packages ).

      lv_level = 0.
      lv_walk_parent = lv_parent.

      " Calculate hierarchy depth
      WHILE lv_walk_parent IS NOT INITIAL.
        ADD 1 TO lv_level.
        lv_walk_parent = get_parent(
          iv_package  = lv_walk_parent
          it_packages = lt_packages ).
      ENDWHILE.

      APPEND INITIAL LINE TO rt_hierarchy ASSIGNING <hierarchy>.
      <hierarchy>-package = lv_package.
      <hierarchy>-original = lv_package.
      <hierarchy>-parent = lv_parent.
      <hierarchy>-level = lv_level.

      REPLACE FIRST OCCURRENCE OF '/ATRM/SERVER' IN <hierarchy>-package WITH '$TRM'.
      REPLACE FIRST OCCURRENCE OF '/ATRM/REST' IN <hierarchy>-package WITH '$TRM_REST'.
      REPLACE FIRST OCCURRENCE OF '/ATRM/SERVER' IN <hierarchy>-parent WITH '$TRM'.
      REPLACE FIRST OCCURRENCE OF '/ATRM/REST' IN <hierarchy>-parent WITH '$TRM_REST'.
    ENDLOOP.

    SORT rt_hierarchy BY level package.
  ENDMETHOD.
  METHOD get_parent.
    DATA: lv_best_len      TYPE i VALUE -1,
          lv_candidate     TYPE ty_package_name,
          lv_candidate_len TYPE i,
          lv_package_len   TYPE i.

    LOOP AT it_packages INTO lv_candidate.
      IF lv_candidate = iv_package.
        CONTINUE.
      ENDIF.

      lv_candidate_len = strlen( lv_candidate ).
      lv_package_len   = strlen( iv_package ).

      " Candidate must be shorter
      IF lv_candidate_len >= lv_package_len.
        CONTINUE.
      ENDIF.

      " Candidate must be a prefix of the package
      IF iv_package+0(lv_candidate_len) <> lv_candidate.
        CONTINUE.
      ENDIF.

      " require '_' after the parent name to avoid false positives
      IF iv_package+lv_candidate_len(1) <> '_'.
        CONTINUE.
      ENDIF.

      " Keep the longest matching prefix = immediate parent
      IF lv_candidate_len > lv_best_len.
        lv_best_len = lv_candidate_len.
        rv_parent = lv_candidate.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_transport_manifest.
    TYPES: BEGIN OF ty_documentation,
             langu   TYPE doku_langu,
             version TYPE dokvers,
             value   TYPE string,
           END OF ty_documentation.
    TYPES: BEGIN OF ty_doktl_line,
             no    TYPE doku_line,
             value TYPE string,
           END OF ty_doktl_line,
           tyt_doktl_line TYPE STANDARD TABLE OF ty_doktl_line WITH DEFAULT KEY,
           BEGIN OF ty_doktl,
             langu     TYPE doku_langu,
             version   TYPE dokvers,
             doc_lines TYPE tyt_doktl_line,
           END OF ty_doktl.
    DATA: docs            TYPE STANDARD TABLE OF ty_documentation,
          lt_doktl        TYPE STANDARD TABLE OF doktl,
          ls_doktl        LIKE LINE OF lt_doktl,
          lt_trkorr_doktl TYPE STANDARD TABLE OF ty_doktl,
          ls_trkorr_doktl LIKE LINE OF lt_trkorr_doktl,
          ls_dokt_line    TYPE ty_doktl_line,
          lt_lines        TYPE STANDARD TABLE OF string.
    FIELD-SYMBOLS: <fs_trkorr_doktl>      TYPE ty_doktl,
                   <fs_trkorr_doktl_line> TYPE ty_doktl_line,
                   <fs_doc>               TYPE ty_documentation.
    SELECT langu dokversion line doktext FROM doktl INTO CORRESPONDING FIELDS OF TABLE lt_doktl WHERE id EQ 'TA' AND object EQ trkorr.
    LOOP AT lt_doktl INTO ls_doktl.
      UNASSIGN <fs_trkorr_doktl>.
      UNASSIGN <fs_trkorr_doktl_line>.
      READ TABLE lt_trkorr_doktl ASSIGNING <fs_trkorr_doktl> WITH KEY langu = ls_doktl-langu version = ls_doktl-dokversion.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO lt_trkorr_doktl ASSIGNING <fs_trkorr_doktl>.
        <fs_trkorr_doktl>-langu = ls_doktl-langu.
        <fs_trkorr_doktl>-version = ls_doktl-dokversion.
      ENDIF.
      APPEND INITIAL LINE TO <fs_trkorr_doktl>-doc_lines ASSIGNING <fs_trkorr_doktl_line>.
      <fs_trkorr_doktl_line>-no = ls_doktl-line.
      <fs_trkorr_doktl_line>-value = ls_doktl-doktext.
      SORT <fs_trkorr_doktl>-doc_lines BY no ASCENDING.
    ENDLOOP.
    SORT lt_trkorr_doktl BY version ASCENDING.
    LOOP AT lt_trkorr_doktl INTO ls_trkorr_doktl.
      UNASSIGN <fs_doc>.
      CLEAR lt_lines.
      CLEAR ls_dokt_line.
      APPEND INITIAL LINE TO docs ASSIGNING <fs_doc>.
      <fs_doc>-langu = ls_trkorr_doktl-langu.
      <fs_doc>-version = ls_trkorr_doktl-version.
      LOOP AT ls_trkorr_doktl-doc_lines INTO ls_dokt_line.
        APPEND ls_dokt_line-value TO lt_lines.
      ENDLOOP.
      CONCATENATE LINES OF lt_lines INTO <fs_doc>-value.
    ENDLOOP.
    UNASSIGN <fs_doc>.
    READ TABLE docs WITH KEY version = '0001' langu = 'EN' ASSIGNING <fs_doc>.
    ASSERT <fs_doc> IS ASSIGNED.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = <fs_doc>-value
      IMPORTING
        buffer = manifest
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    ASSERT sy-subrc EQ 0.
  ENDMETHOD.
  METHOD update_packages_table.
    DATA: package TYPE REF TO data,
          data    TYPE REF TO data.
    FIELD-SYMBOLS: <package>   TYPE any,
                   <name>      TYPE any,
                   <registry>  TYPE any,
                   <devclass>  TYPE any,
                   <manifest>  TYPE any,
                   <trkorr>    TYPE any,
                   <integrity> TYPE any.

    DELETE FROM ('/ATRM/PACKAGES') WHERE package_name EQ name AND package_registry EQ 'public'.
    COMMIT WORK AND WAIT.

    CREATE DATA package TYPE ('/ATRM/PACKAGES').
    ASSIGN package->* TO <package>.

    ASSIGN COMPONENT 'PACKAGE_NAME' OF STRUCTURE <package> TO <name>.
    IF sy-subrc EQ 0.
      <name> = name.
    ENDIF.
    ASSIGN COMPONENT 'PACKAGE_REGISTRY' OF STRUCTURE <package> TO <registry>.
    IF sy-subrc = 0.
      <registry> = 'public'.
    ENDIF.
    ASSIGN COMPONENT 'DEVCLASS' OF STRUCTURE <package> TO <devclass>.
    IF sy-subrc EQ 0.
      <devclass> = devclass.
    ENDIF.
    ASSIGN COMPONENT 'TRKORR' OF STRUCTURE <package> TO <trkorr>.
    IF sy-subrc = 0.
      IF name EQ 'trm-server'.
        <trkorr> = server_trkorr.
      ELSE.
        <trkorr> = rest_trkorr.
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'MANIFEST' OF STRUCTURE <package> TO <manifest>.
    IF sy-subrc = 0.
      IF name EQ 'trm-server'.
        <manifest> = get_transport_manifest( server_trkorr ).
      ELSE.
        <manifest> = get_transport_manifest( rest_trkorr ).
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'INTEGRITY' OF STRUCTURE <package> TO <integrity>.
    IF sy-subrc = 0.
      <integrity> = integrity.
    ENDIF.

    INSERT ('/ATRM/PACKAGES') FROM <package>.
    COMMIT WORK AND WAIT.
  ENDMETHOD.
  METHOD execute_post_activities.
*    TYPES: BEGIN OF ty_manifest,
*             dummy TYPE flag,
*           END OF ty_manifest.
*    DATA ls_manifest TYPE ty_manifest.
*    CALL TRANSFORMATION id SOURCE XML manifest RESULT trm_manifest = ls_manifest.
    "TODO
  ENDMETHOD.
  METHOD activate_rest_sicf.
    TRY.
        CALL METHOD ('CL_ICF_TREE')=>activate_node
          EXPORTING
            url                      = '/ztrmserver'
            hostname                 = 'DEFAULT_HOST'
          EXCEPTIONS
            node_not_existing        = 1
            enqueue_error            = 2
            no_authority             = 3
            url_and_nodeguid_space   = 4
            url_and_nodeguid_fill_in = 5
            OTHERS                   = 6.
        IF sy-subrc <> 0.
          display_error( 'SICF node "/ztrmserver" is not active! Activate it manually.' ).
          WRITE: /, 'SICF node "/ztrmserver" is not active! Activate it manually.'.
        ENDIF.
      CATCH cx_sy_dyn_call_error.
        display_error( 'SICF node "/ztrmserver" is not active! Activate it manually.' ).
        WRITE: /, 'SICF node "/ztrmserver" is not active! Activate it manually.'.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_release.
    TYPES: BEGIN OF ty_manifest,
             name     TYPE string,
             registry TYPE string,
           END OF ty_manifest.
    DATA:
      release_manifest     TYPE ty_manifest,
      release_entry        TYPE string,
      data_file_name       TYPE string,
      header_file_name     TYPE string,
      data_file_path       TYPE string,
      header_file_path     TYPE string,
      log_text             TYPE string,
      file_error           TYPE string,
      file_written         TYPE flag,
      data_file            TYPE xstring,
      header_file          TYPE xstring,
      zip                  TYPE REF TO cl_abap_zip,
      transport_files      TYPE xstring,
      filesys              TYPE filesys,
      file_path_separator  TYPE c,
      dirtrans             TYPE pfevalue,
      tmssysnam            TYPE tmssysnam,
      transport_id         TYPE trkorr,
      subrc                TYPE i,
      e071                 TYPE STANDARD TABLE OF tadir,
      tadir                TYPE STANDARD TABLE OF tadir,
      tadir_line           LIKE LINE OF tadir,
      devclass             TYPE STANDARD TABLE OF tadir,
      devclass_line        LIKE LINE OF devclass,
      devclass_exists      TYPE STANDARD TABLE OF tdevc,
      packages             TYPE ty_package_tab,
      hierarchy            TYPE ty_pkg_node_tab,
      node                 LIKE LINE OF hierarchy,
      sap_package          TYPE scompkdtln,
      sap_package_instance TYPE REF TO if_package,
      tpstdout             TYPE t_tpstdout,
      tpstdout_line        LIKE LINE OF tpstdout,
      log                  TYPE string,
      dummy_message        TYPE string,
      hash_error           TYPE string,
      hash_exception       TYPE REF TO cx_root.

    CLEAR: installed, integrity, manifest.
    transport_id = trkorr.

    get_file_sys(
      IMPORTING
        file_sys = filesys
    ).
    WRITE: /, 'System: ', filesys-filesys.
    IF filesys-filesys EQ 'WINDOWS NT' OR filesys EQ 'DOS'.
      file_path_separator = '\'.
    ELSEIF filesys-filesys EQ 'UNIX' OR filesys-filesys EQ 'AS\400' OR filesys-filesys EQ 'MACINTOSH' OR filesys-filesys EQ 'MPE' OR filesys-filesys EQ 'VMS'.
      file_path_separator = '/'.
    ELSE.
      display_error( 'Cannot get system file path separator' ).
      RETURN.
    ENDIF.
    get_dir_trans( IMPORTING dir_trans = dirtrans ).
    CONCATENATE 'dist/' trkorr INTO release_entry.
    CONCATENATE 'R' trkorr+4(6) '.' trkorr(3) INTO data_file_name.
    CONCATENATE 'K' trkorr+4(6) '.' trkorr(3) INTO header_file_name.
    CONCATENATE dirtrans file_path_separator 'data' file_path_separator data_file_name INTO data_file_path.
    CONCATENATE dirtrans file_path_separator 'cofiles' file_path_separator header_file_name INTO header_file_path.
    WRITE: /, 'Release data: ', data_file_name, ' Release header: ', header_file_name.
    " CL_ABAP_MESSAGE_DIGEST (kernel SHA512) doesn't exist on older releases (e.g. 7.0),
    " and CALCULATE_HASH_FOR_RAW can't do SHA512 (HASHALG is CHAR4): only when the
    " class is missing, fall back to the plain ABAP implementation in LCL_SHA512
    TRY.
        CALL METHOD ('CL_ABAP_MESSAGE_DIGEST')=>('CALCULATE_HASH_FOR_RAW')
          EXPORTING
            if_algorithm     = 'SHA512'
            if_data          = release
          IMPORTING
            ef_hashb64string = integrity.
      CATCH cx_sy_dyn_call_error.
        write_log( 'Kernel SHA512 not available, calculating release checksum in ABAP...' ).
        integrity = lcl_sha512=>hash_b64( release ).
      CATCH cx_root INTO hash_exception.
        hash_error = hash_exception->get_text( ).
        CONCATENATE 'CL_ABAP_MESSAGE_DIGEST:' hash_error INTO hash_error SEPARATED BY space.
        write_log( hash_error ).
        display_error( 'Error in release checksum' ).
        RETURN.
    ENDTRY.
    IF integrity IS INITIAL.
      display_error( 'Error in release checksum' ).
      RETURN.
    ENDIF.
    IF checksum IS NOT INITIAL.
      WRITE: /, 'Verifying release integrity, to match', / checksum.
      IF integrity <> checksum.
        display_error( 'Release checksum does not match!' ).
        RETURN.
      ENDIF.
      write_log( 'Release integrity verified.' ).
    ENDIF.
    CREATE OBJECT zip.
    zip->load(
      EXPORTING
        zip = release
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2
    ).
    IF sy-subrc <> 0.
      display_error( 'Error in release content' ).
      RETURN.
    ENDIF.
    zip->get(
      EXPORTING
        name = 'manifest.json'
      IMPORTING
        content = manifest
      EXCEPTIONS
        zip_decompression_error = 1
        zip_index_error         = 2
        OTHERS                  = 3
    ).
    IF manifest IS INITIAL OR sy-subrc <> 0.
      log_missing_zip_entry( zip = zip expected = 'manifest.json' context = 'release' prefix = 'dist/' ).
      display_error( 'Error in release content: manifest not found' ).
      RETURN.
    ENDIF.
    TRY.
        CALL METHOD ('/UI2/CL_JSON')=>deserialize
          EXPORTING
            jsonx = manifest
          CHANGING
            data  = release_manifest.
      CATCH cx_sy_dyn_call_error.
        WRITE: /, 'Package manifest was not validated: UI add-on/SAP_UI is missing?, install continues.'.
    ENDTRY.
    IF release_manifest IS NOT INITIAL.
      IF release_manifest-name <> name.
        display_error( 'Error in release content: invalid manifest' ).
        WRITE: /, 'Expected manifest name to be trm-server or trm-rest but found', release_manifest-name.
        RETURN.
      ENDIF.
      IF release_manifest-registry IS NOT INITIAL.
        display_error( 'Error in release content: registry field should be empty' ).
        WRITE: /, 'Expected manifest registry field to be empty but found', release_manifest-registry.
        RETURN.
      ENDIF.
    ENDIF.
    zip->get(
      EXPORTING
        name = release_entry
      IMPORTING
        content = transport_files
      EXCEPTIONS
        zip_decompression_error = 1
        zip_index_error         = 2
        OTHERS                  = 3
    ).
    IF transport_files IS INITIAL OR sy-subrc <> 0.
      log_missing_zip_entry( zip = zip expected = release_entry context = 'release' prefix = 'dist/' ).
      write_log( 'Make sure the file is an installer release of trm-server/trm-rest (e.g. downloaded from trmregistry.com).' ).
      display_error( 'Error in release content: transport files not found' ).
      RETURN.
    ENDIF.
    zip->load(
      EXPORTING
        zip = transport_files
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2
    ).
    IF sy-subrc <> 0.
      display_error( 'Error in release content' ).
      RETURN.
    ENDIF.
    zip->get(
      EXPORTING
        name = header_file_name
      IMPORTING
        content = header_file
      EXCEPTIONS
        zip_decompression_error = 1
        zip_index_error         = 2
        OTHERS                  = 3
    ).
    IF header_file IS INITIAL OR sy-subrc <> 0.
      log_missing_zip_entry( zip = zip expected = header_file_name context = release_entry ).
      display_error( 'Error in release content: header file not found' ).
      RETURN.
    ENDIF.
    zip->get(
      EXPORTING
        name = data_file_name
      IMPORTING
        content = data_file
      EXCEPTIONS
        zip_decompression_error = 1
        zip_index_error         = 2
        OTHERS                  = 3
    ).
    IF data_file IS INITIAL OR sy-subrc <> 0.
      log_missing_zip_entry( zip = zip expected = data_file_name context = release_entry ).
      display_error( 'Error in release content: data file not found' ).
      RETURN.
    ENDIF.
    tmssysnam = sy-sysid.

    WRITE: /, 'Deleting', trkorr, 'from TMS queue (if exists)...'.
    delete_from_tms_queue(
      EXPORTING
        trkorr  = trkorr
        system  = tmssysnam
      IMPORTING
        subrc   = subrc
    ).
    IF subrc <> 0.
      display_error( 'Error deleting transport from TMS queue' ).
      RETURN.
    ENDIF.

    write_log( 'All files extracted from release, ready to import into application server.' ).
    CONCATENATE 'Release data:' data_file_path 'Release header:' header_file_path INTO log_text SEPARATED BY space.
    write_log( log_text ).

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = 'Copying release files into application server'.

    CALL METHOD write_binary_file
      EXPORTING
        file_path = header_file_path
        file      = header_file
      IMPORTING
        written   = file_written
        error     = file_error.
    IF file_written <> 'X'.
      CONCATENATE 'Cannot write' header_file_path ':' file_error INTO log_text SEPARATED BY space.
      write_log( log_text ).
      display_error( 'Error writing transport header file.' ).
      RETURN.
    ENDIF.
    CALL METHOD write_binary_file
      EXPORTING
        file_path = data_file_path
        file      = data_file
      IMPORTING
        written   = file_written
        error     = file_error.
    IF file_written <> 'X'.
      CONCATENATE 'Cannot write' data_file_path ':' file_error INTO log_text SEPARATED BY space.
      write_log( log_text ).
      display_error( 'Error writing transport data file.' ).
      RETURN.
    ENDIF.

    WRITE: /, 'Forwarding', trkorr, '...'.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = 'Forwarding'.

    forward(
      EXPORTING
        trkorr       = trkorr
        target       = tmssysnam
        source       = tmssysnam
        import_again = 'X'
      IMPORTING
        subrc         = subrc
    ).
    IF subrc <> 0.
      display_error( 'Error forwarding transport' ).
      RETURN.
    ENDIF.

    WRITE: /, 'Importing', trkorr, '...'.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = 'Importing'.

    import(
      EXPORTING
        trkorr    = trkorr
        system    = tmssysnam
      IMPORTING
         subrc    = subrc
         tpstdout = tpstdout
    ).
    IF tpstdout[] IS NOT INITIAL.
      write_log( '=== R3trans output ===' ).
      LOOP AT tpstdout INTO tpstdout_line.
        log = tpstdout_line-line.
        write_log( log ).
      ENDLOOP.
      write_log( '======================' ).
    ENDIF.
    IF subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO dummy_message.
      write_log( dummy_message ).
      display_error( 'Error importing transport, check logs!' ).
      RETURN.
    ENDIF.

    WRITE: /, 'Import of transport', trkorr, 'completed.'.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = 'Generating SAP packages'.

    SELECT pgmid object obj_name FROM e071 INTO CORRESPONDING FIELDS OF TABLE e071
      WHERE trkorr EQ transport_id AND pgmid EQ 'R3TR'.
    IF e071[] IS NOT INITIAL.
      SELECT pgmid object obj_name devclass
        FROM tadir
        INTO CORRESPONDING FIELDS OF TABLE tadir
        FOR ALL ENTRIES IN e071
        WHERE pgmid EQ e071-pgmid
          AND object EQ e071-object
          AND obj_name EQ e071-obj_name.
      MOVE tadir[] TO devclass[].
      SORT devclass BY devclass ASCENDING.
      DELETE ADJACENT DUPLICATES FROM devclass COMPARING devclass.
      LOOP AT devclass INTO devclass_line.
        APPEND devclass_line-devclass TO packages.
      ENDLOOP.
      hierarchy = rebuild_hierarchy( packages ).
      SELECT devclass
        FROM tdevc
        INTO TABLE devclass_exists
        FOR ALL ENTRIES IN hierarchy
        WHERE devclass EQ hierarchy-package.
      LOOP AT hierarchy INTO node.
        READ TABLE devclass_exists TRANSPORTING NO FIELDS WITH KEY devclass = node-package.
        CHECK sy-subrc <> 0.
        WRITE: /, 'Creating package', node-package.
        CLEAR sap_package.
        CLEAR sap_package_instance.
        sap_package-devclass = node-package.
        sap_package-parentcl = node-parent.
        sap_package-ctext = 'TRM (Transport Request Manager)'.
        sap_package-as4user = sy-uname.
        sap_package-dlvunit = 'LOCAL'.
        sap_package-masterlang = sy-langu.
        cl_package_factory=>create_new_package(
          EXPORTING
            i_reuse_deleted_object       = abap_true
          IMPORTING
            e_package                    = sap_package_instance
          CHANGING
            c_package_data               = sap_package
          EXCEPTIONS
            object_already_existing      = 1
            object_just_created          = 2
            not_authorized               = 3
            wrong_name_prefix            = 4
            undefined_name               = 5
            reserved_local_name          = 6
            invalid_package_name         = 7
            short_text_missing           = 8
            software_component_invalid   = 9
            layer_invalid                = 10
            author_not_existing          = 11
            component_not_existing       = 12
            component_missing            = 13
            prefix_in_use                = 14
            unexpected_error             = 15
            intern_err                   = 16
            no_access                    = 17
            OTHERS                       = 18
        ).
        IF sy-subrc <> 0.
          display_error( 'Error in package create' ).
          RETURN.
        ENDIF.
        sap_package_instance->save( ).
        sap_package_instance->set_changeable( abap_false ).
      ENDLOOP.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = 'Adjusting tadir entries'.

      LOOP AT tadir INTO tadir_line.
        READ TABLE hierarchy INTO node WITH KEY original = tadir_line-devclass.
        CALL FUNCTION 'TRINT_TADIR_MODIFY'
          EXPORTING
            pgmid                = tadir_line-pgmid
            object               = tadir_line-object
            obj_name             = tadir_line-obj_name
            devclass             = node-package
            srcsystem            = 'TRM'
            genflag              = 'X'
          EXCEPTIONS
            object_exists_global = 1
            object_exists_local  = 2
            object_has_no_tadir  = 3
            OTHERS               = 4.
      ENDLOOP.
    ENDIF.
    installed = 'X'.
  ENDMETHOD.

ENDCLASS.

DATA report TYPE REF TO lcl_report.

INITIALIZATION.
  DATA registry_technical_error TYPE string.
  json_supported = lcl_report=>check_json_parser( ).
  vscan_profile_supported = lcl_report=>check_vscan_profile( ).
  lcl_report=>check_registry_certificates(
    IMPORTING
      missing         = install_certificates_visible
      technical_error = registry_technical_error ).
  cert_err               = registry_technical_error.
  cert_btn               = '@48@ Install certificates'.
  sc_titl1               = 'Description'.
  sc_txt1                = 'This report can be used to perform the first installs of trm-server'.
  sc_txt2                = 'and trm-rest.'.
  IF json_supported EQ 'X'.
    sc_txt3              = 'You can either let the report download the latest release from the TRM'.
    sc_txt4              = 'registry or provide a release yourself via file upload.'.
    sc_txt5              = 'To perform online install, registry certificates must be installed.'.
    online               = '@Y4@ From registry'.
  ELSE.
    sc_txt3              = 'You can provide a release via file upload.'.
    json_err             = '@0S@ Online install is not possible: UI add-on/SAP_UI is missing.'.
  ENDIF.
  sc_txt6                = '@X1@ TRM Installer v6.0.0 - RegestaItalia'.
  sc_txt7                = 'Visit trmregistry.com'.
  sc_titl2               = 'Registry connection settings'.
  sc_titl3               = 'Proxy settings (Optional)'.
  %_p_srv_%_app_%-text   = 'Install trm-server'.
  %_p_rest_%_app_%-text  = 'Install trm-rest'.
  offline                = '@FP@ From file'.

  psel-prog      = sy-repid.
  psel-dynnr     = 200.
  psel-activetab = 'TAB2'.

  CREATE OBJECT report.
  DATA:
    confirm_message TYPE string,
    confirm_answer  TYPE c,
    server_version  TYPE string,
    rest_version    TYPE string.
  lcl_report=>get_versions( IMPORTING server = server_version rest = rest_version ).
  IF server_version IS NOT INITIAL OR rest_version IS NOT INITIAL.
    confirm_message = 'Already installed'.
    IF p_srv EQ 'X' AND server_version IS NOT INITIAL.
      CONCATENATE confirm_message 'trm-server' server_version INTO confirm_message SEPARATED BY space.
    ENDIF.
    IF p_rest EQ 'X' AND rest_version IS NOT INITIAL.
      CONCATENATE confirm_message 'trm-rest' rest_version INTO confirm_message SEPARATED BY space.
    ENDIF.
    CONCATENATE confirm_message 'This report should be used for first installs only! Consider upgrading via TRM. Do you want to continue anyway?' INTO confirm_message SEPARATED BY space.
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
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN.
  " replace( ) built-in function is 7.02+, use the classic statement for 7.0
  REPLACE FIRST OCCURRENCE OF REGEX 'http(s?)://' IN p_proxy WITH ''.
  IF sy-ucomm <> 'CERT'.
    CLEAR suppress_certificate_command.
  ENDIF.
  CASE sy-ucomm.
    WHEN 'TAB1'.
      psel-dynnr     = 100.
      psel-activetab = 'TAB1'.
    WHEN 'TAB2'.
      psel-dynnr     = 200.
      psel-activetab = 'TAB2'.
    WHEN 'CERT'.
      IF suppress_certificate_command = 'X'.
        CLEAR: suppress_certificate_command, sy-ucomm, sscrfields-ucomm.
        RETURN.
      ENDIF.
      DATA: confirm_message TYPE string,
            confirm_answer  TYPE c.
      CONCATENATE 'Certificates for' base_url 'will be installed.' INTO confirm_message SEPARATED BY space.
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
      CLEAR: sy-ucomm, sscrfields-ucomm.
      IF confirm_answer = '2'.
        WRITE / 'Installation cancelled by user.'.
        RETURN.
      ELSEIF confirm_answer = '1'.
        IF lcl_report=>install_registry_certificates( ) = 'X'.
          DATA refreshed_registry_error TYPE string.
          lcl_report=>check_registry_certificates(
            IMPORTING
              missing         = install_certificates_visible
              technical_error = refreshed_registry_error ).
          cert_err = refreshed_registry_error.
          suppress_certificate_command = 'X'.
          MESSAGE 'Registry certificates installed successfully' TYPE 'I'.
        ENDIF.
      ENDIF.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  IF json_supported <> 'X' AND psel-activetab = 'TAB1'.
    psel-dynnr     = 200.
    psel-activetab = 'TAB2'.
  ENDIF.
  %_p_id_%_app_%-text     = 'SSL Client Identity'.
  %_p_vscan_%_app_%-text  = 'Use virus scan'.
  %_p_vscanp_%_app_%-text = 'Virus scan profile'.
  %_p_proxy_%_app_%-text  = 'Hostname/IP'.
  %_p_pport_%_app_%-text  = 'Port'.
  %_p_puser_%_app_%-text  = 'Username'.
  %_p_ppwd_%_app_%-text   = 'Password'.
  %_p_lserv_%_app_%-text  = 'trm-server package'.
  %_p_lrest_%_app_%-text  = 'trm-rest package'.
  LOOP AT SCREEN.
    IF screen-group1 EQ 'JSN' AND json_supported <> 'X'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 EQ 'NJS' AND json_supported EQ 'X'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 EQ 'CRT'.
      IF install_certificates_visible EQ 'X'.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name EQ 'P_PPWD'.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF p_vscan EQ 'X'.
      IF screen-name EQ 'P_VSCANP'.
        IF vscan_profile_supported EQ 'X'.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ '%_P_VSCANP_%_APP_%-TEXT'.
        IF vscan_profile_supported EQ 'X'.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-name EQ 'P_VSCANP'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ '%_P_VSCANP_%_APP_%-TEXT'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF p_srv EQ 'X'.
      IF screen-name EQ 'P_LSERV'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ '%_P_LSERV_%_APP_%-TEXT'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-name EQ 'P_LSERV'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ '%_P_LSERV_%_APP_%-TEXT'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF p_rest EQ 'X'.
      IF screen-name EQ 'P_LREST'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ '%_P_LREST_%_APP_%-TEXT'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-name EQ 'P_LREST'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ '%_P_LREST_%_APP_%-TEXT'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.
  report->run( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lserv.
  PERFORM choose_file CHANGING p_lserv.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lrest.
  PERFORM choose_file CHANGING p_lrest.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vscanp.
  DATA: lt_return      TYPE STANDARD TABLE OF ddshretval,
        ls_return_line LIKE LINE OF lt_return.
  IF vscan[] IS INITIAL.
    SELECT vscan_prof~profile vscan_proft~text
      FROM vscan_prof
      LEFT OUTER JOIN vscan_proft ON vscan_proft~profile = vscan_prof~profile
                                 AND vscan_proft~spras = sy-langu
      INTO TABLE vscan.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'PROFILE'
      value_org  = 'S'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
    TABLES
      value_tab  = vscan
      return_tab = lt_return.

  IF lt_return IS NOT INITIAL.
    READ TABLE lt_return INDEX 1 INTO ls_return_line.
    p_vscanp = ls_return_line-fieldval.
  ENDIF.

FORM choose_file CHANGING file TYPE rlgrap-filename.
  DATA: lt_filetable TYPE filetable,
        lv_rc        TYPE i,
        lv_action    TYPE i,
        ls_file      LIKE LINE OF lt_filetable.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select a release'
      file_filter             = '*.trm'
      multiselection          = abap_false
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
      user_action             = lv_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 3.
        lcl_report=>raise_error( 'File chooser is not available in background/no GUI mode.' ).
      WHEN 4.
        lcl_report=>raise_error( 'File chooser is not supported by this GUI.' ).
      WHEN OTHERS.
        lcl_report=>raise_error( 'Error opening file chooser.' ).
    ENDCASE.
    RETURN.
  ENDIF.

  IF lv_action <> cl_gui_frontend_services=>action_ok OR lv_rc <= 0.
    RETURN.
  ENDIF.

  READ TABLE lt_filetable INDEX 1 INTO ls_file.
  IF sy-subrc = 0.
    file = ls_file-filename.
  ENDIF.
ENDFORM.

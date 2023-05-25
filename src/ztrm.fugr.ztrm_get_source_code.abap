FUNCTION ztrm_get_source_code.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PACKAGE) TYPE  DEVCLASS
*"  EXPORTING
*"     VALUE(EV_ZIP) TYPE  XSTRING
*"  EXCEPTIONS
*"      EXCEPTION_RAISED
*"----------------------------------------------------------------------
  DATA lo_repo_srv TYPE REF TO object.

  DATA lr_repo TYPE REF TO data.
  DATA lo_repo TYPE REF TO object.
  FIELD-SYMBOLS <fs_repo> TYPE any.

  DATA lr_dot_abapgit TYPE REF TO data.
  DATA lo_dot_abapgit TYPE REF TO object.
  FIELD-SYMBOLS <fs_dot_abapgit> TYPE any.

  DATA lr_local_settings TYPE REF TO data.
  FIELD-SYMBOLS <fs_local_settings> TYPE any.


  TRY.
      "Keep dynamic - avoid abapGit dependency
      CREATE DATA lr_repo TYPE REF TO ('ZIF_ABAPGIT_REPO').
      ASSIGN lr_repo->* TO <fs_repo>.

      CREATE DATA lr_dot_abapgit TYPE REF TO ('ZCL_ABAPGIT_DOT_ABAPGIT').
      ASSIGN lr_dot_abapgit->* TO <fs_dot_abapgit>.

      CREATE DATA lr_local_settings TYPE ('ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-LOCAL_SETTINGS').
      ASSIGN lr_local_settings->* TO <fs_local_settings>.

      CALL METHOD ('ZCL_ABAPGIT_REPO_SRV')=>('GET_INSTANCE')
        RECEIVING
          ri_srv = lo_repo_srv.


      CALL METHOD lo_repo_srv->('ZIF_ABAPGIT_REPO_SRV~GET_REPO_FROM_PACKAGE')
        EXPORTING
          iv_package = iv_package
        IMPORTING
          ei_repo    = <fs_repo>.

      lo_repo = <fs_repo>.


      IF lo_repo IS BOUND.
        CALL METHOD lo_repo->('GET_DOT_ABAPGIT')
          RECEIVING
            ro_dot_abapgit = <fs_dot_abapgit>.
        lo_dot_abapgit = <fs_dot_abapgit>.
      ELSE.
        CALL METHOD ('ZCL_ABAPGIT_DOT_ABAPGIT')=>('BUILD_DEFAULT')
          RECEIVING
            ro_dot_abapgit = <fs_dot_abapgit>.
        lo_dot_abapgit = <fs_dot_abapgit>.

        CALL METHOD lo_dot_abapgit->('SET_FOLDER_LOGIC')
          EXPORTING
            iv_logic = 'PREFIX'.
      ENDIF.

      CALL METHOD ('ZCL_ABAPGIT_ZIP')=>('EXPORT')
        EXPORTING
          is_local_settings = <fs_local_settings>
          iv_package        = iv_package
          io_dot_abapgit    = <fs_dot_abapgit>
          iv_show_log       = abap_false
        RECEIVING
          rv_xstr           = ev_zip.
    CATCH cx_root.
      RAISE exception_raised.
  ENDTRY.
ENDFUNCTION.

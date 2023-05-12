function ztrm_set_published_package.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_TRM_PUBLISH) TYPE  ZTRM_PUBLISH
*"  EXCEPTIONS
*"      MODIFY_ERROR
*"----------------------------------------------------------------------
  MODIFY ztrm_publish FROM is_trm_publish.
  COMMIT WORK AND WAIT.

  IF sy-subrc NE 0.
    RAISE modify_error.
  ENDIF.



ENDFUNCTION.

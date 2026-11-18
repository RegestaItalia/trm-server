CLASS ltc_object DEFINITION DEFERRED.
CLASS /atrm/cl_object DEFINITION LOCAL FRIENDS ltc_object.

CLASS ltc_object DEFINITION FOR TESTING
  "#AU Risk_Level Harmless
  "#AU Duration Short
.
  PRIVATE SECTION.
    METHODS tadir_missing_message FOR TESTING.
    METHODS tfdir_missing_message FOR TESTING.
ENDCLASS.

CLASS ltc_object IMPLEMENTATION.

  METHOD tadir_missing_message.
    " Message built with CONCATENATE ... SEPARATED BY space (no && on 7.00)
    DATA lo_exception TYPE REF TO /atrm/cx_exception.
    TRY.
        /atrm/cl_object=>get_tadir_dependency(
          object   = 'PROG'
          obj_name = '/ATRM/ZZ_NOT_EXISTING' ).
      CATCH /atrm/cx_exception INTO lo_exception.
    ENDTRY.
    cl_aunit_assert=>assert_bound( act = lo_exception msg = 'Exception expected' ).
    cl_aunit_assert=>assert_equals(
      act = lo_exception->message-msgv1
      exp = 'R3TR PROG /ATRM/ZZ_NOT_EXISTING is not in TADIR' ).
  ENDMETHOD.

  METHOD tfdir_missing_message.
    DATA lo_exception TYPE REF TO /atrm/cx_exception.
    TRY.
        /atrm/cl_object=>get_tfdir_dependency( funcname = '/ATRM/ZZ_NONE' ).
      CATCH /atrm/cx_exception INTO lo_exception.
    ENDTRY.
    cl_aunit_assert=>assert_bound( act = lo_exception msg = 'Exception expected' ).
    cl_aunit_assert=>assert_equals(
      act = lo_exception->message-msgv1
      exp = 'Cannot find function group of /ATRM/ZZ_NONE' ).
  ENDMETHOD.

ENDCLASS.

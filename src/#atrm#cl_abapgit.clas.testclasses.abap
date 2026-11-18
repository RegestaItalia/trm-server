CLASS ltc_abapgit DEFINITION FOR TESTING
  "#AU Risk_Level Harmless
  "#AU Duration Short
.
  PRIVATE SECTION.
    METHODS dot_abapgit_missing_repo FOR TESTING.
ENDCLASS.

CLASS ltc_abapgit IMPLEMENTATION.

  METHOD dot_abapgit_missing_repo.
    " Message built with CONCATENATE ... SEPARATED BY space (no && on 7.00)
    DATA lo_exception TYPE REF TO /atrm/cx_exception.
    TRY.
        /atrm/cl_abapgit=>get_dot_abapgit( devclass = '/ATRM/ZZ_NONE' ).
      CATCH /atrm/cx_exception INTO lo_exception.
    ENDTRY.
    cl_aunit_assert=>assert_bound( act = lo_exception msg = 'Exception expected' ).
    cl_aunit_assert=>assert_equals(
      act = lo_exception->reason( )
      exp = /atrm/cx_exception=>c_reason-abapgit_intergration ).
    cl_aunit_assert=>assert_equals(
      act = lo_exception->message-msgv1
      exp = 'Repository for package /ATRM/ZZ_NONE not found' ).
  ENDMETHOD.

ENDCLASS.

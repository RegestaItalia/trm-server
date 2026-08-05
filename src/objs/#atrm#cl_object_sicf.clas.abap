CLASS /atrm/cl_object_sicf DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ty_icf_name   TYPE c LENGTH 15,
           ty_icfparguid TYPE c LENGTH 25,
           ty_icf_hand   TYPE c LENGTH 32.
    METHODS single_dependencies
      IMPORTING
        !serv_info    TYPE REF TO data
      CHANGING
        !dependencies TYPE /atrm/object_dependency_t.
ENDCLASS.



CLASS /atrm/cl_object_sicf IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: icf_name   TYPE ty_icf_name,
          icfparguid TYPE ty_icfparguid,
          serv_info  TYPE REF TO data.
    FIELD-SYMBOLS: <serv_info>  TYPE ANY TABLE.
    icf_name = me->key-obj_name(15).
    icfparguid = me->key-obj_name+15.
    CREATE DATA serv_info TYPE ('ICFSERVTBL').
    ASSIGN serv_info->* TO <serv_info>.
    CALL METHOD ('CL_ICF_TREE')=>('IF_ICF_TREE~GET_INFO_FROM_SERV')
      EXPORTING
        icf_name          = icf_name
        icfparguid        = icfparguid
      IMPORTING
        serv_info         = <serv_info>
      EXCEPTIONS
        wrong_name        = 1
        wrong_parguid     = 2
        incorrect_service = 3
        no_authority      = 4
        OTHERS            = 5.
    CHECK sy-subrc EQ 0.
    single_dependencies(
      EXPORTING
        serv_info = serv_info
      CHANGING
        dependencies = dependencies
    ).
    CLEAR icfparguid.
    WHILE icfparguid IS NOT INITIAL.
      " repeat for all parent services
    ENDWHILE.
  ENDMETHOD.

  METHOD single_dependencies.
    DATA: handlertbl TYPE REF TO data,
          dependency TYPE /atrm/object_dependency.
    FIELD-SYMBOLS: <serv_info>  TYPE ANY TABLE,
                   <info>       TYPE any,
                   <handlertbl> TYPE ANY TABLE,
                   <handler>    TYPE any,
                   <icfhandler> TYPE ty_icf_hand.
    CHECK serv_info IS BOUND.
    ASSIGN serv_info->* TO <serv_info>.
    CHECK sy-subrc EQ 0.
    LOOP AT <serv_info> ASSIGNING <info>.
      ASSIGN COMPONENT 'HANDLERTBL' OF STRUCTURE <info> TO <handlertbl>.
      CHECK sy-subrc EQ 0.
      LOOP AT <handlertbl> ASSIGNING <handler>.
        UNASSIGN <icfhandler>.
        ASSIGN COMPONENT 'ICFHANDLER' OF STRUCTURE <handler> TO <icfhandler>.
        CHECK sy-subrc EQ 0.
        CLEAR dependency.
        TRY.
            get_tadir_dependency(
              EXPORTING
                object     = 'CLAS'
                obj_name   = <icfhandler>
              RECEIVING
                dependency = dependency
            ).
            READ TABLE dependencies WITH KEY tabname = 'TADIR' tabkey = dependency-tabkey TRANSPORTING NO FIELDS.
            CHECK sy-subrc <> 0.
            APPEND dependency TO dependencies.
          CATCH /atrm/cx_exception.
        ENDTRY.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

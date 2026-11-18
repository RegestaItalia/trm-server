FUNCTION-POOL /atrm/server.                 "MESSAGE-ID ..

* INCLUDE /ATRM/LSERVERD...                  " Local class definition

" SAP_BASIS 700 does not load global interfaces implicitly for
" direct access to their components (/atrm/if_server=>version)
INTERFACE /atrm/if_server LOAD.

DATA: go_exc       TYPE REF TO /atrm/cx_exception,
      go_transport TYPE REF TO /atrm/cl_transport,
      go_package   TYPE REF TO /atrm/cl_package,
      go_log       TYPE REF TO /atrm/cl_log_polling.

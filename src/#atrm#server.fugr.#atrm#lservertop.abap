FUNCTION-POOL /atrm/server.                 "MESSAGE-ID ..

* INCLUDE /ATRM/LSERVERD...                  " Local class definition

DATA: go_exc       TYPE REF TO /atrm/cx_exception,
      go_transport TYPE REF TO /atrm/cl_transport,
      go_package   TYPE REF TO /atrm/cl_package,
      go_log       TYPE REF TO /atrm/cl_log_polling.

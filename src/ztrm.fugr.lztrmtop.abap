FUNCTION-POOL ztrm                       MESSAGE-ID sv.

* INCLUDE LZTRMD...                          " Local class definition

DATA: lo_exc TYPE REF TO zcx_trm_exception,
      lo_transport TYPE REF TO zcl_trm_transport,
      lo_package TYPE REF TO zcl_trm_package.

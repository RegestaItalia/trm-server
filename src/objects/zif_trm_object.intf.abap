INTERFACE zif_trm_object
  PUBLIC.

  METHODS get_dependencies
    EXPORTING et_dependencies TYPE ztrm_object_dependency_t.

ENDINTERFACE.

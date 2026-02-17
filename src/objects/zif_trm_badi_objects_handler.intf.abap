INTERFACE zif_trm_badi_objects_handler
  PUBLIC .

  INTERFACES if_badi_interface.

  METHODS change_object_handler
    IMPORTING
      !key     TYPE ztrm_object
    CHANGING
      !handler TYPE REF TO zif_trm_object .
ENDINTERFACE.

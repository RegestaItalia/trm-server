INTERFACE /atrm/if_badi_objs_handler
  PUBLIC .

  INTERFACES if_badi_interface.

  METHODS change_object_handler
    IMPORTING
      !key     TYPE /atrm/object
    CHANGING
      !handler TYPE REF TO /atrm/if_object .
ENDINTERFACE.

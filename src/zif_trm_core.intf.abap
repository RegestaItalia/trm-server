INTERFACE zif_trm_core
  PUBLIC .
  TYPES: BEGIN OF ty_manifest,
           name     TYPE string,
           version  TYPE string,
           registry TYPE string,
         END OF ty_manifest.
ENDINTERFACE.

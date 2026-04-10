INTERFACE /atrm/if_core
  PUBLIC .
  TYPES: BEGIN OF ty_dependency,
           name      TYPE string,
           registry  TYPE string,
           version   TYPE string,
           integrity TYPE string,
         END OF ty_dependency,
         tyt_dependency TYPE STANDARD TABLE OF ty_dependency WITH DEFAULT KEY,
         BEGIN OF ty_manifest,
           name         TYPE string,
           version      TYPE string,
           registry     TYPE string,
           dependencies TYPE tyt_dependency,
         END OF ty_manifest.

  TYPES: t_e071  TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
         BEGIN OF trm_package_data,
           manifest TYPE xstring,
           trkorr   TYPE trkorr,
         END OF trm_package_data.
ENDINTERFACE.

CLASS zcl_trm_object_ddlx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_trm_object .
    DATA: key TYPE ztrm_object READ-ONLY.

    METHODS constructor
      IMPORTING key TYPE ztrm_object.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_object_ddlx IMPLEMENTATION.

  METHOD constructor.
    me->key = key.
  ENDMETHOD.

  METHOD zif_trm_object~get_dependencies.
    DATA: lv_entity_id TYPE if_sadl_entity=>ty_entity_id,
          lo_entity    TYPE REF TO if_sadl_entity.
    lv_entity_id = key-obj_name.
    lo_entity = cl_sadl_entity_factory=>get_instance( )->get_entity(
      iv_id   = lv_entity_id
      iv_type = cl_sadl_entity_factory=>co_type-cds
    ).
    lo_entity->get_annotations(
      IMPORTING
        et_entity_annotations      = data(et_entity_annotations)
        et_element_annotations     = data(et_element_annotations)
        et_action_annotations      = data(et_action_annotations)
        et_association_annotations = data(et_association_annotations)
        et_parameter_annotations   = data(et_parameter_annotations)
    ).
  ENDMETHOD.

ENDCLASS.

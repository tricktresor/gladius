class ZCL_GLDS_DEMO_HELPER definition
  public
  final
  create public .

public section.

  class-methods GET_SIGNATURE_DESCRIPTION
    importing
      !I_OBJECT type ref to ZIF_GLDS_DEMO_TEST_INTERFACE
    returning
      value(E_TEXT) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GLDS_DEMO_HELPER IMPLEMENTATION.


  METHOD get_signature_description.

    DATA lt_params TYPE string_table.


    TRY.
        DATA(lo_class) = cl_oo_class=>get_instance( i_object->get_interface( ) ).
        READ TABLE lo_class->methods INTO DATA(ls_method) INDEX 1.
        IF sy-subrc = 0.
          "information about signature
          APPEND `Signature information` TO lt_params.
          "Importing parameters
          LOOP AT lo_class->method_parameters INTO DATA(ls_param_imp)
          WHERE pardecltyp = 0. "importing
            APPEND |you will get the parameter { ls_param_imp-sconame } (type: { ls_param_imp-type })| TO lt_params.
          ENDLOOP.
          APPEND `Provide your result as described in the text in the following parameters:` TO lt_params.
          LOOP AT lo_class->method_parameters INTO DATA(ls_param_exp)
              WHERE pardecltyp = 1  "exporting
                 OR pardecltyp = 3. "returning
            APPEND |Name: { ls_param_exp-sconame } (type: { ls_param_exp-type })| TO lt_params.
          ENDLOOP.

          LOOP AT lt_params INTO DATA(lv_param).

            CONCATENATE e_text lv_param INTO e_text SEPARATED BY cl_abap_char_utilities=>cr_lf.
          ENDLOOP.
        ENDIF.

      CATCH cx_class_not_existent.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

CLASS zcl_glds_demo_test_cfg DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_glds_demo_command_list .
    INTERFACES zif_glds_demo_description .
    INTERFACES zif_glds_demo_test_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GLDS_DEMO_TEST_CFG IMPLEMENTATION.


  METHOD zif_glds_demo_command_list~get_black_list.

    "Blacklist
    commands = VALUE #(
      ( `->` )
      ( `=>` ) ).

  ENDMETHOD.


  METHOD zif_glds_demo_command_list~get_white_list.

    "Whitelist
    commands = VALUE #(
      ( `FIELD-SYMBOLS`        )

      ( `CHECK`                )
      ( `EXIT`                 )
      ( `RETURN`               )

      ( `DO`                   )
      ( `ENDDO`                )
      ( `WHILE`                )
      ( `ENDWHILE`             )
      ( `CASE`                 )
      ( `WHEN`                 )
      ( `ENDCASE`              )
      ( `IF`                   )
      ( `ELSEIF`               )
      ( `ELSE`                 )
      ( `ENDIF`                )

      ( `MOVE-CORRESPONDING`   )
      ( `ASSIGN`               )
      ( `UNASSIGN`             )
      ( `CLEAR`                )
      ( `FREE`                 )

      ( `FIND`                 )
      ( `REPLACE`              )

      ( `APPEND`               )
      ( `INSERT`               )
      ( `MODIFY`               )
      ( `DELETE`               )
      ( `COLLECT`              )
      ( `READ`                 )
      ( `LOOP`                 )
      ( `ENDLOOP`              )
      ( `SORT`                 ) ).

  ENDMETHOD.


  METHOD zif_glds_demo_description~get.

    CONCATENATE
      `This is a challenge.`
      `Try to find out what formula is used for computing the result.`
      INTO text  SEPARATED BY cl_abap_char_utilities=>cr_lf.

    "Signature
    DATA(signature_description) = zcl_glds_demo_helper=>get_signature_description( me ).
    IF signature_description IS NOT INITIAL.
      CONCATENATE
        text cl_abap_char_utilities=>cr_lf
        signature_description cl_abap_char_utilities=>cr_lf
      INTO text.
    ENDIF.

  ENDMETHOD.


  METHOD zif_glds_demo_test_interface~get_interface.
    rv_interface = 'ZIF_GLDS_DEMO_TEST'.
  ENDMETHOD.


  METHOD zif_glds_demo_test_interface~get_template.
    rv_template = 'ZGLDS_DEMO_TEST_TEMPLATE'.
  ENDMETHOD.
ENDCLASS.

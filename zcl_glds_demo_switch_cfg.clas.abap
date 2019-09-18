class ZCL_GLDS_DEMO_SWITCH_CFG definition
  public
  final
  create public .

public section.

  interfaces ZIF_GLDS_DEMO_COMMAND_LIST .
  interfaces ZIF_GLDS_DEMO_DESCRIPTION .
  interfaces ZIF_GLDS_DEMO_TEST_INTERFACE .
  interfaces ZIF_GLDS_DEMO_METRICS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GLDS_DEMO_SWITCH_CFG IMPLEMENTATION.


  METHOD zif_glds_demo_command_list~get_black_list.

    "Blacklist
    commands = VALUE #(
      ( `->` )
      ( `=>` )
      ( `CASE` )
      ( `FIELD-SYMBOLS`        )

      ( `CHECK`                )
      ( `EXIT`                 )
      ( `RETURN`               )

      ( `DO`                   )
      ( `ENDDO`                )
      ( `WHILE`                )
      ( `ENDWHILE`             )
      ( `CASE`                 )
      ( `ENDCASE`              )
      ( `IF`                   )
      ( `ELSEIF`               )
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
      ).

  ENDMETHOD.


  METHOD zif_glds_demo_command_list~get_white_list.

    "Whitelist
    commands = VALUE #(

      ( `SWITCH`                 )
      ( `WHEN`                 )
      ( `THEN`                 ) ).

  ENDMETHOD.


  METHOD zif_glds_demo_description~get.

    CONCATENATE
      `Your task is to return the number word for the number from 0 to 10.`
      `Any other number should return an emtpy string.`
      `Use the ABAP command SWITCH to fulfill this task.`
    INTO text  SEPARATED BY cl_abap_char_utilities=>cr_lf.

    DATA(signature_description) = zcl_glds_demo_helper=>get_signature_description( me ).
    IF signature_description IS NOT INITIAL.
      CONCATENATE
        text cl_abap_char_utilities=>cr_lf
        signature_description cl_abap_char_utilities=>cr_lf
      INTO text.
    ENDIF.

  ENDMETHOD.


  METHOD zif_glds_demo_metrics~chk.

    zif_glds_demo_metrics~set( ).

    CLEAR result WITH '-'.

    IF metrics IS INITIAL.
      RETURN.
    ENDIF.

    IF metrics-number_of_statements <= zif_glds_demo_metrics~data-number_of_statements.
      result-number_of_statements = abap_true.
    ELSE.
      result-number_of_statements = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD zif_glds_demo_metrics~set.

    zif_glds_demo_metrics~data-number_of_statements = 1.

  ENDMETHOD.


  METHOD ZIF_GLDS_DEMO_TEST_INTERFACE~GET_INTERFACE.
    rv_interface = 'ZIF_GLDS_DEMO_NUMIN_STROUT'.
  ENDMETHOD.


  METHOD zif_glds_demo_test_interface~get_template.
    rv_template = 'ZGLDS_DEMO_SWITCH_TEMPLATE'.
  ENDMETHOD.
ENDCLASS.

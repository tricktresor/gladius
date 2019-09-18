class ZCL_GLDS_DEMO_RANGE_CFG definition
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



CLASS ZCL_GLDS_DEMO_RANGE_CFG IMPLEMENTATION.


  METHOD ZIF_GLDS_DEMO_COMMAND_LIST~GET_BLACK_LIST.

    "Blacklist
    commands = VALUE #(
      ( `->` )
      ( `=>` )

      ( `CHECK`                )
      ( `EXIT`                 )
      ( `RETURN`               )


      ( `MOVE-CORRESPONDING`   )
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
      ).

  ENDMETHOD.


  METHOD ZIF_GLDS_DEMO_COMMAND_LIST~GET_WHITE_LIST.

    "Whitelist
    commands = VALUE #(

      ( `SWITCH` )
      ( `WHEN` )
      ( `THEN` )
      ( `CASE` )
      ( `OTHERS` )
      ( `ENDCASE`              )
      ( `LOOP` )
      ( `ENDLOOP` )
      ( `FIELD-SYMBOLS`        )
      ( `ASSIGN`               )
      ( `DO`                   )
      ( `ENDDO`                )
      ( `WHILE`                )
      ( `ENDWHILE`             )
      ( `IF`                   )
      ( `ELSE` )
      ( `ELSEIF`               )
      ( `ENDIF`                )

      ).

  ENDMETHOD.


  METHOD ZIF_GLDS_DEMO_DESCRIPTION~GET.

    CONCATENATE
      `Your task is to convert the given ranges table into a string.`
      `If the SIGN is "I" the text has to be "including".`
      `If the SIGN is 'E' the text has to be "excluding".`
      `If the parameter HIGH is not empty then you have to separate LOW and HIGH by "to" `
      cl_abap_char_utilities=>cr_lf
      `EXAMPLE: SIGN: I, OPTION: BT, LOW = 20000504, HIGH: 20010607`
      `RESULT: including 04.05.2000 to 07.06.2001`
    INTO text  SEPARATED BY cl_abap_char_utilities=>cr_lf.

    DATA(signature_description) = zcl_glds_demo_helper=>get_signature_description( me ).
    IF signature_description IS NOT INITIAL.
      CONCATENATE
        text cl_abap_char_utilities=>cr_lf
        signature_description cl_abap_char_utilities=>cr_lf
      INTO text.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_GLDS_DEMO_METRICS~CHK.

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


  METHOD ZIF_GLDS_DEMO_METRICS~SET.

    zif_glds_demo_metrics~data-number_of_statements = 1.

  ENDMETHOD.


  METHOD ZIF_GLDS_DEMO_TEST_INTERFACE~GET_INTERFACE.
    rv_interface = 'ZIF_GLDS_DEMO_RANGE'.
  ENDMETHOD.


  METHOD zif_glds_demo_test_interface~get_template.
    rv_template = 'ZGLDS_DEMO_RANGE_TEMPLATE'.
  ENDMETHOD.
ENDCLASS.

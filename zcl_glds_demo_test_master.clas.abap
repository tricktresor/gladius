class ZCL_GLDS_DEMO_TEST_MASTER definition
  public
  final
  create public .

public section.

  interfaces ZIF_GLDS_DEMO_TEST .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GLDS_DEMO_TEST_MASTER IMPLEMENTATION.


  METHOD zif_glds_demo_test~test_me.

    IF in = 0.
      out = 0.
      RETURN.
    ENDIF.

    out = in * ( in - 1 ).


  ENDMETHOD.
ENDCLASS.

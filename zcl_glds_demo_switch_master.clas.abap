class ZCL_GLDS_DEMO_SWITCH_MASTER definition
  public
  final
  create public .

public section.

  interfaces ZIF_GLDS_DEMO_NUMIN_STROUT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GLDS_DEMO_SWITCH_MASTER IMPLEMENTATION.


  METHOD zif_glds_demo_numin_strout~test_me.

    out = SWITCH string(
            in
             WHEN  0 THEN `zero`
             WHEN  1 THEN `one`
             WHEN  2 THEN `two`
             WHEN  3 THEN `three`
             WHEN  4 THEN `four`
             WHEN  5 THEN `five`
             WHEN  6 THEN `six`
             WHEN  7 THEN `seven`
             WHEN  8 THEN `eight`
             WHEN  9 THEN `nine`
             WHEN 10 THEN `ten`
             ELSE `` ).

  ENDMETHOD.
ENDCLASS.

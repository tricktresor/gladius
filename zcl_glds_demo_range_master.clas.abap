class ZCL_GLDS_DEMO_RANGE_MASTER definition
  public
  final
  create public .

public section.

  interfaces ZIF_GLDS_DEMO_RANGE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GLDS_DEMO_RANGE_MASTER IMPLEMENTATION.


  METHOD zif_glds_demo_range~test_me.

    text = REDUCE string( INIT t = `` FOR line IN range
            NEXT t = |{ t }{ SWITCH #( t WHEN `` THEN `` ELSE cl_abap_char_utilities=>cr_lf ) }{
                        SWITCH #( line-sign
                           WHEN 'I' THEN `including`
                           ELSE `excluding` ) } {
                        SWITCH #( line-high
                           WHEN '00000000' THEN |{ line-low DATE = USER }|
                           ELSE |{ line-low DATE = USER } to { line-high DATE = USER }| ) }| ) .

  ENDMETHOD.
ENDCLASS.

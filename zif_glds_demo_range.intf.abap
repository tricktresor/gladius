interface ZIF_GLDS_DEMO_RANGE
  public .


  types:
    tt_range TYPE RANGE OF dats .
  types:
    ts_range type LINE OF tt_range .

  methods TEST_ME
    importing
      !RANGE type TT_RANGE
    returning
      value(TEXT) type STRING .
endinterface.

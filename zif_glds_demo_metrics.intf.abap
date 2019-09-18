interface ZIF_GLDS_DEMO_METRICS
  public .


  types:
    BEGIN OF ts_metrics_chk,
      number_of_statements TYPE abap_bool,
      number_of_variables  TYPE abap_bool,
      number_of_words      TYPE abap_bool,
      number_of_tokens     TYPE abap_bool,
      number_of_characters TYPE abap_bool,
      runtime              TYPE abap_bool,
    END OF ts_metrics_chk .
  types:
    BEGIN OF ts_metrics,
      number_of_statements TYPE i,
      used_statements      TYPE string_table,
      number_of_variables  TYPE i,
      number_of_words      TYPE i,
      number_of_tokens     TYPE i,
      number_of_characters TYPE i,
      runtime              TYPE i,
    END OF ts_metrics .

  data DATA type TS_METRICS .

  methods CHK
    importing
      !METRICS type TS_METRICS
    returning
      value(RESULT) type TS_METRICS_CHK .
  methods SET .
endinterface.

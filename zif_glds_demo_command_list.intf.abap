interface ZIF_GLDS_DEMO_COMMAND_LIST
  public .


  methods GET_WHITE_LIST
    returning
      value(COMMANDS) type STRING_TABLE .
  methods GET_BLACK_LIST
    returning
      value(COMMANDS) type STRING_TABLE .
endinterface.

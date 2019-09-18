REPORT zglds_demo_switch_template.

CLASS lcl_solution DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_glds_demo_numin_strout.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS lcl_solution IMPLEMENTATION.
  METHOD zif_glds_demo_numin_strout~test_me.

* implementation

  ENDMETHOD.
ENDCLASS.

"* use this source file for your ABAP unit test classes
*UT:CLASS lcl_test DEFINITION
*UT:  INHERITING FROM zcl_glds_demo_switch_tests
*UT:  FOR TESTING
*UT:  RISK LEVEL HARMLESS
*UT:  DURATION SHORT.
*UT:ENDCLASS.

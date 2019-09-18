REPORT zglds_demo_test_template.

CLASS lcl_solution DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_glds_demo_test .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS lcl_solution IMPLEMENTATION.
  METHOD zif_glds_demo_test~test_me.

* implementation

  ENDMETHOD.
ENDCLASS.

"* use this source file for your ABAP unit test classes
*UT:CLASS lcl_test DEFINITION
*UT:  INHERITING FROM zcl_glds_demo_test_units
*UT:  FOR TESTING
*UT:  RISK LEVEL HARMLESS
*UT:  DURATION SHORT.
*UT:ENDCLASS.

"* use this source file for your SECRET unit test classes
*ST:CLASS lcl_secret_test DEFINITION
*ST:  INHERITING FROM zcl_glds_demo_secret_tests
*ST:  FOR TESTING
*ST:  RISK LEVEL HARMLESS
*ST:  DURATION SHORT.
*ST:ENDCLASS.

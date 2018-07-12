REPORT zglds_demo_template.

CLASS lcl_solution DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_glds_demo_test .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS lcl_solution IMPLEMENTATION.
  METHOD zif_glds_demo_test~test_me.

* declarations
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
*??:CLASS lcl_secret_test DEFINITION
*??:  INHERITING FROM zcl_glds_demo_secret_tests
*??:  FOR TESTING
*??:  RISK LEVEL HARMLESS
*??:  DURATION SHORT.
*??:ENDCLASS.

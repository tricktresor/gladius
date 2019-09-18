class ZCL_GLDS_DEMO_SECRET_TESTS definition
  public
  inheriting from ZCL_GLDS_DEMO_TEST_HELPER
  abstract
  create public
  for testing
  duration short
  risk level harmless .

public section.

  methods CONSTRUCTOR .
  methods SECRET_TEST_1
  for testing .
  methods SECRET_TEST_2
  for testing .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_class_to_test TYPE REF TO zif_glds_demo_test .
ENDCLASS.



CLASS ZCL_GLDS_DEMO_SECRET_TESTS IMPLEMENTATION.


  METHOD CONSTRUCTOR.
    super->constructor( ).
    mo_class_to_test ?= mo_class_to_test_generic.
  ENDMETHOD.


  METHOD secret_test_1.
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = mo_class_to_test->test_me( 1 )
      msg = 'Secret test 1 failed. try to think about it...' ).
  ENDMETHOD.


  METHOD secret_test_2.
    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = mo_class_to_test->test_me( 2 )
      msg = 'Secret test 2 failed. Maybe you should think harder... ;)' ).
  ENDMETHOD.
ENDCLASS.

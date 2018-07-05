class ZCL_GLDS_DEMO_TEST_UNITS definition
  public
  inheriting from ZCL_GLDS_DEMO_TEST_HELPER
  abstract
  create public
  for testing
  duration short
  risk level harmless .

public section.

  methods CONSTRUCTOR .
  methods TEST_0
  for testing .
  methods TEST_1
  for testing .
  methods TEST_2
  for testing .
  methods TEST_3
  for testing .
  methods TEST_10
  for testing .
  methods TEST_99
  for testing .
  methods TEST_RANDOM
  for testing .
  methods TEST_DUMMY
  for testing .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_class_to_test TYPE REF TO zif_glds_demo_test .
ENDCLASS.



CLASS ZCL_GLDS_DEMO_TEST_UNITS IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mo_class_to_test ?= mo_class_to_test_generic.
  ENDMETHOD.


  METHOD test_0.
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = mo_class_to_test->test_me( 0 )
      msg = '0 should be 0' ).
  ENDMETHOD.


  METHOD test_1.
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = mo_class_to_test->test_me( 1 )
      msg = '1 should be 0' ).
  ENDMETHOD.


  METHOD test_10.
    cl_abap_unit_assert=>assert_equals(
      exp = 90
      act = mo_class_to_test->test_me( 10 )
      msg = '10 should be 90' ).
  ENDMETHOD.


  METHOD test_2.
    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = mo_class_to_test->test_me( 2 )
      msg = '2 should be 2' ).
  ENDMETHOD.


  METHOD test_3.
    cl_abap_unit_assert=>assert_equals(
      exp = 6
      act = mo_class_to_test->test_me( 3 )
      msg = '3 should be 6' ).
  ENDMETHOD.


  METHOD test_99.
    cl_abap_unit_assert=>assert_equals(
      exp = 9702
      act = mo_class_to_test->test_me( 99 )
      msg = '99 should be 9702' ).
  ENDMETHOD.


  METHOD test_dummy.

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = 0
      msg = |Don't worry - This test always fails to display all other results. |
      level = if_aunit_constants=>tolerable ).

  ENDMETHOD.


  METHOD test_random.
    DATA(rnd) = cl_abap_random_int=>create( seed = 42 min = 10 max = 10000 )->get_next( ).
    DATA(result) = rnd * ( rnd - 1 ).
    cl_abap_unit_assert=>assert_equals(
      exp = result
      act = mo_class_to_test->test_me( rnd )
      msg = |{ rnd } should be { result }| ).
  ENDMETHOD.
ENDCLASS.

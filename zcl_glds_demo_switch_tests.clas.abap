CLASS zcl_glds_demo_switch_tests DEFINITION
  PUBLIC
  INHERITING FROM zcl_glds_demo_test_helper
  ABSTRACT
  CREATE PUBLIC
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS .

  PUBLIC SECTION.

    METHODS constructor .
    METHODS test_0  FOR TESTING .
    METHODS test_1  FOR TESTING .
    METHODS test_2  FOR TESTING .
    METHODS test_3  FOR TESTING .
    METHODS test_4  FOR TESTING .
    METHODS test_5  FOR TESTING .
    METHODS test_6  FOR TESTING .
    METHODS test_7  FOR TESTING .
    METHODS test_8  FOR TESTING .
    METHODS test_9  FOR TESTING .
    METHODS test_10 FOR TESTING .
    METHODS test_greater_ten FOR TESTING .
    METHODS test_dummy FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_class_to_test TYPE REF TO zif_glds_demo_numin_strout .
ENDCLASS.



CLASS ZCL_GLDS_DEMO_SWITCH_TESTS IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mo_class_to_test ?= mo_class_to_test_generic.
  ENDMETHOD.


  METHOD test_0.
    cl_abap_unit_assert=>assert_equals(
      exp = `zero`
      act = mo_class_to_test->test_me( 0 )
      msg = '0 should be ZERO' ).
  ENDMETHOD.


  METHOD test_1.
    cl_abap_unit_assert=>assert_equals(
      exp = `one`
      act = mo_class_to_test->test_me( 1 )
      msg = '1 should be ONE' ).
  ENDMETHOD.


  METHOD test_10.
    cl_abap_unit_assert=>assert_equals(
      exp = `ten`
      act = mo_class_to_test->test_me( 10 )
      msg = '10 should be TEN' ).
  ENDMETHOD.


  METHOD test_2.
    cl_abap_unit_assert=>assert_equals(
      exp = `two`
      act = mo_class_to_test->test_me( 2 )
      msg = '2 should be TWO' ).
  ENDMETHOD.


  METHOD test_3.
    cl_abap_unit_assert=>assert_equals(
      exp = `three`
      act = mo_class_to_test->test_me( 3 )
      msg = '3 should be THREE' ).
  ENDMETHOD.


  METHOD test_4.
    cl_abap_unit_assert=>assert_equals(
      exp = `four`
      act = mo_class_to_test->test_me( 4 )
      msg = '4 should be FOUR' ).
  ENDMETHOD.


  METHOD test_5.
    cl_abap_unit_assert=>assert_equals(
      exp = `five`
      act = mo_class_to_test->test_me( 5 )
      msg = '5 should be FIVE' ).
  ENDMETHOD.


  METHOD test_6.
    cl_abap_unit_assert=>assert_equals(
      exp = `six`
      act = mo_class_to_test->test_me( 6 )
      msg = '6 should be SIX' ).
  ENDMETHOD.


  METHOD test_7.
    cl_abap_unit_assert=>assert_equals(
      exp = `seven`
      act = mo_class_to_test->test_me( 7 )
      msg = '7 should be SEVEN' ).
  ENDMETHOD.


  METHOD test_8.
    cl_abap_unit_assert=>assert_equals(
      exp = `eight`
      act = mo_class_to_test->test_me( 8 )
      msg = '8 should be EIGHT' ).
  ENDMETHOD.


  METHOD test_9.
    cl_abap_unit_assert=>assert_equals(
      exp = `nine`
      act = mo_class_to_test->test_me( 9 )
      msg = '9 should be NINE' ).
  ENDMETHOD.


  METHOD test_dummy.

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = 0
      msg = |Don't worry - This test always fails to display all other results. |
      level = if_aunit_constants=>tolerable ).

  ENDMETHOD.


  METHOD test_greater_ten.
    DATA(number_to_test) = 10.

    DO 90 TIMES.
      ADD 1 TO number_to_test.
      cl_abap_unit_assert=>assert_equals(
        exp = ``
        act = mo_class_to_test->test_me( number_to_test )
        msg = 'any number greater than 10 should return an empty string' ).
    ENDDO.
  ENDMETHOD.
ENDCLASS.

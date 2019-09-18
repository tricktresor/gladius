CLASS zcl_glds_demo_range_tests DEFINITION
  PUBLIC
  INHERITING FROM zcl_glds_demo_test_helper
  ABSTRACT
  CREATE PUBLIC
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS .

  PUBLIC SECTION.

    METHODS constructor .
    METHODS test_low_1_entry
        FOR TESTING .
    METHODS test_low_2_entries
        FOR TESTING .
    METHODS test_low_3_entries
        FOR TESTING .
    METHODS test_high_1_entry
        FOR TESTING .
    METHODS test_high_2_entries
        FOR TESTING .
    METHODS test_high_3_entries
        FOR TESTING .
    METHODS test_low_high_1_entry
        FOR TESTING .
    METHODS test_low_high_2_entries
        FOR TESTING .
    METHODS test_low_high_3_entries
        FOR TESTING .
    METHODS test_empty
        FOR TESTING .
    METHODS test_dummy
        FOR TESTING .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_class_to_test TYPE REF TO zif_glds_demo_range.
ENDCLASS.



CLASS ZCL_GLDS_DEMO_RANGE_TESTS IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mo_class_to_test ?= mo_class_to_test_generic.
  ENDMETHOD.


  METHOD test_dummy.

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = 0
      msg = |Don't worry - This test always fails to display all other results. |
      level = if_aunit_constants=>tolerable ).

  ENDMETHOD.


  METHOD test_empty.

    cl_abap_unit_assert=>assert_equals(
      exp = ``
      act = mo_class_to_test->test_me( VALUE #( ) )
      msg = 'Empty range failed' ).

  ENDMETHOD.


  METHOD test_high_1_entry.
    cl_abap_unit_assert=>assert_equals(
      exp = `including 00.00.0000 to 01.01.2000`
      act = mo_class_to_test->test_me( VALUE #( ( sign = 'I' option = 'EQ' high = '20000101'  ) ) )
      msg = 'Range including one entry "high" failed' ).

  ENDMETHOD.


  METHOD test_high_2_entries.
    cl_abap_unit_assert=>assert_equals(
      exp = |including 00.00.0000 to 01.01.2000{ cl_abap_char_utilities=>cr_lf
            }including 00.00.0000 to 01.01.3000|
      act = mo_class_to_test->test_me( VALUE #(
                      ( sign = 'I' option = 'EQ' high = '20000101'  )
                      ( sign = 'I' option = 'EQ' high = '30000101'  )
                   ) )
      msg = 'Range including two entries "high" failed' ).

  ENDMETHOD.


  METHOD test_high_3_entries.
    cl_abap_unit_assert=>assert_equals(
      exp = |including 00.00.0000 to 01.01.1000{ cl_abap_char_utilities=>cr_lf
            }including 00.00.0000 to 01.01.2000{ cl_abap_char_utilities=>cr_lf
            }including 00.00.0000 to 01.01.3000|
      act = mo_class_to_test->test_me( VALUE #(
                      ( sign = 'I' option = 'EQ' high = '10000101'  )
                      ( sign = 'I' option = 'EQ' high = '20000101'  )
                      ( sign = 'I' option = 'EQ' high = '30000101'  )
                   ) )
      msg = 'Range including three entries "high" failed' ).

  ENDMETHOD.


  METHOD test_low_1_entry.

    cl_abap_unit_assert=>assert_equals(
      exp = `including 01.01.2000`
      act = mo_class_to_test->test_me( VALUE #( ( sign = 'I' option = 'EQ' low = '20000101'  ) ) )
      msg = 'Range including one entry "low" failed' ).

  ENDMETHOD.


  METHOD test_low_2_entries.
    cl_abap_unit_assert=>assert_equals(
      exp = |including 01.01.2000{ cl_abap_char_utilities=>cr_lf
            }including 01.01.3000|
      act = mo_class_to_test->test_me( VALUE #(
                      ( sign = 'I' option = 'EQ' low = '20000101'  )
                      ( sign = 'I' option = 'EQ' low = '30000101'  )
                   ) )
      msg = 'Range including twoe entries "low" failed' ).

  ENDMETHOD.


  METHOD test_low_3_entries.
    cl_abap_unit_assert=>assert_equals(
      exp = |including 01.01.1000{ cl_abap_char_utilities=>cr_lf
            }including 01.01.2000{ cl_abap_char_utilities=>cr_lf
            }including 01.01.3000|
      act = mo_class_to_test->test_me( VALUE #(
                      ( sign = 'I' option = 'EQ' low = '10000101' )
                      ( sign = 'I' option = 'EQ' low = '20000101' )
                      ( sign = 'I' option = 'EQ' low = '30000101' )
                   ) )
      msg = 'Range including three entries "low" failed' ).

  ENDMETHOD.


  METHOD test_low_high_1_entry.
    cl_abap_unit_assert=>assert_equals(
      exp = `including 01.01.2000 to 31.12.2000`
      act = mo_class_to_test->test_me( VALUE #( ( sign = 'I' option = 'EQ' low = '20000101' high = '20001231' ) ) )
      msg = 'Range including one entry "low - high" failed' ).

  ENDMETHOD.


  METHOD test_low_high_2_entries.
    cl_abap_unit_assert=>assert_equals(
      exp = |including 01.02.3000 to 04.05.6000{ cl_abap_char_utilities=>cr_lf
            }including 02.03.4000 to 05.06.7000|
      act = mo_class_to_test->test_me( VALUE #(
                      ( sign = 'I' option = 'EQ' low = '30000201' high = '60000504' )
                      ( sign = 'I' option = 'EQ' low = '40000302' high = '70000605' )
                   ) )
      msg = 'Range including two entries "low - high" failed' ).

  ENDMETHOD.


  METHOD test_low_high_3_entries.
    cl_abap_unit_assert=>assert_equals(
      exp = |including 01.01.2020 to 31.12.2020{ cl_abap_char_utilities=>cr_lf
            }including 01.01.2030 to 31.12.2030{ cl_abap_char_utilities=>cr_lf
            }including 01.01.2040 to 31.12.2040|
      act = mo_class_to_test->test_me( VALUE #(
                      ( sign = 'I' option = 'EQ' low = '20200101' high = '20201231' )
                      ( sign = 'I' option = 'EQ' low = '20300101' high = '20301231' )
                      ( sign = 'I' option = 'EQ' low = '20400101' high = '20401231' )
                   ) )
      msg = 'Range including three entries "low - high " failed' ).

  ENDMETHOD.
ENDCLASS.

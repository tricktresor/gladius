class ZCL_GLDS_DEMO_TEST_HELPER definition
  public
  abstract
  create public .

public section.

  data MO_CLASS_TO_TEST_GENERIC type ref to OBJECT .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GLDS_DEMO_TEST_HELPER IMPLEMENTATION.


  METHOD constructor.

    DATA lv_class_local TYPE c LENGTH 100.
    TRY.
        DATA(lv_classname) = cl_abap_classdescr=>get_class_name( me ).
        IF lv_classname(13) = '\PROGRAM=ZCL_'.
          FIND REGEX '\\PROGRAM=([^=]+)' IN lv_classname SUBMATCHES lv_classname.
          lv_classname = lv_classname(30).
        ELSE.
          lv_class_local = lv_classname.
          REPLACE 'LCL_TEST' WITH 'LCL_SOLUTION' INTO lv_class_local. " '\PROGRAM=ZGLDS_TEST_GENERATE_TEMPLATE\CLASS=LCL_SOLUTION'.
          IF sy-subrc > 0.
            REPLACE 'LCL_SECRET_TEST' WITH 'LCL_SOLUTION' INTO lv_class_local. " '\PROGRAM=ZGLDS_TEST_GENERATE_TEMPLATE\CLASS=LCL_SOLUTION'.
          ENDIF.
          lv_classname = lv_class_local.
        ENDIF.
        CREATE OBJECT me->mo_class_to_test_generic TYPE (lv_classname).
      CATCH cx_root.
    ENDTRY.

    IF me->mo_class_to_test_generic IS NOT BOUND.
      MESSAGE a000(oo) WITH 'unable to create class to test' lv_classname.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

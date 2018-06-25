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

    TRY.
        DATA(lv_classname) = cl_abap_classdescr=>get_class_name( me ).
        FIND REGEX '\\PROGRAM=([^=]+)' IN lv_classname SUBMATCHES lv_classname.
        lv_classname = lv_classname(30).
        CREATE OBJECT me->mo_class_to_test_generic TYPE (lv_classname).
      CATCH cx_root.
    ENDTRY.

    IF me->mo_class_to_test_generic IS NOT BOUND.
      MESSAGE a000(oo) WITH 'unable to create class to test' lv_classname.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

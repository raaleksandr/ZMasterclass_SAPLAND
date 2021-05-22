
class ltcl_Calculator_Tests definition for testing
  duration short
  risk level harmless
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_Calculator_Tests
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_CALCULATOR
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  private section.
    data:
      f_Cut type ref to zcl_Calculator.  "class under test

    methods: add_2_plus_2_eq_4 for testing,
             setup.
endclass.       "ltc_Calculator_Tests


class ltcl_Calculator_Tests implementation.

  method setup.
    f_Cut = new #( ).
  endmethod.

  method add_2_plus_2_eq_4.
    DATA(result) = f_Cut->add( a = 2 b = 2 ).
    cl_aunit_assert=>assert_equals( act = result exp = 4 ).
  endmethod.
endclass.

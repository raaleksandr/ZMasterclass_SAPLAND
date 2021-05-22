
CLASS ltcl_user_utils DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltcl_User_Utils
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_USER_UTILS
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_user_utils.  "class under test

    METHODS: get_user_short_name FOR TESTING.
ENDCLASS.       "ltcl_User_Utils


CLASS ltcl_user_utils IMPLEMENTATION.

  METHOD get_user_short_name.
    TEST-INJECTION zcall_badi.
      l_t_userlist = VALUE #( ( username = 'IVANOVII' firstname = 'Иван' lastname = 'Иванов' fullname = 'Иванов Иван Иванович' ) ).
    END-TEST-INJECTION.

    f_cut = NEW #( ).
    DATA(lv_short_name) = f_cut->get_user_short_name( 'IVANOVII' ).

    cl_abap_unit_assert=>assert_equals( act = lv_short_name exp = 'Иванов И.' ).
  ENDMETHOD.
ENDCLASS.

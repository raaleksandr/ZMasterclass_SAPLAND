*"* use this source file for your ABAP unit test classes

"! Unit tests of ZCL_UITEST_DEPT_SERVICE with the real repository
"! ZCL_UITEST_DEPT_REPO. All Open SQL access to ZDEPARTMENTS is
"! redirected to a test double by the Open SQL Test Double Framework,
"! so the tests neither read nor change the real table.
"! Changing methods are called with i_commit = abap_false.
CLASS ltc_dept_service DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS:
      c_msgid    TYPE symsgid VALUE 'ZUITEST_DEPT',
      c_id_1     TYPE zdepartments-department_id VALUE 'T01',
      c_id_2     TYPE zdepartments-department_id VALUE 'T02',
      c_id_3     TYPE zdepartments-department_id VALUE 'T03',
      c_id_new   TYPE zdepartments-department_id VALUE 'T99',
      c_id_unkn  TYPE zdepartments-department_id VALUE 'TXX'.

    CLASS-DATA go_environment TYPE REF TO if_osql_test_environment.

    DATA mo_cut     TYPE REF TO zcl_uitest_dept_service.
    DATA mt_initial TYPE zif_uitest_dept_repo=>tt_departments.

    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.
    METHODS setup.

    "! Current content of the ZDEPARTMENTS test double
    METHODS db_content
      RETURNING VALUE(rt_departments) TYPE zif_uitest_dept_repo=>tt_departments.

    METHODS assert_single_message
      IMPORTING is_result  TYPE zcl_uitest_dept_service=>ts_result
                iv_type    TYPE bapi_mtype
                iv_number  TYPE symsgno
                iv_v1      TYPE csequence OPTIONAL
                iv_success TYPE abap_bool.

    METHODS get_list_all FOR TESTING.
    METHODS get_list_by_id_range FOR TESTING.
    METHODS get_list_by_name_pattern FOR TESTING.
    METHODS get_list_no_match FOR TESTING.
    METHODS read_existing FOR TESTING.
    METHODS read_unknown FOR TESTING.
    METHODS create_ok FOR TESTING.
    METHODS create_existing FOR TESTING.
    METHODS create_without_id FOR TESTING.
    METHODS save_invalid_mode FOR TESTING.
    METHODS update_ok FOR TESTING.
    METHODS update_unknown FOR TESTING.
    METHODS delete_ok FOR TESTING.
    METHODS delete_unknown FOR TESTING.
ENDCLASS.


CLASS ltc_dept_service IMPLEMENTATION.

  METHOD class_setup.
    go_environment = cl_osql_test_environment=>create(
                       i_dependency_list = VALUE #( ( 'ZDEPARTMENTS' ) ) ).
  ENDMETHOD.


  METHOD class_teardown.
    go_environment->destroy( ).
  ENDMETHOD.


  METHOD setup.
    go_environment->clear_doubles( ).

    mt_initial = VALUE #(
      ( department_id      = c_id_1
        department_name    = 'Finance'
        department_leader  = 'Anna Smith'
        department_address = 'Main Street 1'
        department_room    = '101' )
      ( department_id      = c_id_2
        department_name    = 'Human Resources'
        department_leader  = 'John Brown'
        department_address = 'Main Street 1'
        department_room    = '102' )
      ( department_id      = c_id_3
        department_name    = 'IT Support'
        department_leader  = 'Mary Green'
        department_address = 'Park Avenue 5'
        department_room    = '201' ) ).
    go_environment->insert_test_data( mt_initial ).

    mo_cut = NEW #( ).
  ENDMETHOD.


  METHOD db_content.
    SELECT * FROM zdepartments
      INTO TABLE @rt_departments
      ORDER BY department_id.
    LOOP AT rt_departments ASSIGNING FIELD-SYMBOL(<ls_department>).
      CLEAR <ls_department>-mandt.
    ENDLOOP.
  ENDMETHOD.


  METHOD assert_single_message.
    cl_abap_unit_assert=>assert_equals( act = is_result-success
                                        exp = iv_success
                                        msg = 'Success flag' ).
    cl_abap_unit_assert=>assert_equals( act = lines( is_result-messages )
                                        exp = 1
                                        msg = 'Number of messages' ).
    DATA(ls_message) = is_result-messages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = ls_message-type   exp = iv_type   msg = 'Message type' ).
    cl_abap_unit_assert=>assert_equals( act = ls_message-id     exp = c_msgid   msg = 'Message class' ).
    cl_abap_unit_assert=>assert_equals( act = ls_message-number exp = iv_number msg = 'Message number' ).
    cl_abap_unit_assert=>assert_equals( act = condense( CONV string( ls_message-message_v1 ) )
                                        exp = condense( CONV string( iv_v1 ) )
                                        msg = 'Message variable 1' ).
    cl_abap_unit_assert=>assert_not_initial( act = ls_message-message msg = 'Message text' ).
  ENDMETHOD.


  METHOD get_list_all.
    DATA(lt_act) = mo_cut->get_list( ).

    LOOP AT lt_act ASSIGNING FIELD-SYMBOL(<ls_act>).
      CLEAR <ls_act>-mandt.
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = mt_initial ).
  ENDMETHOD.


  METHOD get_list_by_id_range.
    DATA(lt_act) = mo_cut->get_list(
      is_filter = VALUE #( id = VALUE #( ( sign = 'I' option = 'BT' low = c_id_2 high = c_id_3 ) ) ) ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_act ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_act[ 1 ]-department_id exp = c_id_2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_act[ 2 ]-department_id exp = c_id_3 ).
  ENDMETHOD.


  METHOD get_list_by_name_pattern.
    DATA(lt_act) = mo_cut->get_list(
      is_filter = VALUE #(
        address = VALUE #( ( sign = 'I' option = 'CP' low = 'Main*' ) )
        room    = VALUE #( ( sign = 'I' option = 'EQ' low = '102' ) ) ) ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_act ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_act[ 1 ]-department_id exp = c_id_2 ).
  ENDMETHOD.


  METHOD get_list_no_match.
    DATA(lt_act) = mo_cut->get_list(
      is_filter = VALUE #( name = VALUE #( ( sign = 'I' option = 'EQ' low = 'Sales' ) ) ) ).

    cl_abap_unit_assert=>assert_initial( lt_act ).
  ENDMETHOD.


  METHOD read_existing.
    DATA ls_department TYPE zdepartments.

    DATA(lv_found) = mo_cut->read_data( EXPORTING iv_id         = c_id_1
                                        IMPORTING es_department = ls_department ).

    cl_abap_unit_assert=>assert_true( lv_found ).
    CLEAR ls_department-mandt.
    cl_abap_unit_assert=>assert_equals( act = ls_department exp = mt_initial[ 1 ] ).
  ENDMETHOD.


  METHOD read_unknown.
    DATA ls_department TYPE zdepartments.

    DATA(lv_found) = mo_cut->read_data( EXPORTING iv_id         = c_id_unkn
                                        IMPORTING es_department = ls_department ).

    cl_abap_unit_assert=>assert_false( lv_found ).
    cl_abap_unit_assert=>assert_initial( ls_department ).
  ENDMETHOD.


  METHOD create_ok.
    DATA(ls_new) = VALUE zdepartments( department_id      = c_id_new
                                       department_name    = 'Logistics'
                                       department_leader  = 'Peter White'
                                       department_address = 'Harbour Road 7'
                                       department_room    = '301' ).

    DATA(ls_result) = mo_cut->save_data( is_department = ls_new
                                         iv_mode       = zcl_uitest_dept_service=>c_mode-create
                                         i_commit      = abap_false ).

    assert_single_message( is_result = ls_result iv_success = abap_true
                           iv_type = 'S' iv_number = '005' iv_v1 = c_id_new ).
    DATA(lt_exp) = mt_initial.
    APPEND ls_new TO lt_exp.
    cl_abap_unit_assert=>assert_equals( act = db_content( ) exp = lt_exp ).
  ENDMETHOD.


  METHOD create_existing.
    DATA(ls_department) = mt_initial[ 1 ].
    ls_department-department_name = 'Overwritten'.

    DATA(ls_result) = mo_cut->save_data( is_department = ls_department
                                         iv_mode       = zcl_uitest_dept_service=>c_mode-create
                                         i_commit      = abap_false ).

    assert_single_message( is_result = ls_result iv_success = abap_false
                           iv_type = 'E' iv_number = '004' iv_v1 = c_id_1 ).
    cl_abap_unit_assert=>assert_equals( act = db_content( ) exp = mt_initial ).
  ENDMETHOD.


  METHOD create_without_id.
    DATA(ls_result) = mo_cut->save_data(
                        is_department = VALUE #( department_name = 'No ID' )
                        iv_mode       = zcl_uitest_dept_service=>c_mode-create
                        i_commit      = abap_false ).

    assert_single_message( is_result = ls_result iv_success = abap_false
                           iv_type = 'E' iv_number = '003' ).
    cl_abap_unit_assert=>assert_equals( act = db_content( ) exp = mt_initial ).
  ENDMETHOD.


  METHOD save_invalid_mode.
    DATA(ls_department) = mt_initial[ 1 ].
    ls_department-department_name = 'Changed'.

    DATA(ls_result) = mo_cut->save_data( is_department = ls_department
                                         iv_mode       = 'X'
                                         i_commit      = abap_false ).

    assert_single_message( is_result = ls_result iv_success = abap_false
                           iv_type = 'E' iv_number = '011' iv_v1 = 'X' ).
    cl_abap_unit_assert=>assert_equals( act = db_content( ) exp = mt_initial ).
  ENDMETHOD.


  METHOD update_ok.
    DATA(ls_department) = mt_initial[ 2 ].
    ls_department-department_leader = 'Laura Black'.
    ls_department-department_room   = '110'.

    DATA(ls_result) = mo_cut->save_data( is_department = ls_department
                                         iv_mode       = zcl_uitest_dept_service=>c_mode-update
                                         i_commit      = abap_false ).

    assert_single_message( is_result = ls_result iv_success = abap_true
                           iv_type = 'S' iv_number = '006' iv_v1 = c_id_2 ).
    DATA(lt_exp) = mt_initial.
    lt_exp[ 2 ] = ls_department.
    cl_abap_unit_assert=>assert_equals( act = db_content( ) exp = lt_exp ).
  ENDMETHOD.


  METHOD update_unknown.
    DATA(ls_department) = VALUE zdepartments( department_id   = c_id_unkn
                                              department_name = 'Deleted meanwhile' ).

    DATA(ls_result) = mo_cut->save_data( is_department = ls_department
                                         iv_mode       = zcl_uitest_dept_service=>c_mode-update
                                         i_commit      = abap_false ).

    assert_single_message( is_result = ls_result iv_success = abap_false
                           iv_type = 'E' iv_number = '008' iv_v1 = c_id_unkn ).
    " The department must not be recreated
    cl_abap_unit_assert=>assert_equals( act = db_content( ) exp = mt_initial ).
  ENDMETHOD.


  METHOD delete_ok.
    DATA(ls_result) = mo_cut->delete_data( iv_id = c_id_3 i_commit = abap_false ).

    assert_single_message( is_result = ls_result iv_success = abap_true
                           iv_type = 'S' iv_number = '007' iv_v1 = c_id_3 ).
    DATA(lt_exp) = mt_initial.
    DELETE lt_exp WHERE department_id = c_id_3.
    cl_abap_unit_assert=>assert_equals( act = db_content( ) exp = lt_exp ).
  ENDMETHOD.


  METHOD delete_unknown.
    DATA(ls_result) = mo_cut->delete_data( iv_id = c_id_unkn i_commit = abap_false ).

    assert_single_message( is_result = ls_result iv_success = abap_false
                           iv_type = 'E' iv_number = '008' iv_v1 = c_id_unkn ).
    cl_abap_unit_assert=>assert_equals( act = db_content( ) exp = mt_initial ).
  ENDMETHOD.

ENDCLASS.

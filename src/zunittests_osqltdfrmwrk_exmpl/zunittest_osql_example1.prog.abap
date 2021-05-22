*&---------------------------------------------------------------------*
*& Report ZUNITTEST_ATDF_REFACTORED
*&---------------------------------------------------------------------*
*& Refactored program
*&---------------------------------------------------------------------*
REPORT zunittest_osql_example1.

TABLES: usr02.

DATA: g_t_user_list TYPE zunittest_userlist_table.

SELECT-OPTIONS: s_uname  FOR usr02-bname.

CLASS lcl_data_reader DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
      get_data IMPORTING i_t_uname_range     TYPE susr_t_range_4_xubname
               RETURNING VALUE(r_t_userlist) TYPE zunittest_userlist_table.

  PRIVATE SECTION.
    DATA: mo_database_call TYPE REF TO zcl_username_database_call.
ENDCLASS.

CLASS lcl_data_reader IMPLEMENTATION.
  METHOD constructor.
    mo_database_call = NEW #( ).
  ENDMETHOD.

  METHOD get_data.
    DATA(l_t_userlist_db) = mo_database_call->get_user_list( i_t_uname_range ).

    MOVE-CORRESPONDING l_t_userlist_db TO r_t_userlist.

    LOOP AT r_t_userlist ASSIGNING FIELD-SYMBOL(<l_s_user_list>).
      IF <l_s_user_list>-firstname IS NOT INITIAL.
        <l_s_user_list>-short_name = |{ <l_s_user_list>-lastname } { <l_s_user_list>-firstname(1) }.|.
      ELSE.
        <l_s_user_list>-short_name = <l_s_user_list>-lastname.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_data.

*&---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& Get data
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .

  DATA(lo_reader) = NEW lcl_data_reader( ).
  g_t_user_list[] = lo_reader->get_data( s_uname[] ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& Display selected data
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data .
  IF g_t_user_list[] IS NOT INITIAL.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_salv_table)
                                CHANGING t_table = g_t_user_list ).
      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        MESSAGE lx_salv_msg->get_text( ) TYPE 'I'.
        RETURN.
    ENDTRY.
  ELSE.
    MESSAGE 'No users found' TYPE 'S'.
  ENDIF.

  lo_salv_table->get_columns( )->set_optimize( abap_true ).
  lo_salv_table->display( ).
ENDFORM.

CLASS ltcl_osql_example_unit DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PUBLIC SECTION.

    METHODS: all_data_filled FOR TESTING.
  PRIVATE SECTION.

    CLASS-DATA: mo_environment TYPE REF TO if_osql_test_environment.

    DATA: mo_cut         TYPE REF TO lcl_data_reader.

    CLASS-METHODS: class_setup,
      class_teardown.

    METHODS: setup.
ENDCLASS.

CLASS ltcl_osql_example_unit IMPLEMENTATION.

  METHOD class_setup.
    mo_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'USR21' )
                                                                                    ( 'ADRP' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    mo_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    mo_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD all_data_filled.
    mo_cut = NEW #( ).

    mo_environment->insert_test_data(
      VALUE suid_tt_usr21( ( bname = 'IVANOVII' persnumber = '1' ) ) ).

    mo_environment->insert_test_data(
      VALUE zadrp_table_type( ( persnumber = '1'
                                date_from  = '10000101'
                                date_to    = '99991231'
                                name_first = 'Иван'
                                name_last  = 'Иванов'
                                name_text  = 'Иванов Иван Иванович' ) ) ).

    DATA(l_t_data) = mo_cut->get_data( VALUE #( ) ).

    cl_abap_unit_assert=>assert_equals( act = l_t_data[]
                                        exp = VALUE zunittest_userlist_table( ( username   = 'IVANOVII'
                                                                                firstname  = 'Иван'
                                                                                lastname   = 'Иванов'
                                                                                fullname   = 'Иванов Иван Иванович'
                                                                                short_name = 'Иванов И.' ) ) ).
  ENDMETHOD.
ENDCLASS.

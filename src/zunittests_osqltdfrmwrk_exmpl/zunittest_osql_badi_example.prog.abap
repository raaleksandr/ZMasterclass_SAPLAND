*&---------------------------------------------------------------------*
*& Report ZUNITTEST_ATDF_REFACTORED
*&---------------------------------------------------------------------*
*& Example How Open SQL Test Double framework lets you to include
*& standard badi call in unit test without any mocks.
*&---------------------------------------------------------------------*
REPORT zunittest_osql_badi_example.

TABLES: usr02.

DATA: g_t_user_list TYPE zunittest_userlist_table.

SELECT-OPTIONS: s_uname  FOR usr02-bname.

CLASS lcl_external_call DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_username_badi_call.
ENDCLASS.

CLASS lcl_external_call IMPLEMENTATION.
  METHOD zif_username_badi_call~call_bapi_user_getlist.
    CALL FUNCTION 'BAPI_USER_GETLIST'
      EXPORTING
        max_rows        = max_rows
        with_username   = with_username
      IMPORTING
        rows            = rows
      TABLES
        selection_range = selection_range
        selection_exp   = selection_exp
        userlist        = userlist
        return          = return.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_data_reader DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
      get_data IMPORTING i_t_uname_range TYPE susr_t_range_4_xubname
               EXPORTING e_t_userlist    TYPE zunittest_userlist_table
                         e_t_return      TYPE bapiret2_t.

  PRIVATE SECTION.
    DATA: g_o_external_call TYPE REF TO zif_username_badi_call.
ENDCLASS.

CLASS lcl_data_reader IMPLEMENTATION.
  METHOD constructor.
    g_o_external_call = NEW lcl_external_call( ).
  ENDMETHOD.

  METHOD get_data.
    DATA: l_t_selection_range TYPE TABLE OF bapiussrge,
          l_t_return          TYPE TABLE OF bapiret2,
          l_t_userlist        TYPE hrbas_bapiusname_table.

    REFRESH: e_t_userlist, e_t_return.

    LOOP AT i_t_uname_range ASSIGNING FIELD-SYMBOL(<l_s_uname>).
      APPEND VALUE #( parameter = 'USERNAME' ) TO l_t_selection_range ASSIGNING FIELD-SYMBOL(<l_s_selection_range>).
      MOVE-CORRESPONDING <l_s_uname> TO <l_s_selection_range>.
    ENDLOOP.

    REFRESH g_t_user_list[].

    g_o_external_call->call_bapi_user_getlist( EXPORTING with_username   = abap_true
                                                         selection_range = l_t_selection_range
                                               IMPORTING userlist        = l_t_userlist
                                                         return          = l_t_return ).

    MOVE-CORRESPONDING l_t_userlist TO e_t_userlist.

    LOOP AT e_t_userlist ASSIGNING FIELD-SYMBOL(<l_s_user_list>).
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
  lo_reader->get_data( EXPORTING i_t_uname_range = s_uname[]
                       IMPORTING e_t_userlist    = g_t_user_list
                                 e_t_return      = DATA(l_t_return) ).

  IF l_t_return[] IS NOT INITIAL.
    CALL FUNCTION 'SUSR_DISPLAY_LOG'
      TABLES
        it_log_bapiret2 = l_t_return
      EXCEPTIONS
        OTHERS          = 0.
  ENDIF.
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

CLASS ltcl_osql_badi_example_unit DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PUBLIC SECTION.

    METHODS: all_data_filled FOR TESTING.
  PRIVATE SECTION.

    CLASS-DATA: mo_environment TYPE REF TO if_osql_test_environment.

    DATA: mo_cut         TYPE REF TO lcl_data_reader.

    CLASS-METHODS: class_setup,
      class_teardown.

    METHODS: setup.
ENDCLASS.

CLASS ltcl_osql_badi_example_unit IMPLEMENTATION.

  METHOD class_setup.
    mo_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'USR02' )
                                                                                    ( 'USR21' )
                                                                                    ( 'ADRP' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    mo_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    mo_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD all_data_filled.

    mo_environment->insert_test_data(
      VALUE suid_tt_usr02( ( bname = 'IVANOVII' ) ) ).

    mo_environment->insert_test_data(
      VALUE suid_tt_usr21( ( bname = 'IVANOVII' persnumber = '1' ) ) ).

    mo_environment->insert_test_data(
      VALUE zadrp_table_type( ( persnumber = '1'
                                date_from  = '10000101'
                                date_to    = '99991231'
                                name_first = 'Иван'
                                name_last  = 'Иванов'
                                name_text  = 'Иванов Иван Иванович' ) ) ).

    mo_cut = NEW #( ).
    mo_cut->get_data( EXPORTING i_t_uname_range = VALUE #( )
                      IMPORTING e_t_userlist    = DATA(l_t_userlist)
                                e_t_return      = DATA(l_t_return) ).

    cl_abap_unit_assert=>assert_equals( act = l_t_userlist[]
                                        exp = VALUE zunittest_userlist_table( ( username   = 'IVANOVII'
                                                                                firstname  = 'Иван'
                                                                                lastname   = 'Иванов'
                                                                                fullname   = 'Иванов Иван Иванович'
                                                                                short_name = 'Иванов И.' ) )
                                      ).

    cl_abap_unit_assert=>assert_initial( act = l_t_return[] ).
  ENDMETHOD.
ENDCLASS.

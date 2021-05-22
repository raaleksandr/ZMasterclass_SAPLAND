*&---------------------------------------------------------------------*
*& Report ZUNITTEST_ATDF_REFACTORED
*&---------------------------------------------------------------------*
*& Refactored program
*&---------------------------------------------------------------------*
REPORT zunittest_atdf_initial_refac1.

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
    METHODS: constructor IMPORTING io_external_call TYPE REF TO zif_username_badi_call OPTIONAL,
      get_data IMPORTING i_t_uname_range TYPE susr_t_range_4_xubname
               EXPORTING e_t_userlist    TYPE zunittest_userlist_table
                         e_t_return      TYPE bapiret2_t.

  PRIVATE SECTION.
    DATA: g_o_external_call TYPE REF TO zif_username_badi_call.
ENDCLASS.

CLASS lcl_data_reader IMPLEMENTATION.
  METHOD constructor.
    IF io_external_call IS BOUND.
      g_o_external_call = io_external_call.
    ELSE.
      g_o_external_call = NEW lcl_external_call( ).
    ENDIF.
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

CLASS ltcl_adtf_example_unit DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PUBLIC SECTION.

    METHODS: run_test FOR TESTING.
  PRIVATE SECTION.

    DATA: mo_cut                  TYPE REF TO lcl_data_reader,
          mo_external_call_double TYPE REF TO zif_username_badi_call,
          mt_user_list            TYPE zunittest_userlist_table.

    METHODS: setup,
      given_badi_returns IMPORTING i_t_user_list TYPE zunittest_userlist_table,
      when_run_get_data,
      then_name_equals IMPORTING iv_short_name TYPE clike.
ENDCLASS.

CLASS ltcl_adtf_example_unit IMPLEMENTATION.
  METHOD setup.
    " create test double object
    mo_external_call_double ?=
      cl_abap_testdouble=>create( 'ZIF_USERNAME_BADI_CALL' ).
    mo_cut = NEW #( mo_external_call_double ).
  ENDMETHOD.
  METHOD given_badi_returns.
    cl_abap_testdouble=>configure_call( mo_external_call_double
      )->set_parameter( name = 'USERLIST' value = i_t_user_list[]
      )->ignore_all_parameters( ).

    mo_external_call_double->call_bapi_user_getlist( ).
  ENDMETHOD.
  METHOD when_run_get_data.
    REFRESH mt_user_list.
    mo_cut->get_data( EXPORTING i_t_uname_range = VALUE #( )
                      IMPORTING e_t_userlist    = mt_user_list ).
  ENDMETHOD.
  METHOD then_name_equals.
    cl_abap_unit_assert=>assert_equals( act = mt_user_list[ 1 ]-short_name
                                        exp = iv_short_name ).
  ENDMETHOD.
  METHOD run_test.

    given_badi_returns(
      i_t_user_list =
        VALUE #( ( username  = 'IVANOVII'
                   firstname = 'Иван'
                   lastname  = 'Иванов'
                   fullname  = 'Иванов Иван Иванович'
                 )
               )
             ).

    when_run_get_data( ).

    then_name_equals( 'Иванов И.' ).
  ENDMETHOD.
ENDCLASS.

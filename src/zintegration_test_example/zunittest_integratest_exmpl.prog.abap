*&---------------------------------------------------------------------*
*& Report ZUNITTEST_INTEGRATEST_EXMPL
*&---------------------------------------------------------------------*
*& Test of integration test using abap unit
*&---------------------------------------------------------------------*
REPORT zunittest_integratest_exmpl.

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

" HARMLESS - не меняет СУБД
" DANGEROUS - может изменить данные СУБД, но не трогает настройки
" CRITICAL - может поменять настройки
CLASS lct_integration_test DEFINITION FOR TESTING RISK LEVEL DANGEROUS DURATION SHORT.
  PRIVATE SECTION.
    METHODS: setup,
             integra_test_all_filled FOR TESTING,
             delete_user_if_exists IMPORTING i_username TYPE uname
                                   RETURNING VALUE(r_error_and_exit) TYPE abap_bool,
             create_test_user IMPORTING i_username TYPE uname
                                        i_firstname TYPE AD_NAMEFIR
                                        i_lastname TYPE ad_namelas
                              RETURNING VALUE(r_error_and_exit) TYPE abap_bool,
             given_user IMPORTING i_username TYPE uname
                                  i_firstname TYPE AD_NAMEFIR
                                  i_lastname TYPE ad_namelas
                        RETURNING VALUE(r_error_and_exit) TYPE abap_bool,
             given_selection_screen IMPORTING i_select_username TYPE uname,
             when_run_get_data,
             then_result_must_be IMPORTING i_t_expected_result TYPE zunittest_userlist_table.
ENDCLASS.

CLASS lct_integration_test IMPLEMENTATION.

  METHOD setup.
    REFRESH s_uname[].
  ENDMETHOD.

  METHOD integra_test_all_filled.

    IF given_user( i_username  = '_FORTEST01'
                   i_firstname = 'Иван'
                   i_lastname  = 'Иванов' ) = abap_true.

      RETURN.
    ENDIF.

    when_run_get_data( ).

    then_result_must_be( i_t_expected_result = VALUE #( ( username   = '_FORTEST01'
                                                          firstname  = 'Иван'
                                                          lastname   = 'Иванов'
                                                          fullname   = 'Иван Иванов'
                                                          short_name = 'Иванов И.' ) ) ).
  ENDMETHOD.

  METHOD delete_user_if_exists.

    DATA: l_v_rows   TYPE i,
          l_t_return TYPE TABLE OF bapiret2.

    DATA(l_t_selection_range) = VALUE cts_organizer_tt_wd_user( ( parameter = 'USERNAME'
                                                                  sign      = 'I'
                                                                  option    = 'EQ'
                                                                  low       = '_FORTEST01' ) ).

    CALL FUNCTION 'BAPI_USER_GETLIST'
      IMPORTING
        rows            = l_v_rows
      TABLES
        selection_range = l_t_selection_range[].

    IF l_v_rows > 0.
      CALL FUNCTION 'BAPI_USER_DELETE'
        EXPORTING
          username = '_FORTEST01'
        TABLES
          return   = l_t_return[].
    ENDIF.

    LOOP AT l_t_return TRANSPORTING NO FIELDS
      WHERE type CA 'EAX'.

      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      cl_aunit_assert=>assert_subrc( exp = 4 act = sy-subrc msg = 'Ошибка при вызове BAPI по удалению пользователя' ).
      r_error_and_exit = abap_true.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD given_user.
    r_error_and_exit = delete_user_if_exists( i_username ).
    r_error_and_exit = create_test_user( i_username  = i_username
                                         i_firstname = i_firstname
                                         i_lastname  = i_lastname ).
    given_selection_screen( i_username ).
  ENDMETHOD.

  METHOD given_selection_screen.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = i_select_username ) TO s_uname.
  ENDMETHOD.

  METHOD when_run_get_data.
    PERFORM get_data.
  ENDMETHOD.

  METHOD then_result_must_be.
    cl_aunit_assert=>assert_equals( act = g_t_user_list[] exp = i_t_expected_result[] msg = 'Итоговая таблица ALV не равна эталонной' ).
  ENDMETHOD.

  METHOD create_test_user.

    DATA: l_t_return TYPE TABLE OF BAPIRET2.

    CALL FUNCTION 'BAPI_USER_CREATE1'
      EXPORTING
        username  = i_username
        logondata = VALUE bapilogond( gltgb = '99991231' )
        password  = VALUE bapipwd( bapipwd = 'Init12345' )
        address   = VALUE bapiaddr3( firstname = i_firstname lastname = i_lastname )
      TABLES
        return    = l_t_return[].

    LOOP AT l_t_return TRANSPORTING NO FIELDS
      WHERE type CA 'EAX'.

      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      cl_aunit_assert=>assert_subrc( exp = 4 act = sy-subrc msg = 'Ошибка при вызове BAPI по созданию пользователя' ).
      r_error_and_exit = abap_true.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDMETHOD.
ENDCLASS.

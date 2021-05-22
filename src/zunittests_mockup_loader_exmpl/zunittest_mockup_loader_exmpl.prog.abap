*&---------------------------------------------------------------------*
*& Report ZUNITTEST_MOCKUP_LOADER_EXMPL
*&---------------------------------------------------------------------*
*& Refactored program
*&---------------------------------------------------------------------*
REPORT zunittest_mockup_loader_exmpl.

TABLES: usr02.

DATA: g_t_user_list TYPE zunittest_userlist_table.

PARAMETERS: p_uname  TYPE usr02-bname OBLIGATORY.

INTERFACE lif_database_call.
  METHODS: select_usr21 IMPORTING i_uname   TYPE uname
                        EXPORTING e_s_usr21 TYPE usr21,
    select_adrp IMPORTING i_persnumber TYPE adrp-persnumber
                EXPORTING e_s_adrp     TYPE adrp.
ENDINTERFACE.

CLASS lcl_database_call DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_database_call.
ENDCLASS.

CLASS lcl_database_call IMPLEMENTATION.
  METHOD lif_database_call~select_usr21.
    CLEAR e_s_usr21.
    SELECT SINGLE *
      FROM usr21
      INTO CORRESPONDING FIELDS OF e_s_usr21
      WHERE bname = i_uname.
  ENDMETHOD.

  METHOD lif_database_call~select_adrp.
    CLEAR e_s_adrp.
    SELECT SINGLE *
      FROM adrp
      INTO CORRESPONDING FIELDS OF e_s_adrp
      WHERE persnumber = i_persnumber.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_data_reader DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING io_database_call TYPE REF TO lif_database_call OPTIONAL,
      get_data IMPORTING i_username   TYPE xubname
               EXPORTING e_t_userlist TYPE zunittest_userlist_table.

  PRIVATE SECTION.
    DATA: g_o_database_call TYPE REF TO lif_database_call.
ENDCLASS.

CLASS lcl_data_reader IMPLEMENTATION.
  METHOD constructor.
    IF io_database_call IS BOUND.
      g_o_database_call = io_database_call.
    ELSE.
      g_o_database_call = NEW lcl_database_call( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_data.
    DATA: l_s_userlist LIKE LINE OF e_t_userlist.

    g_o_database_call->select_usr21( EXPORTING i_uname   = i_username
                                     IMPORTING e_s_usr21 = DATA(l_s_usr21) ).

    IF l_s_usr21-persnumber IS NOT INITIAL.
      g_o_database_call->select_adrp( EXPORTING i_persnumber = l_s_usr21-persnumber
                                      IMPORTING e_s_adrp     = DATA(l_s_adrp) ).
    ENDIF.

    IF l_s_adrp IS NOT INITIAL.
      l_s_userlist-username  = l_s_usr21-bname.
      l_s_userlist-firstname = l_s_adrp-name_first.
      l_s_userlist-lastname  = l_s_adrp-name_last.
      l_s_userlist-fullname  = l_s_adrp-name_text.

      IF l_s_userlist-firstname IS NOT INITIAL.
        l_s_userlist-short_name = |{ l_s_userlist-lastname } { l_s_userlist-firstname(1) }.|.
      ENDIF.

      APPEND l_s_userlist TO e_t_userlist.
    ENDIF.
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
  lo_reader->get_data( EXPORTING i_username      = p_uname
                       IMPORTING e_t_userlist    = g_t_user_list ).
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

CLASS ltdl_external_call_fake DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif_database_call.

    METHODS: constructor IMPORTING io_ml TYPE REF TO zcl_mockup_loader.

  PRIVATE SECTION.
    DATA: o_ml TYPE REF TO zcl_mockup_loader.
ENDCLASS.

CLASS ltdl_external_call_fake IMPLEMENTATION.
  METHOD constructor.

    IF io_ml IS BOUND.
      o_ml = io_ml.
    ENDIF.

*    o_ml->load_data(
*        exporting i_obj       = 'zunittests_mockup_loader_example/given_usr21'
*        importing e_container = lt_testcases ).
  ENDMETHOD.

  METHOD lif_database_call~select_usr21.

    DATA: l_t_usr21 TYPE TABLE OF usr21.

    " Load and store flights table
    TRY.
        zcl_mockup_loader_store=>load_and_store(
          io_ml    = o_ml
          i_obj    = 'zunittests_mockup_loader_example/given_usr21'
          i_name   = 'USR21'
          i_strict = abap_false
          i_tabkey = 'BNAME'
          i_type   = 'SUID_TT_USR21' ).
      CATCH zcx_mockup_loader_error INTO DATA(lo_error).
        cl_aunit_assert=>fail( lo_error->get_text( ) ).
    ENDTRY.

    zcl_mockup_loader_store=>retrieve(
        EXPORTING
          i_name = 'USR21'
          i_sift = i_uname
        IMPORTING
          e_data = e_s_usr21
        EXCEPTIONS OTHERS = 4 ).
  ENDMETHOD.

  METHOD lif_database_call~select_adrp.

    " Load and store flights table
    TRY.
        zcl_mockup_loader_store=>load_and_store(
          io_ml    = o_ml
          i_obj    = 'zunittests_mockup_loader_example/given_adrp'
          i_name   = 'ADRP'
          i_strict = abap_false
          i_tabkey = 'PERSNUMBER'
          i_type   = 'ZADRP_TABLE_TYPE' ).
      CATCH zcx_mockup_loader_error INTO DATA(lo_error).
        cl_aunit_assert=>fail( lo_error->get_text( ) ).
    ENDTRY.

    zcl_mockup_loader_store=>retrieve(
        EXPORTING
          i_name = 'ADRP'
          i_sift = i_persnumber
        IMPORTING
          e_data = e_s_adrp
        EXCEPTIONS OTHERS = 4 ).
  ENDMETHOD.
ENDCLASS.
*
CLASS ltcl_mockupldr_example_unit DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PUBLIC SECTION.

    METHODS: run_test_all_filled FOR TESTING.
  PRIVATE SECTION.

    DATA: mo_cut                  TYPE REF TO lcl_data_reader,
          mo_database_call_double TYPE REF TO lif_database_call,
          o_ml                    TYPE REF TO zcl_mockup_loader.

    METHODS: setup.
ENDCLASS.

CLASS ltcl_mockupldr_example_unit IMPLEMENTATION.
  METHOD setup.

    TRY.
        o_ml = zcl_mockup_loader=>create(
          i_type       = 'MIME'
          i_path       = 'ZUNITTESTS_MLOADER_EXMPL'
          i_encoding   = zif_mockup_loader_constants=>encoding_utf8
          i_amt_format = ' ,' ).
      CATCH cx_static_check INTO DATA(lo_ex).
        cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    ENDTRY.

    " create test double object
    mo_database_call_double ?= NEW ltdl_external_call_fake( o_ml ).
    mo_cut = NEW #( mo_database_call_double ).
  ENDMETHOD.

  METHOD run_test_all_filled.

    DATA: l_t_userlist_expected TYPE zunittest_userlist_table.

    mo_cut->get_data( EXPORTING i_username   = 'IVANOVII'
                      IMPORTING e_t_userlist = DATA(l_t_userlist_actual) ).

    o_ml->load_data( EXPORTING i_obj       = 'zunittests_mockup_loader_example/then_result_grid'
                     IMPORTING e_container = l_t_userlist_expected[] ).

    cl_aunit_assert=>assert_equals( act = l_t_userlist_actual[]
                                    exp = l_t_userlist_expected[] ).


*      cl_abap_testdouble=>create( 'ZIF_USERNAME_BADI_CALL' ).
*    mo_cut = NEW #( mo_external_call_double ).
*  ENDMETHOD.
*  METHOD run_test.
*
*    cl_abap_testdouble=>configure_call( mo_external_call_double
*      )->set_parameter( name = 'USERLIST' value =
*        VALUE zunittest_userlist_table(
*        ( username = 'IVANOVII' firstname = 'Иван'
*          lastname = 'Иванов' fullname = 'Иванов Иван Иванович' )
*           )
*      )->ignore_all_parameters( ).
*
*    mo_external_call_double->call_bapi_user_getlist( ).
*
*    mo_cut->get_data( EXPORTING i_t_uname_range = VALUE #( )
*                      IMPORTING e_t_userlist    = DATA(l_t_userlist) ).
*
*    cl_abap_unit_assert=>assert_equals( act = l_t_userlist[ 1 ]-short_name
*                                        exp = 'Иванов И.' ).
  ENDMETHOD.
ENDCLASS.

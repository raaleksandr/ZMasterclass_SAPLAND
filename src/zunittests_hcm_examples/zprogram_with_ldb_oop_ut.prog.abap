*&---------------------------------------------------------------------*
*& Report ZPROGRAM_WITH_LOGICAL_DB
*&---------------------------------------------------------------------*
*& Program using logical database and having unit tests
*&---------------------------------------------------------------------*
REPORT zprogram_with_ldb_refactored.

TABLES : objec, plog.

TYPES: BEGIN OF ty_line,
         short        TYPE objec-short,
         stext        TYPE objec-stext,
         period_start TYPE text20,
         period_end   TYPE text20,
       END OF ty_line.

TYPES: ty_lines TYPE STANDARD TABLE OF ty_line WITH DEFAULT KEY.

CLASS lcl_sy DEFINITION.
  PUBLIC SECTION.
    METHODS: datum RETURNING VALUE(datum) TYPE datum.
ENDCLASS.

CLASS lcl_sy IMPLEMENTATION.
  METHOD datum.
    datum = sy-datum.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_data_model DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING io_sy TYPE REF TO lcl_sy OPTIONAL,
      on_get_objec IMPORTING is_objec TYPE objec,
      get_data RETURNING VALUE(lines) TYPE ty_lines.
  PRIVATE SECTION.
    DATA: mt_lines TYPE ty_lines,
          mo_sy    TYPE REF TO lcl_sy.

    METHODS: _fill_alv_line_from_objec IMPORTING is_objec               TYPE objec
                                       RETURNING VALUE(result_alv_line) TYPE ty_line,
      _date_to_period_text IMPORTING iv_date            TYPE datum
                           RETURNING VALUE(period_text) TYPE string,
      _is_date_in_this_month IMPORTING iv_date                 TYPE datum
                             RETURNING VALUE(is_in_this_month) TYPE abap_bool,
      _is_date_in_this_year IMPORTING iv_date                TYPE datum
                            RETURNING VALUE(is_in_this_year) TYPE abap_bool,
      _is_date_in_the_past IMPORTING iv_date               TYPE datum
                           RETURNING VALUE(is_in_the_past) TYPE abap_bool,
      _is_date_unlimited_end IMPORTING iv_date                 TYPE datum
                             RETURNING VALUE(is_unlimited_end) TYPE abap_bool.
ENDCLASS.

CLASS lcl_data_model IMPLEMENTATION.

  METHOD constructor.
    IF io_sy IS BOUND.
      mo_sy = io_sy.
    ELSE.
      mo_sy = NEW #( ).
    ENDIF.
  ENDMETHOD.

  METHOD on_get_objec.
    DATA(ls_new_line) = _fill_alv_line_from_objec( is_objec ).
    APPEND ls_new_line TO mt_lines.
  ENDMETHOD.

  METHOD get_data.
    lines = mt_lines.
  ENDMETHOD.

  METHOD _fill_alv_line_from_objec.
    result_alv_line-short = is_objec-short.
    result_alv_line-stext = is_objec-stext.

    result_alv_line-period_start = _date_to_period_text( is_objec-begda ).
    result_alv_line-period_end = _date_to_period_text( is_objec-endda ).
  ENDMETHOD.

  METHOD _date_to_period_text.
    IF _is_date_unlimited_end( iv_date ).
      period_text = 'Unlimited'.
    ELSEIF _is_date_in_this_month( iv_date ).
      period_text = 'This Month'.
    ELSEIF _is_date_in_this_year( iv_date ).
      period_text = 'This Year'.
    ELSEIF _is_date_in_the_past( iv_date ).
      period_text = 'In the past'.
    ELSE.
      period_text = 'In the future'.
    ENDIF.
  ENDMETHOD.

  METHOD _is_date_in_this_month.
    is_in_this_month = boolc( substring( val = iv_date len = 6 ) = substring( val = mo_sy->datum( ) len = 6 ) ).
  ENDMETHOD.

  METHOD _is_date_in_this_year.
    is_in_this_year = boolc( substring( val = iv_date len = 4 ) = substring( val = mo_sy->datum( ) len = 4 ) ).
  ENDMETHOD.

  METHOD _is_date_in_the_past.
    is_in_the_past = boolc( iv_date < mo_sy->datum( ) ).
  ENDMETHOD.

  METHOD _is_date_unlimited_end.
    is_unlimited_end = boolc( iv_date = '99991231' ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
      on_get_objec IMPORTING is_objec TYPE objec,
      on_end_of_selection.

  PRIVATE SECTION.

    METHODS: _display_data_in_alv IMPORTING i_t_data TYPE ty_lines.

    DATA: mo_data_model TYPE REF TO lcl_data_model.
ENDCLASS.

CLASS lcl_application IMPLEMENTATION.

  METHOD constructor.
    mo_data_model = NEW #( ).
  ENDMETHOD.

  METHOD on_get_objec.
    mo_data_model->on_get_objec( is_objec ).
  ENDMETHOD.

  METHOD on_end_of_selection.
    DATA(l_t_data) = mo_data_model->get_data( ).
    _display_data_in_alv( l_t_data ).
  ENDMETHOD.

  METHOD _display_data_in_alv.

    DATA: l_t_data LIKE i_t_data.

    l_t_data = i_t_data.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_salv_table)
                                CHANGING t_table = l_t_data ).
      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        MESSAGE lx_salv_msg->get_text( ) TYPE 'I'.
        RETURN.
    ENDTRY.

    lo_salv_table->get_columns( )->set_optimize( abap_true ).

    TRY.
        lo_salv_table->get_columns( )->get_column( 'PERIOD_START' )->set_medium_text( 'Period Start' ).
        lo_salv_table->get_columns( )->get_column( 'PERIOD_END' )->set_short_text( 'Period End' ).
      CATCH cx_salv_not_found INTO DATA(lx_salv_not_found).
        MESSAGE lx_salv_not_found->get_text( ) TYPE 'I'.
        RETURN.
    ENDTRY.

    lo_salv_table->display( ).
  ENDMETHOD.
ENDCLASS.

CLASS ltd_sy_mock DEFINITION FOR TESTING INHERITING FROM lcl_sy.
  PUBLIC SECTION.
    METHODS: datum REDEFINITION,
      inject_sy_datum IMPORTING iv_sy_datum_for_test TYPE datum.

  PRIVATE SECTION.
    DATA: gv_sy_datum TYPE datum.
ENDCLASS.

CLASS ltd_sy_mock IMPLEMENTATION.
  METHOD datum.
    IF gv_sy_datum IS NOT INITIAL.
      datum = gv_sy_datum.
    ELSE.
      datum = super->datum( ).
    ENDIF.
  ENDMETHOD.

  METHOD inject_sy_datum.
    gv_sy_datum = iv_sy_datum_for_test.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_data_model DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA: mo_cut   TYPE REF TO lcl_data_model,
          mo_sy    TYPE REF TO ltd_sy_mock,
          ms_objec TYPE objec.

    METHODS: setup,
             start_this_mon_end_unlim FOR TESTING,
             start_this_year_end_unlim FOR TESTING,
             start_past_end_this_mon FOR TESTING,
             start_future_end_unlim FOR TESTING,
             _given_sy_datum IMPORTING iv_sy_datum TYPE datum,
             _given_objec IMPORTING is_objec TYPE objec,
             _when_lbd_get_objec,
             _then_getdata_equals IMPORTING i_t_expected TYPE ty_lines.
ENDCLASS.

CLASS ltcl_data_model IMPLEMENTATION.
  METHOD start_this_mon_end_unlim.

    _given_sy_datum( '20210426' ).
    _given_objec( VALUE #( short = 'CODE123'  stext = 'Org unit 1'
                           begda = '20210427' endda = '99991231' ) ).

    _when_lbd_get_objec( ).

    _then_getdata_equals( i_t_expected = VALUE #( ( short = 'CODE123' stext = 'Org unit 1'
                                                    period_start = 'This Month' period_end = 'Unlimited' ) )
                        ).
  ENDMETHOD.

  METHOD start_this_year_end_unlim.

    _given_sy_datum( '20210426' ).
    _given_objec( VALUE #( short = 'CODE123'  stext = 'Org unit 1'
                           begda = '20210301' endda = '99991231' ) ).

    _when_lbd_get_objec( ).

    _then_getdata_equals( i_t_expected = VALUE #( ( short = 'CODE123' stext = 'Org unit 1'
                                                    period_start = 'This Year' period_end = 'Unlimited' ) )
                        ).
  ENDMETHOD.

  METHOD start_past_end_this_mon.
    _given_sy_datum( '20210426' ).
    _given_objec( VALUE #( short = 'CODE123'  stext = 'Org unit 1'
                           begda = '20201201' endda = '20210425' ) ).

    _when_lbd_get_objec( ).

    _then_getdata_equals( i_t_expected = VALUE #( ( short = 'CODE123' stext = 'Org unit 1'
                                                    period_start = 'In the past' period_end = 'This Month' ) )
                        ).
  ENDMETHOD.

  METHOD start_future_end_unlim.
    _given_sy_datum( '20210426' ).
    _given_objec( VALUE #( short = 'CODE123'  stext = 'Org unit 1'
                           begda = '20220101' endda = '99991231' ) ).

    _when_lbd_get_objec( ).

    _then_getdata_equals( i_t_expected = VALUE #( ( short = 'CODE123' stext = 'Org unit 1'
                                                    period_start = 'In the future' period_end = 'Unlimited' ) )
                        ).
  ENDMETHOD.

  METHOD setup.
    mo_sy = new #( ).
    mo_cut = new #( mo_sy ).
    CLEAR ms_objec.
  ENDMETHOD.

  METHOD _given_sy_datum.
    IF mO_sy IS BOUND.
      mo_sy->inject_sy_datum( iv_sy_datum ).
    ENDIF.
  ENDMETHOD.

  METHOD _given_objec.
    ms_objec = is_objec.
  ENDMETHOD.

  METHOD _when_lbd_get_objec.
    mo_cut->on_get_objec( is_objec = ms_objec ).
  ENDMETHOD.

  METHOD _then_getdata_equals.
    DATA(l_t_data_actual) = mo_cut->get_data( ).
    cl_abap_unit_assert=>assert_equals( act = l_t_data_actual exp = i_t_expected ).
  ENDMETHOD.
ENDCLASS.

DATA: go_application TYPE REF TO lcl_application.

START-OF-SELECTION.
  go_application = NEW #( ).

GET objec.
  go_application->on_get_objec( objec ).

END-OF-SELECTION.
  go_application->on_end_of_selection( ).

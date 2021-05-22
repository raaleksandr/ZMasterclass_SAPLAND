*&---------------------------------------------------------------------*
*& Report ZPROGRAM_WITH_LOGICAL_DB
*&---------------------------------------------------------------------*
*& OOP Program using logical database
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

CLASS lcl_data_model DEFINITION.
  PUBLIC SECTION.
    METHODS: on_get_objec IMPORTING is_objec TYPE objec,
      get_data RETURNING VALUE(lines) TYPE ty_lines.
  PRIVATE SECTION.
    DATA: mt_lines TYPE ty_lines.

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
    is_in_this_month = boolc( iv_date(6) = sy-datum(6) ).
  ENDMETHOD.

  METHOD _is_date_in_this_year.
    is_in_this_year = boolc( iv_date(4) = sy-datum(4) ).
  ENDMETHOD.

  METHOD _is_date_in_the_past.
    is_in_the_past = boolc( iv_date < sy-datum ).
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

DATA: go_application TYPE REF TO lcl_application.

START-OF-SELECTION.
  go_application = NEW #( ).

GET objec.
  go_application->on_get_objec( objec ).

END-OF-SELECTION.
  go_application->on_end_of_selection( ).

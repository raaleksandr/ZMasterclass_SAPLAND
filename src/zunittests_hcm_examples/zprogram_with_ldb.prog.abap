*&---------------------------------------------------------------------*
*& Report ZPROGRAM_WITH_LOGICAL_DB
*&---------------------------------------------------------------------*
*& Procedural program using logical database
*&---------------------------------------------------------------------*
REPORT zprogram_with_ldb.

TABLES : objec, plog.

DATA: BEGIN OF wa_line,
        short        TYPE objec-short,
        stext        TYPE objec-stext,
        period_start TYPE text20,
        period_end   TYPE text20,
      END OF wa_line.

DATA: alv_lines LIKE TABLE OF wa_line.

GET objec.
  wa_line-short = objec-short.
  wa_line-stext = objec-stext.

  IF objec-begda(6) = sy-datum(6).
    wa_line-period_start = 'This Month'.
  ELSEIF objec-begda(4) = sy-datum(4).
    wa_line-period_start = 'This Year'.
  ELSEIF objec-begda < sy-datum.
    wa_line-period_start = 'In the past'.
  ELSE.
    wa_line-period_start = 'In the future'.
  ENDIF.

  IF objec-endda = '99991231'.
    wa_line-period_end = 'Unlimited'.
  ELSEIF objec-endda(4) = sy-datum(4).
    wa_line-period_end = 'This Year'.
  ELSEIF objec-endda < sy-datum.
    wa_line-period_end = 'In the past'.
  ELSE.
    wa_line-period_end = 'In the future'.
  ENDIF.

  APPEND wa_line TO alv_lines.

END-OF-SELECTION.

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_salv_table)
                              CHANGING t_table = alv_lines ).
    CATCH cx_salv_msg INTO DATA(lx_salv_msg).
      MESSAGE lx_salv_msg->get_text( ) TYPE 'I'.
      RETURN.
  ENDTRY.

  lo_salv_table->get_columns( )->set_optimize( abap_true ).
  lo_salv_table->get_columns( )->get_column( 'PERIOD_START' )->set_medium_text( 'Period Start' ).
  lo_salv_table->get_columns( )->get_column( 'PERIOD_END' )->set_short_text( 'Period End' ).
  lo_salv_table->display( ).

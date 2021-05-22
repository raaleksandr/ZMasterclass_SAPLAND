*&---------------------------------------------------------------------*
*& Report ZUNITTEST_ATDF_INITIAL_PROG
*&---------------------------------------------------------------------*
*& Initial program which we will test
*&---------------------------------------------------------------------*
REPORT zunittest_atdf_initial_prog.

TABLES: usr02.

DATA: g_t_user_list TYPE TABLE OF zunittest_userlist_struct.

SELECT-OPTIONS: s_uname  FOR usr02-bname.

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

  DATA: l_t_selection_range TYPE TABLE OF bapiussrge,
        l_t_return          TYPE TABLE OF bapiret2,
        l_t_userlist        TYPE TABLE OF bapiusname.

  LOOP AT s_uname ASSIGNING FIELD-SYMBOL(<l_s_uname>).
    APPEND VALUE #( parameter = 'USERNAME' ) TO l_t_selection_range ASSIGNING FIELD-SYMBOL(<l_s_selection_range>).
    MOVE-CORRESPONDING <l_s_uname> TO <l_s_selection_range>.
  ENDLOOP.

  REFRESH g_t_user_list[].
  CALL FUNCTION 'BAPI_USER_GETLIST'
    EXPORTING
      with_username   = abap_true
    TABLES
      selection_range = l_t_selection_range[]
      userlist        = l_t_userlist
      return          = l_t_return.

  MOVE-CORRESPONDING l_t_userlist TO g_t_user_list.

  LOOP AT g_t_user_list ASSIGNING FIELD-SYMBOL(<l_s_user_list>).
    IF <l_s_user_list>-firstname IS NOT INITIAL.
      <l_s_user_list>-short_name = |{ <l_s_user_list>-lastname } { <l_s_user_list>-firstname(1) }.|.
    ELSE.
      <l_s_user_list>-short_name = <l_s_user_list>-lastname.
    ENDIF.
  ENDLOOP.

  IF l_t_return[] IS NOT INITIAL.
    CALL FUNCTION 'SUSR_DISPLAY_LOG'
      TABLES
        it_log_bapiret2 = l_t_return[]
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

class ZCL_USER_UTILS definition
  public
  create public .

public section.

  methods GET_USER_SHORT_NAME
    importing
      value(IV_USER_NAME) type UNAME
    returning
      value(RV_SHORT_NAME) type BU_BEZ20 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_USER_UTILS IMPLEMENTATION.


method GET_USER_SHORT_NAME.

  DATA: l_t_selection_range TYPE TABLE OF BAPIUSSRGE,
        l_t_userlist        TYPE TABLE OF BAPIUSNAME.

  APPEND VALUE #( parameter = 'USERNAME' sign = 'I' option = 'EQ' low = iv_user_name )
    TO l_t_selection_range ASSIGNING FIELD-SYMBOL(<l_s_selection_range>).

  TEST-SEAM zcall_badi.
  CALL FUNCTION 'BAPI_USER_GETLIST'
    EXPORTING
      with_username   = abap_true
    TABLES
      selection_range = l_t_selection_range[]
      userlist        = l_t_userlist.
  END-TEST-SEAM.

  ASSIGN l_t_userlist[ 1 ] TO FIELD-SYMBOL(<l_s_userlist>).
  IF sy-subrc = 0.
    IF <l_s_userlist>-firstname IS NOT INITIAL.
      rv_short_name = |{ <l_s_userlist>-lastname } { <l_s_userlist>-firstname(1) }.|.
    ELSE.
      rv_short_name = <l_s_userlist>-lastname.
    ENDIF.
  ENDIF.
endmethod.
ENDCLASS.

interface ZIF_USERNAME_BADI_CALL
  public .


  types:
    TTYP_BAPIUSSEXP TYPE STANDARD TABLE OF bapiussexp WITH DEFAULT KEY .

  methods CALL_BAPI_USER_GETLIST
    importing
      value(MAX_ROWS) type BAPIUSMISC-BAPIMAXROW optional
      value(WITH_USERNAME) type BAPIUSMISC-WITH_NAME optional
      value(SELECTION_RANGE) type CTS_ORGANIZER_TT_WD_USER optional
      !SELECTION_EXP type TTYP_BAPIUSSEXP optional
    exporting
      !USERLIST type HRBAS_BAPIUSNAME_TABLE
      value(ROWS) type BAPIUSMISC-BAPIROWS
      !RETURN type BAPIRET2_T .
endinterface.

class ZCL_USERNAME_DATABASE_CALL definition
  public
  create public .

public section.

  methods GET_USER_LIST
    importing
      !I_T_USERNAME_RANGE type SUSR_T_RANGE_4_XUBNAME optional
    returning
      value(R_T_USER_LIST) type HRBAS_BAPIUSNAME_TABLE .
protected section.

  methods GET_SY_DATUM
    returning
      value(RV_SY_DATUM) type SYDATUM .
private section.
ENDCLASS.



CLASS ZCL_USERNAME_DATABASE_CALL IMPLEMENTATION.


method GET_SY_DATUM.
  rv_sy_datum = sy-datum.
endmethod.


method GET_USER_LIST.

  DATA: l_t_usr21 TYPE TABLE OF usr21,
        l_t_adrp  TYPE SORTED TABLE OF adrp WITH NON-UNIQUE KEY persnumber.

  SELECT *
    FROM usr21
    INTO CORRESPONDING FIELDS OF TABLE @l_t_usr21
    WHERE bname IN @i_t_username_range.

  DATA(l_t_usr21_for_all_entries) = l_t_usr21.
  DELETE l_t_usr21_for_all_entries WHERE persnumber IS INITIAL.

  IF l_t_usr21_for_all_entries[] IS NOT INITIAL.

    DATA(lv_sy_datum) = get_sy_datum( ).
    SELECT *
      FROM adrp
      INTO CORRESPONDING FIELDS OF TABLE @l_t_adrp
      FOR ALL ENTRIES IN @l_t_usr21_for_all_entries
      WHERE persnumber = @l_t_usr21_for_all_entries-persnumber
        AND date_from <= @lv_sy_datum
        AND date_to   >= @lv_sy_datum.
  ENDIF.

  LOOP AT l_t_usr21 ASSIGNING FIELD-SYMBOL(<l_s_usr21>).
    TRY.
      DATA(l_s_adrp) = l_t_adrp[ persnumber = <l_s_usr21>-persnumber ].
    CATCH cx_sy_itab_line_not_found.
      CLEAR l_s_adrp.
    ENDTRY.

    APPEND VALUE #( username  = <l_s_usr21>-bname
                    firstname = l_s_adrp-name_first
                    lastname  = l_s_adrp-name_last
                    fullname  = l_s_adrp-name_text ) TO r_t_user_list.
  ENDLOOP.
endmethod.
ENDCLASS.

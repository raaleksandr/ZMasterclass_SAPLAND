*&---------------------------------------------------------------------*
*& Report ZUITEST_DEPARTMENTS
*&---------------------------------------------------------------------*
*& Departments (SAP GUI): SAP GUI version of the "Departments" Fiori app.
*& Selection screen 1000 -> ALV list (screen 0200) -> form (screen 0300).
*& Data is read and changed in ZDEPARTMENTS through the business layer
*& ZCL_UITEST_DEPT_SERVICE; this report only handles screens and messages.
*& Screens 0200/0300, STATUS_0200/STATUS_0300 and TITLE_0200/TITLE_0300
*& follow the contract D8 of the OpenSpec change
*& add-departments-sap-gui-app. Do not rename screen elements, modules
*& or function codes without updating that contract.
*&---------------------------------------------------------------------*
REPORT zuitest_departments MESSAGE-ID zuitest_dept.

* Global data referenced by screens 0200 and 0300 (contract D8)
DATA:
  gv_okcode TYPE sy-ucomm,
  gs_form   TYPE zdepartments,
  gv_mode   TYPE c LENGTH 1.

* Form title variable (&1 of TITLE_0300): Create / Change
DATA gv_form_title TYPE c LENGTH 10.

* Typing reference for the selection screen only
DATA gs_sel TYPE zdepartments.

SELECT-OPTIONS:
  s_id   FOR gs_sel-department_id,
  s_name FOR gs_sel-department_name LOWER CASE,
  s_lead FOR gs_sel-department_leader LOWER CASE,
  s_addr FOR gs_sel-department_address LOWER CASE,
  s_room FOR gs_sel-department_room LOWER CASE.

*----------------------------------------------------------------------*
* Application: list screen 0200 with ALV, form screen 0300
*----------------------------------------------------------------------*
CLASS lcl_app DEFINITION FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_fc,
        back     TYPE sy-ucomm VALUE 'BACK',
        exit     TYPE sy-ucomm VALUE 'EXIT',
        cancel   TYPE sy-ucomm VALUE 'CANC',
        save     TYPE sy-ucomm VALUE 'SAVE',
        create   TYPE sy-ucomm VALUE 'ZCREATE',
        change   TYPE sy-ucomm VALUE 'ZCHANGE',
        delete   TYPE sy-ucomm VALUE 'ZDELETE',
        refresh  TYPE sy-ucomm VALUE 'ZREFRESH',
      END OF c_fc.

    CLASS-METHODS run.
    CLASS-METHODS init_alv.
    CLASS-METHODS handle_list_command
      CHANGING cv_okcode TYPE sy-ucomm.
    CLASS-METHODS handle_form_command
      CHANGING cv_okcode TYPE sy-ucomm.

  PRIVATE SECTION.
    CLASS-DATA:
      go_service     TYPE REF TO zcl_uitest_dept_service,
      gt_departments TYPE zif_uitest_dept_repo=>tt_departments,
      go_container   TYPE REF TO cl_gui_custom_container,
      go_grid        TYPE REF TO cl_gui_alv_grid,
      gv_form_saved  TYPE abap_bool.

    CLASS-METHODS load_data.
    CLASS-METHODS refresh_list.
    CLASS-METHODS free_alv.
    CLASS-METHODS build_fieldcat
      RETURNING VALUE(rt_fieldcat) TYPE lvc_t_fcat.
    CLASS-METHODS get_selected_id
      RETURNING VALUE(rv_id) TYPE zdepartments-department_id.
    CLASS-METHODS create_department.
    CLASS-METHODS change_department.
    CLASS-METHODS delete_department.
    CLASS-METHODS open_form
      IMPORTING iv_mode TYPE c.
    CLASS-METHODS save_form.
    CLASS-METHODS show_messages
      IMPORTING it_messages TYPE bapiret2_t.

    CLASS-METHODS on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.
    CLASS-METHODS on_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD run.
    go_service = NEW #( ).
    load_data( ).
    IF gt_departments IS INITIAL.
      MESSAGE s001.
    ENDIF.
    CALL SCREEN 0200.
  ENDMETHOD.

  METHOD load_data.
    gt_departments = go_service->get_list( VALUE #(
      id      = s_id[]
      name    = s_name[]
      leader  = s_lead[]
      address = s_addr[]
      room    = s_room[] ) ).
  ENDMETHOD.

  METHOD refresh_list.
    load_data( ).
    IF go_grid IS BOUND.
      go_grid->refresh_table_display(
        is_stable = VALUE #( row = abap_true col = abap_true ) ).
    ENDIF.
  ENDMETHOD.

  METHOD init_alv.
    IF go_container IS BOUND.
      RETURN.
    ENDIF.

    go_container = NEW #( container_name = 'CC_ALV' ).
    go_grid = NEW #( i_parent = go_container ).

    SET HANDLER on_toolbar FOR go_grid.
    SET HANDLER on_user_command FOR go_grid.

    DATA(ls_layout) = VALUE lvc_s_layo(
      sel_mode   = 'A'
      cwidth_opt = abap_true
      zebra      = abap_true ).
    DATA(lt_fieldcat) = build_fieldcat( ).

    go_grid->set_table_for_first_display(
      EXPORTING
        is_layout       = ls_layout
      CHANGING
        it_outtab       = gt_departments
        it_fieldcatalog = lt_fieldcat
      EXCEPTIONS
        OTHERS          = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD free_alv.
    IF go_grid IS BOUND.
      go_grid->free( ).
    ENDIF.
    IF go_container IS BOUND.
      go_container->free( ).
    ENDIF.
    CLEAR: go_grid, go_container.
    cl_gui_cfw=>flush( ).
  ENDMETHOD.

  METHOD build_fieldcat.
    rt_fieldcat = VALUE #(
      ( fieldname = 'MANDT' ref_table = 'ZDEPARTMENTS' tech = abap_true )
      ( fieldname = 'DEPARTMENT_ID' ref_table = 'ZDEPARTMENTS' key = abap_true
        coltext = 'Department ID' scrtext_l = 'Department ID'
        scrtext_m = 'Department ID' scrtext_s = 'Dept. ID' )
      ( fieldname = 'DEPARTMENT_NAME' ref_table = 'ZDEPARTMENTS'
        coltext = 'Department name' scrtext_l = 'Department name'
        scrtext_m = 'Department name' scrtext_s = 'Name' )
      ( fieldname = 'DEPARTMENT_LEADER' ref_table = 'ZDEPARTMENTS'
        coltext = 'Department leader' scrtext_l = 'Department leader'
        scrtext_m = 'Department leader' scrtext_s = 'Leader' )
      ( fieldname = 'DEPARTMENT_ADDRESS' ref_table = 'ZDEPARTMENTS'
        coltext = 'Department address' scrtext_l = 'Department address'
        scrtext_m = 'Department address' scrtext_s = 'Address' )
      ( fieldname = 'DEPARTMENT_ROOM' ref_table = 'ZDEPARTMENTS'
        coltext = 'Department room' scrtext_l = 'Department room'
        scrtext_m = 'Department room' scrtext_s = 'Room' ) ).
  ENDMETHOD.

  METHOD on_toolbar.
    APPEND VALUE #( butn_type = 3 ) TO e_object->mt_toolbar.
    APPEND VALUE #( function = c_fc-create icon = icon_create
                    text = 'Create' quickinfo = 'Create department' )
      TO e_object->mt_toolbar.
    APPEND VALUE #( function = c_fc-change icon = icon_change
                    text = 'Change' quickinfo = 'Change department' )
      TO e_object->mt_toolbar.
    APPEND VALUE #( function = c_fc-delete icon = icon_delete
                    text = 'Delete' quickinfo = 'Delete department' )
      TO e_object->mt_toolbar.
    APPEND VALUE #( function = c_fc-refresh icon = icon_refresh
                    text = 'Refresh' quickinfo = 'Refresh list' )
      TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD on_user_command.
    CASE e_ucomm.
      WHEN c_fc-create.
        create_department( ).
      WHEN c_fc-change.
        change_department( ).
      WHEN c_fc-delete.
        delete_department( ).
      WHEN c_fc-refresh.
        refresh_list( ).
        IF gt_departments IS INITIAL.
          MESSAGE s001.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD get_selected_id.
    go_grid->get_selected_rows( IMPORTING et_index_rows = DATA(lt_rows) ).
    IF lines( lt_rows ) <> 1.
      MESSAGE s002 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    READ TABLE gt_departments INDEX lt_rows[ 1 ]-index
      INTO DATA(ls_department).
    IF sy-subrc = 0.
      rv_id = ls_department-department_id.
    ELSE.
      MESSAGE s002 DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD create_department.
    CLEAR gs_form.
    open_form( zcl_uitest_dept_service=>c_mode-create ).
  ENDMETHOD.

  METHOD change_department.
    DATA ls_department TYPE zdepartments.

    DATA(lv_id) = get_selected_id( ).
    IF lv_id IS INITIAL.
      RETURN.
    ENDIF.

    IF go_service->read_data( EXPORTING iv_id         = lv_id
                              IMPORTING es_department = ls_department ) = abap_false.
      MESSAGE s008 WITH lv_id DISPLAY LIKE 'E'.
      refresh_list( ).
      RETURN.
    ENDIF.

    gs_form = ls_department.
    open_form( zcl_uitest_dept_service=>c_mode-update ).
  ENDMETHOD.

  METHOD delete_department.
    DATA lv_answer TYPE c LENGTH 1.

    DATA(lv_id) = get_selected_id( ).
    IF lv_id IS INITIAL.
      RETURN.
    ENDIF.

    MESSAGE s010 WITH lv_id INTO DATA(lv_question).
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Delete department'
        text_question         = lv_question
        text_button_1         = 'Yes'
        text_button_2         = 'No'
        default_button        = '2'
        display_cancel_button = abap_true
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0 OR lv_answer <> '1'.
      RETURN.
    ENDIF.

    show_messages( go_service->delete_data( lv_id )-messages ).
    refresh_list( ).
  ENDMETHOD.

  METHOD open_form.
    gv_mode = iv_mode.
    gv_form_title = SWITCH #( iv_mode
                              WHEN zcl_uitest_dept_service=>c_mode-update THEN 'Change'
                              ELSE 'Create' ).
    gv_form_saved = abap_false.
    CALL SCREEN 0300.
    IF gv_form_saved = abap_true.
      refresh_list( ).
    ENDIF.
  ENDMETHOD.

  METHOD save_form.
    DATA(ls_result) = go_service->save_data( is_department = gs_form
                                             iv_mode       = gv_mode ).
    show_messages( ls_result-messages ).

    " Department was deleted meanwhile (008): close the form and refresh
    " the list; any other error keeps the form open
    IF ls_result-success = abap_false
       AND NOT line_exists( ls_result-messages[ id = 'ZUITEST_DEPT' number = '008' ] ).
      RETURN.
    ENDIF.

    gv_form_saved = abap_true.
    LEAVE TO SCREEN 0.
  ENDMETHOD.

  METHOD show_messages.
    " The business layer returns one message per operation: show the last
    " one in the status bar, errors as type S displayed like E
    DATA(lv_lines) = lines( it_messages ).
    IF lv_lines = 0.
      RETURN.
    ENDIF.
    DATA(ls_message) = it_messages[ lv_lines ].

    IF ls_message-type = 'E'.
      MESSAGE ID ls_message-id TYPE 'S' NUMBER ls_message-number
        WITH ls_message-message_v1 ls_message-message_v2
             ls_message-message_v3 ls_message-message_v4
        DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE ID ls_message-id TYPE 'S' NUMBER ls_message-number
        WITH ls_message-message_v1 ls_message-message_v2
             ls_message-message_v3 ls_message-message_v4.
    ENDIF.
  ENDMETHOD.

  METHOD handle_list_command.
    DATA(lv_okcode) = cv_okcode.
    CLEAR cv_okcode.
    CASE lv_okcode.
      WHEN c_fc-back.
        free_alv( ).
        LEAVE TO SCREEN 0.
      WHEN c_fc-exit OR c_fc-cancel.
        free_alv( ).
        LEAVE PROGRAM.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_form_command.
    DATA(lv_okcode) = cv_okcode.
    CLEAR cv_okcode.
    CASE lv_okcode.
      WHEN c_fc-save.
        save_form( ).
      WHEN c_fc-back OR c_fc-cancel.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl_app=>run( ).

*----------------------------------------------------------------------*
* Screen 0200: departments list
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STATUS_0200'.
  SET TITLEBAR 'TITLE_0200'.
ENDMODULE.

MODULE init_alv_0200 OUTPUT.
  lcl_app=>init_alv( ).
ENDMODULE.

MODULE exit_0200 INPUT.
  lcl_app=>handle_list_command( CHANGING cv_okcode = gv_okcode ).
ENDMODULE.

MODULE user_command_0200 INPUT.
  lcl_app=>handle_list_command( CHANGING cv_okcode = gv_okcode ).
ENDMODULE.

*----------------------------------------------------------------------*
* Screen 0300: department form (fullscreen)
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'STATUS_0300'.
  SET TITLEBAR 'TITLE_0300' WITH gv_form_title.
ENDMODULE.

MODULE modify_screen_0300 OUTPUT.
  IF gv_mode = zcl_uitest_dept_service=>c_mode-update.
    LOOP AT SCREEN.
      IF screen-name = 'GS_FORM-DEPARTMENT_ID'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.

MODULE cancel_0300 INPUT.
  CLEAR gv_okcode.
  LEAVE TO SCREEN 0.
ENDMODULE.

MODULE user_command_0300 INPUT.
  lcl_app=>handle_form_command( CHANGING cv_okcode = gv_okcode ).
ENDMODULE.

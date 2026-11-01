"! <p class="shorttext synchronized" lang="en">Departments: business logic</p>
"! Business logic of the departments app (report ZUITEST_DEPARTMENTS).
"! Methods show no messages, popups or screens: changing methods return
"! a result with a success flag and messages of class ZUITEST_DEPT.
"! All database access goes through ZIF_UITEST_DEPT_REPO, which can be
"! replaced by a test double via the constructor.
CLASS zcl_uitest_dept_service DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES ty_mode TYPE c LENGTH 1.

    CONSTANTS:
      BEGIN OF c_mode,
        create TYPE ty_mode VALUE 'C',
        update TYPE ty_mode VALUE 'U',
      END OF c_mode.

    TYPES:
      BEGIN OF ts_result,
        success  TYPE abap_bool,
        messages TYPE bapiret2_t,
      END OF ts_result.

    "! @parameter io_repo | Data access; default ZCL_UITEST_DEPT_REPO
    METHODS constructor
      IMPORTING io_repo TYPE REF TO zif_uitest_dept_repo OPTIONAL.

    "! Departments matching the filter, sorted by ID
    METHODS get_list
      IMPORTING is_filter             TYPE zif_uitest_dept_repo=>ts_filter OPTIONAL
      RETURNING VALUE(rt_departments) TYPE zif_uitest_dept_repo=>tt_departments.

    "! @parameter rv_found | abap_true if the department exists
    METHODS read_data
      IMPORTING iv_id           TYPE zdepartments-department_id
      EXPORTING es_department   TYPE zdepartments
      RETURNING VALUE(rv_found) TYPE abap_bool.

    "! Create (c_mode-create) or change (c_mode-update) one department
    "! @parameter i_commit | abap_false: no COMMIT WORK / ROLLBACK WORK
    METHODS save_data
      IMPORTING is_department  TYPE zdepartments
                iv_mode        TYPE ty_mode
                i_commit       TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(rs_result) TYPE ts_result.

    "! @parameter i_commit | abap_false: no COMMIT WORK / ROLLBACK WORK
    METHODS delete_data
      IMPORTING iv_id          TYPE zdepartments-department_id
                i_commit       TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(rs_result) TYPE ts_result.

  PRIVATE SECTION.
    DATA mo_repo TYPE REF TO zif_uitest_dept_repo.

    "! Appends the message in sy-msgty/-msgid/-msgno/-msgv1..4
    METHODS add_message
      IMPORTING iv_text   TYPE string
      CHANGING  cs_result TYPE ts_result.

    METHODS end_luw
      IMPORTING is_result TYPE ts_result
                i_commit  TYPE abap_bool.
ENDCLASS.



CLASS ZCL_UITEST_DEPT_SERVICE IMPLEMENTATION.


  METHOD add_message.
    APPEND VALUE #( type       = sy-msgty
                    id         = sy-msgid
                    number     = sy-msgno
                    message    = iv_text
                    message_v1 = sy-msgv1
                    message_v2 = sy-msgv2
                    message_v3 = sy-msgv3
                    message_v4 = sy-msgv4 ) TO cs_result-messages.
    cs_result-success = xsdbool( NOT line_exists( cs_result-messages[ type = 'E' ] ) ).
  ENDMETHOD.


  METHOD constructor.
    mo_repo = COND #( WHEN io_repo IS BOUND THEN io_repo
                      ELSE NEW zcl_uitest_dept_repo( ) ).
  ENDMETHOD.


  METHOD delete_data.
    DATA lv_text TYPE string.

    CASE mo_repo->delete( iv_id = iv_id i_commit = abap_false ).
      WHEN zif_uitest_dept_repo=>c_result-ok.
        MESSAGE s007(zuitest_dept) WITH iv_id INTO lv_text.
      WHEN zif_uitest_dept_repo=>c_result-not_found.
        MESSAGE e008(zuitest_dept) WITH iv_id INTO lv_text.
      WHEN OTHERS.
        MESSAGE e009(zuitest_dept) INTO lv_text.
    ENDCASE.
    add_message( EXPORTING iv_text = lv_text CHANGING cs_result = rs_result ).

    end_luw( is_result = rs_result i_commit = i_commit ).
  ENDMETHOD.


  METHOD end_luw.
    IF i_commit = abap_false.
      RETURN.
    ENDIF.
    IF is_result-success = abap_true.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDMETHOD.


  METHOD get_list.
    rt_departments = mo_repo->select_by_filters( is_filter ).
  ENDMETHOD.


  METHOD read_data.
    es_department = mo_repo->read( iv_id ).
    rv_found = xsdbool( es_department IS NOT INITIAL ).
  ENDMETHOD.


  METHOD save_data.
    DATA lv_text TYPE string.

    IF iv_mode <> c_mode-create AND iv_mode <> c_mode-update.
      MESSAGE e011(zuitest_dept) WITH iv_mode INTO lv_text.
      add_message( EXPORTING iv_text = lv_text CHANGING cs_result = rs_result ).
      RETURN.
    ENDIF.

    IF is_department-department_id IS INITIAL.
      MESSAGE e003(zuitest_dept) INTO lv_text.
      add_message( EXPORTING iv_text = lv_text CHANGING cs_result = rs_result ).
      RETURN.
    ENDIF.

    IF iv_mode = c_mode-create.
      IF mo_repo->exists( is_department-department_id ) = abap_true.
        MESSAGE e004(zuitest_dept) WITH is_department-department_id INTO lv_text.
        add_message( EXPORTING iv_text = lv_text CHANGING cs_result = rs_result ).
        RETURN.
      ENDIF.
      CASE mo_repo->insert( is_department = is_department i_commit = abap_false ).
        WHEN zif_uitest_dept_repo=>c_result-ok.
          MESSAGE s005(zuitest_dept) WITH is_department-department_id INTO lv_text.
        WHEN zif_uitest_dept_repo=>c_result-duplicate.
          MESSAGE e004(zuitest_dept) WITH is_department-department_id INTO lv_text.
        WHEN OTHERS.
          MESSAGE e009(zuitest_dept) INTO lv_text.
      ENDCASE.
    ELSE.
      CASE mo_repo->update( is_department = is_department i_commit = abap_false ).
        WHEN zif_uitest_dept_repo=>c_result-ok.
          MESSAGE s006(zuitest_dept) WITH is_department-department_id INTO lv_text.
        WHEN zif_uitest_dept_repo=>c_result-not_found.
          " Department was deleted meanwhile: do not recreate it
          MESSAGE e008(zuitest_dept) WITH is_department-department_id INTO lv_text.
        WHEN OTHERS.
          MESSAGE e009(zuitest_dept) INTO lv_text.
      ENDCASE.
    ENDIF.
    add_message( EXPORTING iv_text = lv_text CHANGING cs_result = rs_result ).

    end_luw( is_result = rs_result i_commit = i_commit ).
  ENDMETHOD.
ENDCLASS.

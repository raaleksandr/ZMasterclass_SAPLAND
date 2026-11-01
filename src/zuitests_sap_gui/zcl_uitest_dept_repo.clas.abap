"! <p class="shorttext synchronized" lang="en">Departments: Open SQL access to ZDEPARTMENTS</p>
"! With i_commit = abap_true each change is committed on success and
"! rolled back on failure. With i_commit = abap_false neither COMMIT WORK
"! nor ROLLBACK WORK is executed: the caller controls the LUW.
CLASS zcl_uitest_dept_repo DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_uitest_dept_repo.

  PRIVATE SECTION.
    METHODS end_luw
      IMPORTING iv_result TYPE i
                i_commit  TYPE abap_bool.
ENDCLASS.



CLASS ZCL_UITEST_DEPT_REPO IMPLEMENTATION.


  METHOD end_luw.
    IF i_commit = abap_false.
      RETURN.
    ENDIF.
    IF iv_result = zif_uitest_dept_repo=>c_result-ok.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitest_dept_repo~delete.
    TRY.
        DELETE FROM zdepartments WHERE department_id = @iv_id.
        rv_result = COND #( WHEN sy-subrc = 0
                            THEN zif_uitest_dept_repo=>c_result-ok
                            ELSE zif_uitest_dept_repo=>c_result-not_found ).
      CATCH cx_sy_open_sql_db.
        rv_result = zif_uitest_dept_repo=>c_result-db_error.
    ENDTRY.
    end_luw( iv_result = rv_result i_commit = i_commit ).
  ENDMETHOD.


  METHOD zif_uitest_dept_repo~exists.
    SELECT SINGLE department_id FROM zdepartments
      INTO @DATA(lv_id)
      WHERE department_id = @iv_id.
    rv_exists = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD zif_uitest_dept_repo~insert.
    TRY.
        INSERT zdepartments FROM @is_department.
        rv_result = COND #( WHEN sy-subrc = 0
                            THEN zif_uitest_dept_repo=>c_result-ok
                            ELSE zif_uitest_dept_repo=>c_result-duplicate ).
      CATCH cx_sy_open_sql_db.
        rv_result = zif_uitest_dept_repo=>c_result-db_error.
    ENDTRY.
    end_luw( iv_result = rv_result i_commit = i_commit ).
  ENDMETHOD.


  METHOD zif_uitest_dept_repo~read.
    SELECT SINGLE * FROM zdepartments
      INTO @rs_department
      WHERE department_id = @iv_id.
  ENDMETHOD.


  METHOD zif_uitest_dept_repo~select_by_filters.
    SELECT * FROM zdepartments
      INTO TABLE @rt_departments
      WHERE department_id      IN @is_filter-id
        AND department_name    IN @is_filter-name
        AND department_leader  IN @is_filter-leader
        AND department_address IN @is_filter-address
        AND department_room    IN @is_filter-room
      ORDER BY department_id.
  ENDMETHOD.


  METHOD zif_uitest_dept_repo~update.
    TRY.
        UPDATE zdepartments FROM @is_department.
        rv_result = COND #( WHEN sy-subrc = 0
                            THEN zif_uitest_dept_repo=>c_result-ok
                            ELSE zif_uitest_dept_repo=>c_result-not_found ).
      CATCH cx_sy_open_sql_db.
        rv_result = zif_uitest_dept_repo=>c_result-db_error.
    ENDTRY.
    end_luw( iv_result = rv_result i_commit = i_commit ).
  ENDMETHOD.
ENDCLASS.

"! <p class="shorttext synchronized" lang="en">Departments: data access (ZDEPARTMENTS)</p>
"! Contract for reading and changing ZDEPARTMENTS. ZCL_UITEST_DEPT_SERVICE
"! depends only on this interface, so unit tests can inject a test double.
INTERFACE zif_uitest_dept_repo
  PUBLIC.

  TYPES:
    tt_departments TYPE STANDARD TABLE OF zdepartments WITH DEFAULT KEY,
    tt_r_id        TYPE RANGE OF zdepartments-department_id,
    tt_r_text      TYPE RANGE OF zdepartments-department_name,
    tt_r_room      TYPE RANGE OF zdepartments-department_room,
    BEGIN OF ts_filter,
      id      TYPE tt_r_id,
      name    TYPE tt_r_text,
      leader  TYPE tt_r_text,
      address TYPE tt_r_text,
      room    TYPE tt_r_room,
    END OF ts_filter.

  CONSTANTS:
    BEGIN OF c_result,
      ok        TYPE i VALUE 0,
      not_found TYPE i VALUE 4,
      duplicate TYPE i VALUE 5,
      db_error  TYPE i VALUE 8,
    END OF c_result.

  "! Departments matching all ranges of the filter, sorted by ID
  METHODS select_by_filters
    IMPORTING is_filter             TYPE ts_filter
    RETURNING VALUE(rt_departments) TYPE tt_departments.

  "! One department; initial structure if not found
  METHODS read
    IMPORTING iv_id                TYPE zdepartments-department_id
    RETURNING VALUE(rs_department) TYPE zdepartments.

  METHODS exists
    IMPORTING iv_id            TYPE zdepartments-department_id
    RETURNING VALUE(rv_exists) TYPE abap_bool.

  "! @parameter i_commit | abap_false: no COMMIT WORK / ROLLBACK WORK
  "! @parameter rv_result | c_result-ok, -duplicate or -db_error
  METHODS insert
    IMPORTING is_department    TYPE zdepartments
              i_commit         TYPE abap_bool DEFAULT abap_true
    RETURNING VALUE(rv_result) TYPE i.

  "! @parameter i_commit | abap_false: no COMMIT WORK / ROLLBACK WORK
  "! @parameter rv_result | c_result-ok, -not_found or -db_error
  METHODS update
    IMPORTING is_department    TYPE zdepartments
              i_commit         TYPE abap_bool DEFAULT abap_true
    RETURNING VALUE(rv_result) TYPE i.

  "! @parameter i_commit | abap_false: no COMMIT WORK / ROLLBACK WORK
  "! @parameter rv_result | c_result-ok, -not_found or -db_error
  METHODS delete
    IMPORTING iv_id            TYPE zdepartments-department_id
              i_commit         TYPE abap_bool DEFAULT abap_true
    RETURNING VALUE(rv_result) TYPE i.
ENDINTERFACE.

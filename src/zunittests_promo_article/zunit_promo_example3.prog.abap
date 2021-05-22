*&---------------------------------------------------------------------*
*& Report ZUNIT_PROMO_EXAMPLE1
*&---------------------------------------------------------------------*
*& Example program for unit test promo article of master class 05.2021
*& Program with isolation variant 2 (dependency injection) and test
*&---------------------------------------------------------------------*
REPORT zunit_promo_example3.

PARAMETERS: p_doc_id TYPE char10 MATCHCODE OBJECT ztable1_f4.

INTERFACE lif_ztable1_reader.
  METHODS read_value_by_doc_id
    IMPORTING iv_doc_id    TYPE char10
    RETURNING VALUE(value) TYPE i.
ENDINTERFACE.

CLASS lcl_ztable1_reader DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_ztable1_reader.

ENDCLASS.

CLASS lcl_ztable1_reader IMPLEMENTATION.
  METHOD lif_ztable1_reader~read_value_by_doc_id.
    SELECT SINGLE value
        INTO value
        FROM ztable1
        WHERE doc_id = iv_doc_id.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_ztable1_mock_reader DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_ztable1_reader.

    METHODS add_rec_to_test_buffer
      IMPORTING iv_doc_id TYPE char10
                iv_value  TYPE i.

  PRIVATE SECTION.
    DATA gtd_table TYPE STANDARD TABLE OF ztable1 WITH KEY doc_id.
ENDCLASS.

CLASS lcl_ztable1_mock_reader IMPLEMENTATION.
  METHOD lif_ztable1_reader~read_value_by_doc_id.
    READ TABLE gtd_table WITH KEY doc_id = iv_doc_id ASSIGNING FIELD-SYMBOL(<ls_row>).
    IF sy-subrc = 0.
      value = <ls_row>-value.
    ENDIF.
  ENDMETHOD.

  METHOD add_rec_to_test_buffer.
    DATA: ls_row  LIKE LINE OF gtd_table.

    ls_row-doc_id = iv_doc_id.
    ls_row-value  = iv_value.

    READ TABLE gtd_table FROM ls_row TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      MODIFY TABLE gtd_table FROM ls_row.
    ELSE.
      APPEND ls_row TO gtd_table.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_example_class DEFINITION.
  PUBLIC SECTION.
    DATA test_mode TYPE abap_bool.
    DATA test_buffer TYPE TABLE OF ztable1.
    METHODS get_value_plus_one
      IMPORTING if_ztable1_reader   TYPE REF TO lif_ztable1_reader
                iv_doc_id           TYPE char10
      RETURNING VALUE(return_value) TYPE i.
ENDCLASS.

CLASS lcl_example_class IMPLEMENTATION.
  METHOD get_value_plus_one.
    DATA lv_value TYPE i.

    lv_value = if_ztable1_reader->read_value_by_doc_id( iv_doc_id ).
    IF lv_value IS NOT INITIAL.
      return_value = lv_value + 1.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_example_class_unit DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    METHODS run_test FOR TESTING.
ENDCLASS.

CLASS lcl_example_class_unit IMPLEMENTATION.
  METHOD run_test.
    DATA(lo_cut) = NEW lcl_example_class( ).
    DATA(lo_mock_reader) = NEW lcl_ztable1_mock_reader( ).
    lo_mock_reader->add_rec_to_test_buffer( iv_doc_id = '0000000005'
                                            iv_value  = 10 ).
    DATA(lv_result) = lo_cut->get_value_plus_one( if_ztable1_reader = lo_mock_reader
                                                  iv_doc_id         = '0000000005' ).

    cl_aunit_assert=>assert_equals( act = lv_result
                                    exp = 11 ).
  ENDMETHOD.
ENDCLASS.

FORM main.
  DATA(lo_class) = NEW lcl_example_class( ).
  DATA(lo_reader) = NEW lcl_ztable1_reader( ).
  DATA(lv_result) = lo_class->get_value_plus_one( if_ztable1_reader = lo_reader
                                                  iv_doc_id         = p_doc_id ).
  WRITE 'The result is'.
  WRITE: / lv_result.
ENDFORM.

START-OF-SELECTION.
  PERFORM main.

*&---------------------------------------------------------------------*
*& Report ZUNIT_PROMO_EXAMPLE1
*&---------------------------------------------------------------------*
*& Example program for unit test promo article of master class 05.2021
*& Program with isolation variant 1 and test
*&---------------------------------------------------------------------*
REPORT zunit_promo_example2.

PARAMETERS: p_doc_id TYPE char10 MATCHCODE OBJECT ztable1_f4.

CLASS lcl_example_class DEFINITION.
  PUBLIC SECTION.
    DATA test_mode TYPE abap_bool.
    DATA test_buffer TYPE TABLE OF ztable1.
    METHODS get_value_plus_one
      IMPORTING iv_doc_id           TYPE char10
      RETURNING VALUE(return_value) TYPE i.
ENDCLASS.

CLASS lcl_example_class IMPLEMENTATION.
  METHOD get_value_plus_one.
    DATA lv_value TYPE i.

    IF test_mode = abap_true.

      READ TABLE test_buffer WITH KEY doc_id = iv_doc_id
        ASSIGNING FIELD-SYMBOL(<ls_row>).
      IF sy-subrc = 0.
        lv_value = <ls_row>-value.
      ENDIF.

    ELSE.
      SELECT SINGLE value
        INTO lv_value
        FROM ztable1
        WHERE doc_id = iv_doc_id.
    ENDIF.
    return_value = lv_value + 1.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_example_class_unit DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    METHODS run_test FOR TESTING.
ENDCLASS.

CLASS lcl_example_class_unit IMPLEMENTATION.
  METHOD run_test.
    DATA(lo_cut) = new lcl_example_class( ).
    lo_cut->test_mode = abap_true.
    lO_cut->test_buffer = VALUE #( ( doc_id = '0000000005' value = 10 ) ).
    DATA(lv_result) = lo_cut->get_value_plus_one( '0000000005' ).

    cl_aunit_assert=>assert_equals( act = lv_result
                                    exp = 11 ).
  ENDMETHOD.
ENDCLASS.

FORM main.
  DATA(lo_class) = NEW lcl_example_class( ).
  DATA(lv_result) = lo_class->get_value_plus_one( p_doc_id ).
  WRITE 'The result is'.
  WRITE: / lv_result.
ENDFORM.

START-OF-SELECTION.
  PERFORM main.

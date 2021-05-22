*&---------------------------------------------------------------------*
*& Report ZUNIT_PROMO_EXAMPLE1
*&---------------------------------------------------------------------*
*& Example program for unit test promo article of master class 05.2021
*& Initial program that will be tested
*&---------------------------------------------------------------------*
REPORT zunit_promo_example1.

PARAMETERS: p_doc_id TYPE char10 MATCHCODE OBJECT ztable1_f4.

CLASS lcl_example_class DEFINITION.
  PUBLIC SECTION.
    METHODS get_value_plus_one
      IMPORTING iv_doc_id           TYPE char10
      RETURNING VALUE(return_value) TYPE i.
ENDCLASS.

CLASS lcl_example_class IMPLEMENTATION.
  METHOD get_value_plus_one.
    DATA lv_value TYPE i.

    lv_value = '-1'.

    SELECT SINGLE value
      INTO lv_value
      FROM ztable1
      WHERE doc_id = iv_doc_id.

    return_value = lv_value + 1.
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

class ZCL_CALCULATOR definition
  public
  final
  create public .

public section.

  methods ADD
    importing
      !A type INT4
      !B type INT4
    returning
      value(RESULT) type INT4 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CALCULATOR IMPLEMENTATION.


  method ADD.
    result = a + b.
  endmethod.
ENDCLASS.

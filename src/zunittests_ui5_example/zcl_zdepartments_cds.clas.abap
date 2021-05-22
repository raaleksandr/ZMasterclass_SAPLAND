class ZCL_ZDEPARTMENTS_CDS definition
  public
  inheriting from CL_SADL_GTK_EXPOSURE_MPC
  final
  create public .

public section.
protected section.

  methods GET_PATHS
    redefinition .
  methods GET_TIMESTAMP
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZDEPARTMENTS_CDS IMPLEMENTATION.


  method GET_PATHS.
et_paths = VALUE #(
( |CDS~ZDEPARTMENTS_CDS| )
).
  endmethod.


  method GET_TIMESTAMP.
RV_TIMESTAMP = 20210505095518.
  endmethod.
ENDCLASS.

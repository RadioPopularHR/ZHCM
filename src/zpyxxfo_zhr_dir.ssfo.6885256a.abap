 data: lv_ev_entty type ppt_entty.
*       GV_FDATA  TYPE  PPT_FDATA.

 call function 'HR_PT_GET_ENTITY_CODE'
   exporting
     iv_bukrs               = v_bukrs
     iv_date                = sy-datum
  importing
    ev_entty               = lv_ev_entty
*  EXCEPTIONS
*    ENTITY_NOT_FOUND       = 1
*    OTHERS                 = 2
           .

  if sy-subrc eq 0.
    select single fdata from t5pfd
      into gv_fdata
      where entty = lv_ev_entty
      and field = 'COUNC'.

 endif.






















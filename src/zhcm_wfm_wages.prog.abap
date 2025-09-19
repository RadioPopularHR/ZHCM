*&---------------------------------------------------------------------*
*& Report ZHCM_WFM_WAGES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhcm_wfm_wages.
INCLUDE zhcm_wfm_wages_top.
INCLUDE zhcm_wfm_wages_f01.

START-OF-SELECTION.
  PERFORM get_pernr.

END-OF-SELECTION.
  PERFORM register_days.

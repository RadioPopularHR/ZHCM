*&---------------------------------------------------------------------*
*& Report ZHR_WFM_INTEGRACAO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhcm_wfm_integracao.

INCLUDE zhcm_wfm_integracao_top.
INCLUDE zhcm_wfm_integracao_scr.
INCLUDE zhcm_wfm_integracao_f01.

START-OF-SELECTION.
  PERFORM process_data.

END-OF-SELECTION.
  PERFORM send_email.

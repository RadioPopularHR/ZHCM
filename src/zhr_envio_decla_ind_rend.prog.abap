*&---------------------------------------------------------------------*
*& Report  ZHR_ENVIO_DECLA_IND_RENDIMENTO
*&
*&---------------------------------------------------------------------*
*& Author: Catarina Nunes (ROFF SDF)
*& Date:   13.01.2012
*& Title:  Impressão e envio de Declarações Individuais de
*&         Rendimentos (PDF)
*&
*&
*&---------------------------------------------------------------------*

REPORT  zhr_envio_decla_ind_rend.

INCLUDE zhr_envio_decla_ind_rend_top.
INCLUDE zhr_envio_decla_ind_rend_cls.
INCLUDE zhr_envio_decla_ind_rend_f01.
INCLUDE zhr_envio_decla_ind_rend_pai. " Módulos PAI
INCLUDE zhr_envio_decla_ind_rend_pbo. " Módulos PBO
INCLUDE zhr_envio_decla_ind_rend_log. " Include para rotinas de LOG

START-OF-SELECTION.
  gv_year = pa_year.


GET pernr.

  PERFORM get_payroll_pernr.

  IF gt_pernr IS NOT INITIAL.

*   Inicialização do log
    PERFORM app_init_log USING gc_object
                             gc_subobj.
    PERFORM get_data_alv.

  ENDIF.

 end-of-SELECTION.

    IF gt_final_alv IS NOT INITIAL.
*     Apresentar os dados numa ALV
      PERFORM display_alv.
    ELSE.
      MESSAGE text-001 TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.

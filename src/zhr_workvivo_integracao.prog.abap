*&---------------------------------------------------------------------*
*& Report ZHR_WORKVIVO_INTEGRACAO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhr_workvivo_integracao.

INCLUDE: zhr_workvivo_integracao_top,
         zhr_workvivo_integracao_f01.

INITIALIZATION.
  PERFORM f_initialization.

START-OF-SELECTION.
GET peras.
  PERFORM f_workvivo_info.

END-OF-SELECTION.
  IF gt_workvivo IS NOT INITIAL.
    PERFORM f_exibe_log.
  ELSE.
    MESSAGE 'Não há dados para exibição' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

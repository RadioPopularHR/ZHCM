*----------------------------------------------------------------------*
*                                                                      *
*       Subroutines for infotype 9009                                  *
*                                                                      *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_COD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_cod .
  CHECK P9009-COD_SECCAO IS NOT INITIAL.
  DATA: ls_sec TYPE zhr_seccao.
  SELECT SINGLE * FROM zhr_seccao
    INTO ls_sec
    WHERE cod_seccao = p9009-cod_seccao.

  IF sy-subrc <> 0.

    MESSAGE i006(zhcm) WITH p9009-cod_seccao.
    CLEAR: p9009-cod_seccao,
           p9009-desc_seccao.
    LEAVE SCREEN.
  ENDIF.
  p9009-desc_seccao = ls_sec-desc_seccao.
ENDFORM.                    " VERIFICA_COD

*----------------------------------------------------------------------*
*                                                                      *
*       Input-modules for infotype 9010                                *
*                                                                      *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  DESCRICAO_EQUIPA  INPUT
*&---------------------------------------------------------------------*
MODULE descricao_equipa INPUT.
  PERFORM f_descricao_equipa.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DESCRICAO_SUBAREA  INPUT
*&---------------------------------------------------------------------*
MODULE descricao_subarea INPUT.
  PERFORM f_descricao_subarea.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DESCRICAO_CENTRO_CST  INPUT
*&---------------------------------------------------------------------*
MODULE descricao_centro_cst INPUT.
  PERFORM f_descricao_centro_cst.

* >>> INI Inetum SAM EMP/SS HR 7000228548 21.04.2025
  IF p9010-centro_cst IS NOT INITIAL
 AND p9010-idchefia   IS INITIAL.
    SELECT SINGLE mail INTO p9010-idchefia
                       FROM zhr_centro_custo
                       WHERE kostl EQ p9010-centro_cst
                         AND begda LE p9010-endda
                         AND endda GE p9010-begda.
  ENDIF.
* <<< END Inetum SAM EMP/SS HR 7000228548 21.04.2025

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DESCRICAO_FUNCAO  INPUT
*&---------------------------------------------------------------------*
MODULE descricao_funcao INPUT.
  PERFORM f_descricao_funcao.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DESCRICAO_FUNCAO  INPUT
*&---------------------------------------------------------------------*
MODULE valida_email INPUT.
  PERFORM f_valida_email.
ENDMODULE.
* >>> INI Inetum SAM EMP/SS HR 7000228548 21.04.2025
*&---------------------------------------------------------------------*
*&      Module  CHECK_ON_SAVE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_on_save INPUT.

* Na gravação, validar se o campo do mail da chefia está preenchido
  IF sy-ucomm EQ 'UPD'
 AND p9010-idchefia IS INITIAL.
    MESSAGE e001(zhr_generic) WITH 'O campo ID da Chefia é'
                                   'de preenchimento obrigatório'.
  ENDIF.

ENDMODULE.
* <<< END Inetum SAM EMP/SS HR 7000228548 21.04.2025

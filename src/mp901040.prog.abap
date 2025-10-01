*----------------------------------------------------------------------*
*                                                                      *
*       Subroutines for infotype 9010                                  *
*                                                                      *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_DESCRICAO_CENTRO_CST
*&---------------------------------------------------------------------*
FORM f_descricao_centro_cst .

  CLEAR: ls_desc-centro, ls_desc-ktext.

  SELECT *
    FROM zhr_centro_custo
    INTO TABLE @DATA(lt_centro_custo)
    WHERE kostl EQ @p9010-centro_cst.

  IF sy-subrc IS INITIAL.
    SORT lt_centro_custo BY endda DESCENDING.
    LOOP AT lt_centro_custo INTO DATA(ls_centro_custo).
      IF ( p9010-begda BETWEEN ls_centro_custo-begda AND ls_centro_custo-endda ) AND
         ( p9010-endda BETWEEN ls_centro_custo-begda AND ls_centro_custo-endda ).
        ls_desc-centro = ls_centro_custo-descricao.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF p9010-centro_cst IS NOT INITIAL.
    SELECT SINGLE ktext
      INTO ls_desc-ktext
      FROM cskt INNER JOIN csks
        ON cskt~kokrs EQ csks~kokrs
       AND cskt~kostl EQ csks~kostl
       AND cskt~datbi EQ csks~datbi
     WHERE cskt~spras EQ sy-langu
       AND cskt~kostl EQ p9010-centro_cst
       AND csks~datab LE p9010-begda
       AND csks~datbi GE p9010-endda.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_DESCRICAO_SUBAREA
*&---------------------------------------------------------------------*
FORM f_descricao_subarea .

  CLEAR: ls_desc-subarea, ls_desc-btext.

  SELECT *
    FROM zhr_subarea
    INTO TABLE @DATA(lt_subarea)
    WHERE btrtl EQ @p9010-subarea.

  IF sy-subrc IS INITIAL.
    SORT lt_subarea BY endda DESCENDING.
    LOOP AT lt_subarea INTO DATA(ls_subarea).
      IF ( p9010-begda BETWEEN ls_subarea-begda AND ls_subarea-endda ) AND
         ( p9010-endda BETWEEN ls_subarea-begda AND ls_subarea-endda ).
        ls_desc-subarea = ls_subarea-descricao.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF p9010-subarea IS NOT INITIAL.
    SELECT SINGLE btext
      FROM t001p
      INTO ls_desc-btext
      WHERE btrtl EQ p9010-subarea.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DESCRICAO_EQUIPA
*&---------------------------------------------------------------------*
FORM f_descricao_equipa .

  CLEAR ls_desc-equipa.

  SELECT e~endda, e~begda, t~descricao
    INTO TABLE @DATA(lt_equipa)
    FROM zhr_des_equipa_t AS t
    INNER JOIN zhr_des_equipa AS e
    ON t~codigo EQ e~codigo
    AND t~endda EQ e~endda
    WHERE t~codigo EQ @p9010-equipa.

  IF sy-subrc IS INITIAL.
    SORT lt_equipa BY endda DESCENDING.
    LOOP AT lt_equipa INTO DATA(ls_equipa).
      IF ( p9010-begda BETWEEN ls_equipa-begda AND ls_equipa-endda ) AND
         ( p9010-endda BETWEEN ls_equipa-begda AND ls_equipa-endda ).
        ls_desc-equipa = ls_equipa-descricao.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DESCRICAO_FUNCAO
*&---------------------------------------------------------------------*
FORM f_descricao_funcao .

*  CLEAR: ls_desc-funcao, ls_desc-plstx.
  CLEAR: ls_desc-funcao.

  SELECT *
    FROM zhr_funcao
    INTO TABLE @DATA(lt_funcao)
    WHERE plans EQ @p9010-titulo_trabalho.

  IF sy-subrc IS INITIAL.
    SORT lt_funcao BY endda DESCENDING.
    LOOP AT lt_funcao INTO DATA(ls_funcao).
      IF ( p9010-begda BETWEEN ls_funcao-begda AND ls_funcao-endda ) AND
         ( p9010-endda BETWEEN ls_funcao-begda AND ls_funcao-endda ).
        ls_desc-funcao = ls_funcao-descricao.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDIF.

*  SELECT SINGLE plstx
*    FROM t528t
*    INTO ls_desc-plstx
*    WHERE plans EQ p9010-titulo_trabalho.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_EMAIL
*&---------------------------------------------------------------------*
FORM f_valida_email.

  FIND '@' IN p9010-email MATCH OFFSET DATA(lv_off).
  IF p9010-email+lv_off NE '@radiopopular.pt'.
    MESSAGE 'Email inválido' TYPE 'E'.
  ENDIF.

ENDFORM.

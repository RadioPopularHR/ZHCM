*&---------------------------------------------------------------------*
*& Report  ZHR_DATA_DIUTURNIDADES
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

PROGRAM  zhr_data_diuturnidades.
INCLUDE mppdat00.
TABLES: t001p,
        p0008.
TABLES: rp50d.

*---------------------------------------------------------------------*
*       FORM CALL_P0015                                               *
*---------------------------------------------------------------------*
FORM data_diuturn.

  DATA: t_p0001 TYPE TABLE OF p0001,
        l_p0001 type p0001,
        lv_anos TYPE anzhl,
        lv_data TYPE sydatum.                         "XAIP30K105961
  CLEAR rp50d.
  CALL FUNCTION 'HR_READ_INFOTYPE'
    EXPORTING
      pernr           = p0008-pernr
      infty           = '0001'
      begda           = p0008-begda
      endda           = p0008-endda
    TABLES
      infty_tab       = t_p0001
    EXCEPTIONS
      infty_not_found = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  SORT t_p0001 DESCENDING BY endda.
  DELETE t_p0001 WHERE sprps IS NOT INITIAL.
  READ TABLE t_p0001 INDEX 1 INTO l_p0001.

  SELECT SINGLE anos INTO lv_anos FROM zhrdiuturnidade
    WHERE trfar EQ p0008-trfar
    AND   trfgb EQ p0008-trfgb
    AND   btrtl EQ l_p0001-btrtl
    AND   begda LE p0008-begda
    AND   endda GE p0008-begda.

  IF sy-subrc EQ 0.
    lv_anos = 365 * lv_anos.
    lv_data = p0008-begda + lv_anos.
    rp50d-field1 = lv_data.
  ELSE.
    MESSAGE w000(zhcm).
  ENDIF.

ENDFORM.                    "data_diuturn

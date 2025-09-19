*----------------------------------------------------------------------*
***INCLUDE LZHR_RPCIIDP0F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GROUP_AMOUNTS_BY_RESID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_RESID_AMOUNTS  text
*      -->P_P_TAX_DATA  text
*      -->P_P_PERSONNEL_PERNR  text
*----------------------------------------------------------------------*
FORM group_amounts_by_resid
                TABLES p_tab_resid_amounts STRUCTURE resid_amounts
                       p_tax_data STRUCTURE tax_data1
                USING  p_pernr LIKE pernr-pernr.

  DATA: wa_resid_amounts LIKE resid_amounts.

  LOOP AT p_tax_data WHERE pernr EQ p_pernr.

    wa_resid_amounts-pernr = p_tax_data-pernr.
    wa_resid_amounts-resid = p_tax_data-resid.
    wa_resid_amounts-income = p_tax_data-gross_remun.
    wa_resid_amounts-deduction = p_tax_data-retent_remun.

    COLLECT wa_resid_amounts INTO p_tab_resid_amounts.
  ENDLOOP.

ENDFORM.                    "group_amounts_by_resid

*&---------------------------------------------------------------------*
*&      Form  REGROUP_TAX_DATA_WITHOUT_RESID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TAX_DATA  text
*      -->P_P_PERSONNEL_PERNR  text
*----------------------------------------------------------------------*
FORM regroup_tax_data_without_resid
              TABLES p_tax_data STRUCTURE tax_data1
              USING  p_pernr LIKE pernr-pernr.

  DATA: wa_tax_data TYPE ppt02_tax_data.

  LOOP AT p_tax_data INTO wa_tax_data WHERE pernr EQ p_pernr.
    IF NOT wa_tax_data-resid IS INITIAL.
      DELETE p_tax_data INDEX sy-tabix.
      CLEAR: wa_tax_data-resid.
      COLLECT wa_tax_data INTO p_tax_data.
    ENDIF.
  ENDLOOP.

  LOOP AT p_tax_data INTO wa_tax_data WHERE pernr EQ p_pernr.
    IF ( wa_tax_data-retent_remun EQ 0 )
    AND ( wa_tax_data-gross_remun EQ 0 ).
      DELETE p_tax_data INDEX sy-tabix.
    ENDIF.
  ENDLOOP.


ENDFORM.                    "group_amounts_by_resid

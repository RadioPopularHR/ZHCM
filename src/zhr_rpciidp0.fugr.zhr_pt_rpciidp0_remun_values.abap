FUNCTION zhr_pt_rpciidp0_remun_values.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(I_DATA) TYPE  PPT_FORM_IID
*"  TABLES
*"      P_PERSONNEL TYPE  PPT02_PERS
*"      P_TAX_DATA TYPE  PPT02_TAXD
*"      P_DEDUCTION TYPE  PPT02_DEDU
*"      P_REPORT TYPE  PPT02_REPO
*"      P_RESID_AMOUNTS STRUCTURE  ZHR_PT_RPCIIDP0_RESID_AMOUNTS
*"----------------------------------------------------------------------

  DATA: fl_loop TYPE c.
  DATA: p_return_code LIKE sy-subrc.
  DATA: ls_tax_data TYPE ppt_form_tax_data,"local data of pdf
        ls_deducts TYPE ppt_form_deducts,"local data of pdf
        ls_incash TYPE ppt_form_incash,"local data of pdf
        ls_resid_amounts TYPE ppt_form_resid_amounts.

  LOOP AT p_personnel.
    CLEAR p_tax_data.

    LOOP AT p_report WHERE pernr EQ p_personnel-pernr.
      MOVE p_report-deduct     TO p_tax_data-retent_remun.
      MOVE p_report-tot_income TO p_tax_data-gross_remun.
      MOVE p_report-resid      TO p_tax_data-resid.
      IF NOT p_tax_data IS INITIAL.
        MOVE p_report-pernr    TO p_tax_data-pernr.
        MOVE p_report-inc_type TO p_tax_data-inc_type.
        COLLECT p_tax_data.
      ENDIF.
      DELETE p_report.

      LOOP AT p_tax_data WHERE pernr EQ p_personnel-pernr.
        IF p_tax_data-gross_remun LT 0.
          p_tax_data-gross_remun = 0.
          MODIFY p_tax_data.
          CLEAR p_tax_data.
        ENDIF.
      ENDLOOP.

      CLEAR p_tax_data.
    ENDLOOP.

    PERFORM group_amounts_by_resid TABLES p_resid_amounts
                                          p_tax_data
                                   USING  p_personnel-pernr.

    LOOP AT p_resid_amounts WHERE pernr EQ p_personnel-pernr.

      CASE p_resid_amounts-resid.
        WHEN 'P'.
          CONCATENATE text-047 text-048
            INTO ls_resid_amounts-resid_text
              SEPARATED BY space.
        WHEN 'I'.
          CONCATENATE text-047 '  ' text-049
            INTO ls_resid_amounts-resid_text
              SEPARATED BY space.
        WHEN 'M'.
          CONCATENATE text-047 '  ' text-050
            INTO ls_resid_amounts-resid_text
              SEPARATED BY space.
        WHEN 'A'.
          CONCATENATE  text-047 '  ' text-051
            INTO ls_resid_amounts-resid_text
              SEPARATED BY space.
      ENDCASE.

      MOVE p_resid_amounts-deduction TO ls_resid_amounts-retent_remun.
      MOVE p_resid_amounts-income    TO ls_resid_amounts-gross_remun.
      APPEND ls_resid_amounts        TO i_data-resid_amounts.

    ENDLOOP.

    PERFORM regroup_tax_data_without_resid TABLES p_tax_data
                                           USING  p_personnel-pernr.

    LOOP AT p_tax_data WHERE pernr EQ p_personnel-pernr AND
                           ( inc_type EQ 'A'  OR
                             inc_type EQ 'H'  OR
                             inc_type EQ income_hp ).

      IF p_tax_data-inc_type EQ income_a.
        ls_tax_data-itype = text-019.
      ELSE.
        IF p_tax_data-inc_type EQ income_h.
          ls_tax_data-itype = text-020.
        ELSEIF p_tax_data-inc_type EQ income_hp.
          ls_tax_data-itype = text-040.
        ENDIF.
      ENDIF.

      MOVE-CORRESPONDING p_tax_data TO ls_tax_data.
      APPEND ls_tax_data TO i_data-amounts.
      CLEAR ls_tax_data.
    ENDLOOP.

    CLEAR fl_loop.

    LOOP AT p_deduction WHERE pernr EQ p_personnel-pernr.


      MOVE-CORRESPONDING p_deduction TO ls_deducts.
      APPEND ls_deducts TO i_data-deducts.
      CLEAR ls_deducts.
    ENDLOOP.

    CLEAR fl_loop.

    LOOP AT p_tax_data WHERE pernr EQ p_personnel-pernr AND
                             inc_type EQ 'I'.

      IF p_tax_data-inc_type EQ income_i.

        ls_incash-itype = text-036.
      ENDIF.

      MOVE-CORRESPONDING p_tax_data TO ls_incash.
      APPEND ls_incash TO i_data-incash.
      CLEAR ls_incash.
    ENDLOOP.

    CLEAR fl_loop.

    SORT p_report BY pernr rep_year.
    LOOP AT p_report WHERE pernr EQ p_personnel-pernr.

    ENDLOOP.
  ENDLOOP.

ENDFUNCTION.

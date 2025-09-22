CLEAR: layout2, layout3, layout4, layout5.

* INI ROFF SAM HR MN/EMP 7000033777 em 06.02.2017
CLEAR: layout_prev_years.
* END ROFF SAM HR MN/EMP 7000033777 em 06.02.2017

DATA: w_output2 TYPE  zhr_dir_s_tax_data,
      w_output3 TYPE  zhr_dir_s_deduction,
      w_output4 TYPE  zhr_dir_s_resid_amounts,
      w_output5 TYPE  zhr_dir_s_sub_natal.
* INI ROFF SAM HR MN/EMP 7000033777 em 06.02.2017
DATA: ls_prev_years TYPE zhr_dir_s_inc_prev_years.

READ TABLE t_prev_years INTO ls_prev_years
     WITH KEY pernr = w_output-pernr.
IF sy-subrc = 0.
  layout_prev_years = 'X'.
ENDIF.
* END ROFF SAM HR MN/EMP 7000033777 em 06.02.2017

READ TABLE t_output2 INTO w_output2
WITH KEY pernr = w_output-pernr.

IF sy-subrc = 0.
  layout2 = 'X'.
ENDIF.

READ TABLE t_output3 INTO w_output3
WITH KEY pernr = w_output-pernr.

IF sy-subrc = 0.
  layout3 = 'X'.
ENDIF.

READ TABLE t_output4 INTO w_output4
WITH KEY pernr = w_output-pernr.

IF sy-subrc = 0.
  layout4 = 'X'.
ENDIF.

READ TABLE t_output5 INTO w_output5
WITH KEY pernr = w_output-pernr.

IF sy-subrc = 0.
  layout5 = 'X'.
ENDIF.
* >>> INI ROFF SAM EMP/SM HR 7000089707 14.02.2020
READ TABLE t_suj_englob INTO w_output6
WITH KEY pernr = w_output-pernr.
IF sy-subrc = 0.
  layout6 = 'X'.
ENDIF.
* <<< END ROFF SAM EMP/SM HR 7000089707 14.02.2020
* >>> INI ROFF SAM EMP/SM HR 7000089707 21.02.2020
READ TABLE t_total_rend INTO wa_total_rend
WITH KEY pernr = w_output-pernr.
IF sy-subrc = 0.
  layout7 = 'X'.
ENDIF.
* <<< END ROFF SAM EMP/SM HR 7000089707 21.02.2020

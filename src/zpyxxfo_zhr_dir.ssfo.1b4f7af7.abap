CLEAR: v_tipo_ded.
CASE w_output4-resid.
* WHEN 'P'.
*   CONCATENATE 'Incluídos no Imposto retido a titulo'
*   'de sobretaxa no Continente' INTO V_TIPO_DED SEPARATED BY SPACE.
*WHEN 'I'.
*  CONCATENATE 'Incluídos no Imposto retido a titulo'
*   'de sobretaxa em Açores' INTO V_TIPO_DED SEPARATED by space.
*WHEN 'M'.
*    CONCATENATE 'Incluídos no Imposto retido a titulo'
*   'de sobretaxa em Madeira' INTO V_TIPO_DED SEPARATED by space.
*WHEN 'A'.
*      CONCATENATE 'Incluídos no Imposto retido a titulo'
* 'de sobretaxa como não residente' INTO V_TIPO_DED SEPARATED by space.

* --------------------------------------
* INI ROFF SAM HR SS/EMP 29.01.2014
* 5000000253 - Texto sobretaxa formulário DIR
  WHEN 'P'.
    CONCATENATE 'Importância retida a titulo de Sobretaxa (não incluido noutros campos)'
     'no Continente' INTO v_tipo_ded SEPARATED BY space.
  WHEN 'I'.
    CONCATENATE 'Importância retida a titulo de Sobretaxa (não incluido noutros campos)'
     'em Açores' INTO v_tipo_ded SEPARATED BY space.
  WHEN 'M'.
    CONCATENATE 'Importância retida a titulo de Sobretaxa (não incluido noutros campos)'
     'em Madeira' INTO v_tipo_ded SEPARATED BY space.
  WHEN 'A'.
    CONCATENATE 'Importância retida a titulo de Sobretaxa (não incluido noutros campos)'
     'como não residente' INTO v_tipo_ded SEPARATED BY space.
ENDCASE.
* 5000000253 - Texto sobretaxa formulário DIR
* END ROFF SAM HR MR/EMP 29.01.2014
* --------------------------------------

CONDENSE v_tipo_ded.

















*CLEAR: V_TIPO_DED.
*    CASE WA_TOTAL_REND-RESID.
*      WHEN 'P'.
*        V_TIPO_DED = 'Incluídos valores auferidos no Continente'.
*      WHEN 'I'.
*        V_TIPO_DED = 'Incluídos valores auferidos em Açores'.
*      WHEN 'M'.
*        V_TIPO_DED = 'Incluídos valores auferidos em Madeira'.
*      WHEN 'A'.
*        V_TIPO_DED = 'Incluídos valores auferidos como não residente'.
*    ENDCASE.
*
*    CONDENSE V_TIPO_DED.

CLEAR: v_tipo_rend.
CASE wa_total_rend-inc_type(1).
  WHEN 'A'.
    CONCATENATE wa_total_rend-inc_type '- TRABALHO DEPENDENTE'
    INTO v_tipo_rend SEPARATED BY space.
  WHEN 'B'.
    CONCATENATE wa_total_rend-inc_type '- TRABALHO INDEPENDENTE'
    INTO v_tipo_rend SEPARATED BY space.

  WHEN 'C'.
    CONCATENATE wa_total_rend-inc_type '- CAPITAIS'
    INTO v_tipo_rend SEPARATED BY space.

  WHEN 'F'.
    CONCATENATE wa_total_rend-inc_type '- PREDIAS'
    INTO v_tipo_rend SEPARATED BY space.

  WHEN 'G'.
    CONCATENATE wa_total_rend-inc_type '- INCREMENTOS PATRIMONIAIS'
    INTO v_tipo_rend SEPARATED BY space.

  WHEN 'H'.
    CONCATENATE wa_total_rend-inc_type '- PENSÕES'
    INTO v_tipo_rend SEPARATED BY space.
ENDCASE.


















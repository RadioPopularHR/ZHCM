CLEAR: v_tipo_ded.
CASE wa_total_rend-resid.
  WHEN 'P'.
    v_tipo_ded = 'Informações adicionais de valores auferidos no Continente'.
  WHEN 'I'.
    v_tipo_ded = 'Informações adicionais de valores auferidos nos Açores'.
  WHEN 'M'.
    v_tipo_ded = 'Informações adicionais de valores auferidos na Madeira'.
  WHEN 'A'.
    v_tipo_ded = 'Informações adicionais de valores auferidos como não residente'.
ENDCASE.

CONDENSE v_tipo_ded.

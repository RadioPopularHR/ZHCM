CLEAR: V_TIPO_DED.
    CASE W_OUTPUT4-RESID.
      WHEN 'P'.
        V_TIPO_DED = 'Incluídos valores auferidos no Continente'.
      WHEN 'I'.
        V_TIPO_DED = 'Incluídos valores auferidos em Açores'.
      WHEN 'M'.
        V_TIPO_DED = 'Incluídos valores auferidos em Madeira'.
      WHEN 'A'.
        V_TIPO_DED = 'Incluídos valores auferidos como não residente'.
    ENDCASE.

    CONDENSE V_TIPO_DED.























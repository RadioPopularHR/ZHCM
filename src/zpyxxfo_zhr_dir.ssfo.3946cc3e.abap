DATA: wa_ZHR_DIR_BUKRS TYPE ZHR_DIR_BUKRS.

SELECT * FROM ZHR_DIR_BUKRS INTO wa_ZHR_DIR_BUKRS
  WHERE bukrs = v_BUKRS
    AND begda LE sy-datum
    AND endda GE sy-datum.
ENDSELECT.
v_logo = wa_ZHR_DIR_BUKRS-logo.
v_assinatura = wa_ZHR_DIR_BUKRS-assinatura.






















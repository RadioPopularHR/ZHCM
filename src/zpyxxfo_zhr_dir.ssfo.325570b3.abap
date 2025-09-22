*
*Nota: Este código foi comentado porque a data legal para
*      emissão da DIR é 20 de Janeiro do ano vigente
*
*data: lv_mes type text10.
*
*clear v_date.
*
*select single ltx
*  from t247
*  into lv_mes
*  where spras eq 'PT'
*  and mnr eq sy-datum+4(2).
*
*
*  concatenate sy-datum+6(2) 'de' lv_mes 'de' sy-datum(4)
*  into v_date separated by space.

  concatenate '20 de Janeiro de' sy-datum(4)
  into v_date separated by space.























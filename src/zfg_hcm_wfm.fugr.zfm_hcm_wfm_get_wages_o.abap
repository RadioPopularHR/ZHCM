FUNCTION ZFM_HCM_WFM_GET_WAGES_O .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ZHCM_S_I_DUMMY
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ZHCM_S_O_WAGES
*"----------------------------------------------------------------------
  zcl_hcm_ws_wfm=>initialize_interface(
    EXPORTING
      iv_id       = 'ZHCM_WFM_WAGES_O'
      iv_method   = 'GET_WAGES_O'
      iv_request  = 'GET'
      is_input    = input
      iv_url      = '/Integrations/api/IntegrationOut/Wages'
    CHANGING
      cs_output   = output
  ).
ENDFUNCTION.

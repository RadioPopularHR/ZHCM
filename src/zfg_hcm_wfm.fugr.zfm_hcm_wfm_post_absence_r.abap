FUNCTION ZFM_HCM_WFM_POST_ABSENCE_R.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ZHCM_S_I_ABSENCE_R
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ZHCM_S_WFM_OUTPUT
*"----------------------------------------------------------------------
  zcl_hcm_ws_wfm=>initialize_interface(
    EXPORTING
      iv_id       = 'ZHCM_WFM_ABSENCE_R'
      iv_method   = space
      iv_request  = 'POST'
      is_input    = input
      iv_url      = '/Integrations/api/IntegrationIn/AbsenceReason'
    CHANGING
      cs_output   = output
  ).
ENDFUNCTION.

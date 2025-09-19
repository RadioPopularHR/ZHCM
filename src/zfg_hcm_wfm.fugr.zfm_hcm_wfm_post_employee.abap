FUNCTION zfm_hcm_wfm_post_employee.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ZHCM_S_I_EMPLOYEE
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ZHCM_S_WFM_OUTPUT
*"----------------------------------------------------------------------
  zcl_hcm_ws_wfm=>initialize_interface(
    EXPORTING
      iv_id       = 'ZHCM_WFM_EMPLOYEE'
      iv_method   = space
      iv_request  = 'POST'
      is_input    = input
      iv_url      = '/Integrations/api/IntegrationIn/Employee'
    CHANGING
      cs_output   = output
  ).
ENDFUNCTION.

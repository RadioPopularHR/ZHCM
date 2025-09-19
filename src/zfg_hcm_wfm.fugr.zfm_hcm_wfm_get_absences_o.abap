FUNCTION zfm_hcm_wfm_get_absences_o .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ZHCM_S_I_DUMMY
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ZHCM_S_O_ABSENCES
*"----------------------------------------------------------------------

    zcl_hcm_ws_wfm=>initialize_interface(
      EXPORTING
        iv_id       = 'ZHCM_WFM_ABSENCES_O'
        iv_method   = 'GET_ABSENCES_O'
        iv_request  = 'GET'
        is_input    = input
        iv_url      = '/Integrations/api/IntegrationOut/Absence'
      CHANGING
        cs_output   = output
    ).

ENDFUNCTION.

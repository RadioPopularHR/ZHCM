class ZCL_HCM_WS_WFM definition
  public
  inheriting from ZZCA_CL_WS_GENERAL
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_NO_COMMIT type FLAG optional
      !IV_ID_IF type STRING optional .
  class-methods INITIALIZE_INTERFACE
    importing
      !IV_ID type STRING
      !IV_METHOD type STRING
      !IV_REQUEST type STRING
      !IS_INPUT type ANY
      !IV_URL type STRING
    changing
      !CS_OUTPUT type ANY .
  class-methods INFOTYPE_OPERATION
    importing
      !IV_INFOTYPE type INFTY
      !IV_EMPLOYEE_NUMBER type PERSNO
      !IV_SUBTY type SUBTY
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
      !IV_OPERATION type ACTIO
      !IV_SEQNR type SEQNR optional
      !IV_SPRPS type SPRPS optional
      !IV_COMMIT type BOOLEAN
      !IS_RECORD type ANY
    exporting
      !ES_RETURN type BAPIRETURN1
      !ES_KEY type BAPIPAKEY .
  class-methods INFOTYPE_OPERATION_DELETE
    importing
      !IV_INFOTYPE type INFTY
      !IV_EMPLOYEE_NUMBER type PERSNO
      !IV_SUBTY type SUBTY
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
      !IV_OPERATION type ACTIO
      !IV_SEQNR type SEQNR optional
      !IV_SPRPS type SPRPS optional
      !IV_COMMIT type BOOLEAN
    exporting
      !ES_RETURN type BAPIRETURN1
      !ES_KEY type BAPIPAKEY
    changing
      !CS_RECORD type ANY .
  class-methods INFOTYPE_OPERATION_INSERT
    importing
      !IV_INFOTYPE type INFTY
      !IV_EMPLOYEE_NUMBER type PERSNO
      !IV_SUBTY type SUBTY
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
      !IV_OPERATION type ACTIO
      !IV_SEQNR type SEQNR optional
      !IV_SPRPS type SPRPS optional
      !IV_COMMIT type BOOLEAN
    exporting
      !ES_RETURN type BAPIRETURN1
      !ES_KEY type BAPIPAKEY
    changing
      !CS_RECORD type ANY .

  methods GET_INFO_IN
    redefinition .
  methods GET_INFO_OUT
    redefinition .
  methods SET_INFO_IN
    redefinition .
  methods SET_INFO_OUT
    redefinition .
protected section.
private section.

  data GS_INFO_IN type ref to DATA .
  data GS_INFO_OUT type ref to DATA .
  data GV_DOMAIN type STRING .
  data GV_TOKEN type STRING .
  data GV_TEST type BOOLEAN .
  data GT_SUBTYPE type ZTTWFM_HCM_SUBTYP .
  constants GC_MSGID type STRING value 'ZWFM' ##NO_TEXT.

  methods HTTP_REQUEST
    importing
      !IV_URL type STRING
      !IV_REQUEST type STRING
      !IV_DATA type STRING
      !IT_FIELDS type TIHTTPNVP
    exporting
      !EV_STATUS_CODE type I
      !EV_OPERATION_INFO type STRING
      !EV_RESPONSE type STRING .
  methods GET_TOKEN
    exporting
      !EV_TOKEN type STRING .
  methods AUTHENTICATE .
  methods DELIVERY_CONFIRMATION
    importing
      !IV_CONF_ID type STRING
      !IO_FLUXO type ref to ZCL_HCM_WS_WFM .
  methods REQUEST
    importing
      !IS_INPUT type ANY
      !IO_FLUXO type ref to ZCL_HCM_WS_WFM
      !IV_URL type STRING
      !IV_REQUEST type STRING
    exporting
      !EV_RESPONSE type STRING
    changing
      !CS_OUTPUT type ANY
      !CV_ERROR type ZZCA_STATWS .
  methods GET_ABSENCES_O
    importing
      !IO_FLUXO type ref to ZCL_HCM_WS_WFM
      !IV_RESPONSE type STRING
    changing
      !CS_OUTPUT type ANY
      !CV_ERROR type ZZCA_STATWS
      !CV_FLAG type ABAP_BOOL
      !CV_CONFIRM type STRING .
  methods GET_SCHEDULE_O
    importing
      !IO_FLUXO type ref to ZCL_HCM_WS_WFM
      !IV_RESPONSE type STRING
    changing
      !CS_OUTPUT type ANY
      !CV_ERROR type ZZCA_STATWS .
  methods GET_WAGES_O
    importing
      !IO_FLUXO type ref to ZCL_HCM_WS_WFM
      !IV_RESPONSE type STRING
    changing
      !CS_OUTPUT type ANY
      !CV_ERROR type ZZCA_STATWS
      !CV_FLAG type ABAP_BOOL
      !CV_CONFIRM type STRING .
  methods GET_DATA_O
    importing
      !IO_FLUXO type ref to ZCL_HCM_WS_WFM
      !IV_RESPONSE type STRING
    changing
      !CS_OUTPUT type ANY
      !CV_ERROR type ZZCA_STATWS
      !CV_FLAG type ABAP_BOOL
      !CV_CONFIRM type STRING .
  methods ADD_TEXT_LOG
    importing
      !IV_TEXT type STRING
      !IV_MSGTY type SYMSGTY .
  methods GET_DOMAIN .
  methods GET_SUBTYPE .
  methods GET_RECORD
    importing
      !IV_API type STRING
      !IS_DATA type ANY
    exporting
      !ES_RECORD type ANY .
  methods REQUEST_STATUS
    importing
      !IS_INPUT type ANY
      !IO_FLUXO type ref to ZCL_HCM_WS_WFM
      !IV_URL type STRING
      !IV_REQUEST type STRING
    exporting
      !EV_RESPONSE type STRING
    changing
      !CS_OUTPUT type ANY
      !CV_ERROR type ZZCA_STATWS
      !CV_STATUS type NUMC5 .
  methods ADICIONA_OUTPUT
    importing
      !IV_METHOD type STRING
      !IS_OUTPUT type ANY
    changing
      !CS_OUTPUT type ANY .
  methods GET_COLABORADORES
    importing
      !EMPLOYEE_ID type STRING
    exporting
      !VALUE type CHAR1 .
ENDCLASS.



CLASS ZCL_HCM_WS_WFM IMPLEMENTATION.


  METHOD add_text_log.
    DATA: lt_split TYPE TABLE OF swastrtab,

          ls_msg   TYPE bal_s_msg.

    CALL FUNCTION 'SWA_STRING_SPLIT'
      EXPORTING
        input_string                 = iv_text
        max_component_length         = 50
      TABLES
        string_components            = lt_split
      EXCEPTIONS
        max_component_length_invalid = 1
        OTHERS                       = 2.

    DATA(lv_len) = lines( lt_split ).

    IF lv_len > 0.
      ls_msg-msgid = 'ZWFM'.
      ls_msg-msgno = 0.
      ls_msg-msgty = iv_msgty.
      ls_msg-msgv1 = lt_split[ 1 ]-str.

      IF lv_len > 1.
        ls_msg-msgv2 = lt_split[ 2 ]-str.

        IF lv_len > 2.
          ls_msg-msgv3 = lt_split[ 3 ]-str.

          IF lv_len > 3.
            ls_msg-msgv4 = lt_split[ 4 ]-str.
          ENDIF.
        ENDIF.
      ENDIF.

      add_msg_log(
        EXPORTING
          us_msg    = ls_msg                 " Log de aplicação: dados de uma mensagem
        EXCEPTIONS
          error_log = 1                " Erro em registo de Log
          OTHERS    = 2
      ).
    ENDIF.
  ENDMETHOD.


  METHOD adiciona_output.
    DATA: ls_output_a     TYPE zhcm_s_o_absences,
          ls_output_a_aux TYPE zhcm_s_o_absences,
          ls_output_w     TYPE zhcm_s_o_wages,
          ls_output_w_aux TYPE zhcm_s_o_wages,
          ls_output_d     TYPE zhcm_s_o_data,
          ls_output_d_aux TYPE zhcm_s_o_data.

    CASE iv_method.
      WHEN 'GET_ABSENCES_O'.
        ls_output_a = is_output.
        ls_output_a_aux = cs_output.

        IF ls_output_a-status_code NE 'E'.
          ls_output_a_aux-status_code = ls_output_a-status_code.
          ls_output_a_aux-operation_info = ls_output_a-operation_info.

          LOOP AT ls_output_a-absence_result INTO DATA(ls_a_aux).
            APPEND ls_a_aux TO ls_output_a_aux-absence_result.
          ENDLOOP.
        ENDIF.

        DELETE ADJACENT DUPLICATES FROM ls_output_a_aux-absence_result.
        cs_output = ls_output_a_aux.

      WHEN 'GET_WAGES_O'.
        ls_output_w = is_output.
        ls_output_w_aux = cs_output.

        IF ls_output_w-status_code NE 'E'.
          ls_output_w_aux-status_code = ls_output_w-status_code.
          ls_output_w_aux-operation_info = ls_output_w-operation_info.

          LOOP AT ls_output_w-wages_result INTO DATA(ls_w_aux).
            APPEND ls_w_aux TO ls_output_w_aux-wages_result.
          ENDLOOP.
        ENDIF.

        DELETE ADJACENT DUPLICATES FROM ls_output_w_aux-wages_result.
        cs_output = ls_output_w_aux.

      WHEN 'GET_DATA_O'.
        ls_output_d = is_output.
        ls_output_d_aux = cs_output.

        IF ls_output_d-status_code NE 'E'.
          ls_output_d_aux-status_code = ls_output_d-status_code.
          ls_output_d_aux-operation_info = ls_output_d-operation_info.

          LOOP AT ls_output_d-data_result INTO DATA(ls_d_aux).
            APPEND ls_d_aux TO ls_output_d_aux-data_result.
          ENDLOOP.
        ENDIF.

        DELETE ADJACENT DUPLICATES FROM ls_output_d_aux-data_result.
        cs_output = ls_output_d_aux.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD authenticate.

    TYPES: BEGIN OF ty_s_data,
             token TYPE string,
           END OF ty_s_data.

    TYPES: BEGIN OF ty_s_authentication,
             token    TYPE string,
             sid      TYPE string,
             success  TYPE string,
             code     TYPE string,
             codetype TYPE string,
             message  TYPE string,
             data     TYPE ty_s_data,
           END OF ty_s_authentication.

    DATA: lt_fields         TYPE tihttpnvp,

          ls_json           TYPE ty_s_authentication,

          lv_status_code    TYPE i,
          lv_operation_info TYPE string,
          lv_response       TYPE string,
          lv_msg            TYPE string,
          lv_msg_status     TYPE string.

    APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).
    <fs_fields>-name = 'Tlan-Bff-name'.
    <fs_fields>-value = 'WFMINTEGRATION'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Tlan-Bff-enrolment-id'.
    <fs_fields>-value = 'd7771511d69848cb08db28785de57ef6cf1dbedf'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Tlan-Bff-channel'.
    <fs_fields>-value = 'EXT_APIS'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Tlan-Bff-culture-code'.
    <fs_fields>-value = 'pt-PT'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'AuthenticationKey'.
    <fs_fields>-value = '13f48b18e0825e28adf4d4543d0c75c2556512bc'.

    http_request(
      EXPORTING
        iv_url            = gv_domain && '/Authentication/ExtAuthentication'
        iv_request        = 'POST'
        iv_data           = space
        it_fields         = lt_fields
      IMPORTING
        ev_status_code    = lv_status_code
        ev_operation_info = lv_operation_info
        ev_response       = lv_response
    ).

    CASE lv_status_code.
      WHEN 200.
        /ui2/cl_json=>deserialize(
          EXPORTING
            json             = lv_response
            pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
          CHANGING
            data             = ls_json
        ).

        gv_token = ls_json-data-token.
      WHEN OTHERS.
        lv_msg_status = lv_status_code.

        CONCATENATE 'Authentication Error: ' lv_msg_status '-' lv_operation_info
        INTO lv_msg SEPARATED BY space.

        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
    ENDCASE.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( iv_no_commit ).
    gv_interface_id = iv_id_if.
    get_domain( ).
    get_subtype( ).
    authenticate( ).
  ENDMETHOD.


  METHOD delivery_confirmation.
    TYPES: BEGIN OF ty_s_input,
             delivery_confirmation_id TYPE string,
           END OF ty_s_input.

    DATA: lt_fields         TYPE tihttpnvp,

          ls_msg            TYPE bal_s_msg,
          ls_input          TYPE ty_s_input,

          lv_data           TYPE string,
          lv_status_code    TYPE i,
          lv_operation_info TYPE string,
          lv_response       TYPE string.

    CHECK io_fluxo->gv_test EQ abap_false.

    APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).
    <fs_fields>-name = 'Tlan-Bff-name'.
    <fs_fields>-value = 'WFMINTEGRATION'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Tlan-Bff-enrolment-id'.
    <fs_fields>-value = 'd7771511d69848cb08db28785de57ef6cf1dbedf'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Tlan-Bff-channel'.
    <fs_fields>-value = 'EXT_APIS'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Tlan-Bff-culture-code'.
    <fs_fields>-value = 'pt-PT'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Tlan-Bff-app-os'.
    <fs_fields>-value = 'hd2kLym9pM4tNXv8Byfu7R7rrUyB2uQt'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Authorization'.

    io_fluxo->get_token(
      IMPORTING
        ev_token = <fs_fields>-value
    ).

    CONCATENATE 'Bearer ' <fs_fields>-value INTO <fs_fields>-value RESPECTING BLANKS.

*FALTA VALIDAR - Se o p_teste estive checked - Não passa o confirmation_id
    ls_input-delivery_confirmation_id = iv_conf_id.

    /ui2/cl_json=>serialize(
      EXPORTING
        data             = ls_input
        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
      RECEIVING
        r_json           = lv_data
    ).

    DO 2 TIMES.

      io_fluxo->http_request(
        EXPORTING
          iv_url            = io_fluxo->gv_domain && '/Integrations/api/IntegrationOut/DeliveryConfirmation'
          iv_request        = 'PUT'
          iv_data           = lv_data
          it_fields         = lt_fields
        IMPORTING
          ev_status_code    = lv_status_code
          ev_operation_info = lv_operation_info
          ev_response       = lv_response
      ).

      IF lv_status_code NE 401.
        EXIT.
      ENDIF.

    ENDDO.
  ENDMETHOD.


  METHOD get_absences_o.
    TYPES: BEGIN OF ty_s_data,
             absence_result       TYPE ztt_hcm_absence_result,
             absence_resul_out_id TYPE string,
           END OF ty_s_data.

    TYPES: BEGIN OF ty_s_absences_o,
             token    TYPE string,
             sid      TYPE string,
             success  TYPE string,
             code     TYPE string,
             codetype TYPE string,
             message  TYPE string,
             data     TYPE ty_s_data,
           END OF ty_s_absences_o.

    TYPES: BEGIN OF ty_s_pa.
             INCLUDE TYPE pakey.
             INCLUDE TYPE pshd1.
             INCLUDE TYPE ps2001.
           TYPES: END OF ty_s_pa.

    DATA: lt_integ     TYPE TABLE OF ztwfm_hcm_integ,

          lr_id        TYPE RANGE OF ztwfm_hcm_integ-id,

          ls_json      TYPE ty_s_absences_o,
          ls_absences  TYPE zstruct_hcm_absence_result,
          ls_return    TYPE bapireturn1,
          ls_key       TYPE bapipakey,
          ls_msg       TYPE bal_s_msg,

          lv_operation TYPE actio,
          lv_response  TYPE string,
          lv_error     TYPE zzca_statws,
          lv_data      TYPE REF TO data,
          lv_date_diff TYPE p.

    DATA:
      o_ref        TYPE REF TO data,

      lv_error_aux TYPE zzca_statws,
      lv_auxiliar  TYPE tabname,
      lt_pa        TYPE TABLE OF ty_s_pa.

    FIELD-SYMBOLS:
      <fs_t_table>    TYPE STANDARD TABLE.

*    DATA: lt_absences_o TYPE TABLE OF ty_s_absences_aux_o,
    DATA: lt_absences_o TYPE TABLE OF zhcm_absences,
          ls_absences_o TYPE zhcm_absences,
          lt_id         TYPE zhcm_tt_id,
          lv_prev_day   TYPE datum,
          lv_last_day   TYPE datum,
          lv_date       TYPE datum.

***********************************************************************************

    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = iv_response
        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data             = ls_json
    ).

    LOOP AT ls_json-data-absence_result INTO DATA(ls_absences_aux).
      APPEND INITIAL LINE TO lr_id ASSIGNING FIELD-SYMBOL(<fs_id>).
      <fs_id>-sign = 'I'.
      <fs_id>-option = 'EQ'.
      <fs_id>-low = ls_absences_aux-id.
    ENDLOOP.

    SELECT id
      FROM ztwfm_hcm_integ
      INTO TABLE @DATA(lt_integrated)
      WHERE id IN @lr_id
        AND api = 'ABSENCES'
        AND integrated = @abap_true.

    IF sy-subrc = 0.
      LOOP AT lt_integrated INTO DATA(ls_integrated).
        SHIFT ls_integrated-id LEFT DELETING LEADING '0'.
        DELETE ls_json-data-absence_result WHERE id = ls_integrated-id.
      ENDLOOP.
    ENDIF.

* >>> INI Inetum SAM EMP/SS HR 7000206480 07.08.2024
* Comentado, para que fique tudo ordenado pela ordenação original
*    SORT ls_json-data-absence_result BY employee_id
*                                        absence_reason
*                                        absence_begin_date.

    SORT ls_json-data-absence_result BY employee_id id.
* <<< END Inetum SAM EMP/SS HR 7000206480 07.08.2024

* >>> INI Inetum SAM EMP/SS HR 7000202896 24.06.2024
* Comentado
*    io_fluxo->get_colaboradores(
*     EXPORTING
*         results = ls_json-data-absence_result
*     IMPORTING
*       value = DATA(lv_value) ).
*
*    "Se os colaboradores existirem
*    IF lv_value EQ abap_false.
* <<< END Inetum SAM EMP/SS HR 7000202896 24.06.2024

    LOOP AT ls_json-data-absence_result INTO ls_absences. "DATA(ls_absences).
* >>> INI Inetum SAM EMP/SS HR 7000202896 24.06.2024
      io_fluxo->get_colaboradores(
       EXPORTING
           employee_id = ls_absences-employee_id
       IMPORTING
         value = DATA(lv_value) ).

      IF lv_value EQ abap_false.
        "Se os colaboradores existirem
      ELSE.
        "Se os colaboradores não existirem
        DATA: lv_msg2 TYPE string.
        CONCATENATE 'Colaborador' ls_absences-employee_id 'inexistente em SAP.'
               INTO lv_msg2
               SEPARATED BY space.

        io_fluxo->add_text_log(
        EXPORTING
          iv_text  = lv_msg2   " Mensagem
          iv_msgty = 'E'       " Tipo de mensagem
        ).
        CONTINUE.
      ENDIF.
* <<< END Inetum SAM EMP/SS HR 7000202896 24.06.2024

      DATA(lv_index) = sy-tabix.
      IF io_fluxo->gv_test EQ abap_true.
        DATA(lv_commit) = abap_true.
      ELSE.
        lv_commit = abap_false.
      ENDIF.

      READ TABLE gt_subtype INTO DATA(ls_subtype) WITH KEY subtype = ls_absences-absence_reason.
      IF sy-subrc = 0.
        DATA(lv_struct) = 'P' && ls_subtype-infotype.
        CREATE DATA lv_data TYPE (lv_struct).
        ASSIGN lv_data->* TO FIELD-SYMBOL(<fs_record>).

        io_fluxo->get_record(
          EXPORTING
            iv_api     = 'ABSENCES'
            is_data    = ls_absences
          IMPORTING
            es_record  = <fs_record>
        ).

        IF lv_index = 1.
          MOVE-CORRESPONDING <fs_record> TO ls_absences_o.

          IF ls_absences-excluded_record IS NOT INITIAL.
            ls_absences_o-operation = 'DEL'.
          ELSE.
            ls_absences_o-operation = 'INS'.
          ENDIF.

          APPEND ls_absences-id TO ls_absences_o-id.
          APPEND ls_absences_o TO lt_absences_o.
        ELSE.

*>> APP - 13-07-2023 - Ausências agrupadas com dias inteiros/parciais
*            "Se tiver BEGIN_TIME preenchido - Não é dia inteiro
*            IF ls_absences-absence_begin_time IS NOT INITIAL.
*              MOVE-CORRESPONDING <fs_record> TO ls_absences_o.
*
*              IF ls_absences-excluded_record IS NOT INITIAL.
*                ls_absences_o-operation = 'DEL'.
*              ELSE.
*                ls_absences_o-operation = 'INS'.
*              ENDIF.
*
*              APPEND ls_absences-id TO ls_absences_o-id.
*              APPEND ls_absences_o TO lt_absences_o.
*              "Se BEGIN_TIME a vazio - É dia inteiro
*            ELSE.



          READ TABLE ls_json-data-absence_result INTO DATA(ls_testes_aux) INDEX lv_index - 1.

          ASSIGN COMPONENT 'PERNR' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_pernr>).
          ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_subty>).
          ASSIGN COMPONENT 'AWART' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_awart>).
          ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_begda>).
          ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_endda>).
          ASSIGN COMPONENT 'AEDTM' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_aedtm>).
          ASSIGN COMPONENT 'STDAZ' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_stdaz>).

*              IF ls_testes_aux-employee_id = <fs_pernr> AND ls_testes_aux-absence_reason = <fs_subty>.

          "Se PERNR e SUBTY forem iguais ao anterior + BEGIN_TIME vazio -> É dia inteiro
*            IF ls_testes_aux-employee_id = <fs_pernr> AND ls_testes_aux-absence_reason = <fs_subty> AND ls_testes_aux-absence_begin_time IS INITIAL.
          "Se PERNR e SUBTY forem iguais ao anterior + OPERATION_VALUE vazio/ 8H -> É dia inteiro
*            IF ls_testes_aux-employee_id = <fs_pernr> AND ls_testes_aux-absence_reason = <fs_subty> AND ( <fs_record> IS INITIAL OR ls_testes_aux-operation_value = '8' ).
          IF ls_testes_aux-employee_id = <fs_pernr> AND ls_testes_aux-absence_reason = <fs_subty> AND ( <fs_stdaz> IS INITIAL OR <fs_stdaz> = '8' )
*<< APP - 13-07-2023 - Ausências agrupadas com dias inteiros/parciais
* >>> INI Inetum SAM EMP/SS HR 7000206480 07.08.2024
* Só agrupa se o registo anterior for igual. Quando era uma exclusão, estava a agrupar erradamente
         AND ls_testes_aux-excluded_record = ls_absences-excluded_record.
* <<< END Inetum SAM EMP/SS HR 7000206480 07.08.2024
            CONCATENATE <fs_endda>(4) <fs_endda>+4(2) <fs_endda>+6(2)
              INTO lv_date.

            CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
              EXPORTING
                date      = lv_date
                days      = 1
                signum    = '-'
                months    = 0
                years     = 0
              IMPORTING
                calc_date = lv_prev_day.

            READ TABLE lt_absences_o ASSIGNING FIELD-SYMBOL(<fs_absence_o>) WITH KEY pernr = <fs_pernr>
                                                                                     subty = <fs_subty>
                                                                                     endda = lv_prev_day.
            IF sy-subrc EQ 0.

              "Validar se a data anterior é o ultimo dia do mês
              IF lv_date+6(2) EQ '01'.
                CLEAR ls_absences_o-id.
                "append da linha à tabela - Criar novo bloco!
                MOVE-CORRESPONDING <fs_record> TO ls_absences_o.
                MOVE lv_date TO ls_absences_o-endda.

                IF ls_absences-excluded_record IS NOT INITIAL.
                  ls_absences_o-operation = 'DEL'.
                ELSE.
                  ls_absences_o-operation = 'INS'.
                ENDIF.

                APPEND ls_absences-id TO ls_absences_o-id.
                APPEND ls_absences_o TO lt_absences_o.
              ELSE.
                "Modify da linha com a data nova de fim por INDEX / Assigning Field-Symbols
                <fs_absence_o>-endda = lv_date.

                APPEND ls_absences-id TO <fs_absence_o>-id.
              ENDIF.
            ELSE.
              CLEAR ls_absences_o-id.
              "append da linha à tabela - Criar novo bloco!
              MOVE-CORRESPONDING <fs_record> TO ls_absences_o.

              IF ls_absences-excluded_record IS NOT INITIAL.
                ls_absences_o-operation = 'DEL'.
              ELSE.
                ls_absences_o-operation = 'INS'.
              ENDIF.

              APPEND ls_absences-id TO ls_absences_o-id.
              APPEND ls_absences_o TO lt_absences_o.
            ENDIF.
          ELSE.
            CLEAR ls_absences_o-id.
            "Adiciono linha à tabela por blocos
            MOVE-CORRESPONDING <fs_record> TO ls_absences_o.

            IF ls_absences-excluded_record IS NOT INITIAL.
              ls_absences_o-operation = 'DEL'.
            ELSE.
              ls_absences_o-operation = 'INS'.
            ENDIF.

            APPEND ls_absences-id TO ls_absences_o-id.
            APPEND ls_absences_o TO lt_absences_o.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

* >>> INI Inetum SAM EMP/SS HR 7000206480 07.08.2024
* Comentado, para que fique tudo ordenado pela ordenação original
*    SORT lt_absences_o BY pernr
*                          subty
*                          begda.
**                            operation/excluded_record.  "1º os INS e depois os DEL
* <<< END Inetum SAM EMP/SS HR 7000206480 07.08.2024

*Aqui vamos ter de validar em cada insert, se existe algum intervalo em que se possível integrar os dados

    LOOP AT lt_absences_o INTO ls_absences_o.
      DATA(lv_lines) = lines( ls_absences_o-id ).
      DATA(lv_counter) = 1.
      LOOP AT ls_absences_o-id INTO DATA(ls_absences_id).
        IF ls_absences_o-subty NE '1001'.

          SHIFT ls_absences_id LEFT DELETING LEADING '0'.
          READ TABLE ls_json-data-absence_result INTO ls_absences WITH KEY id = ls_absences_id.
          IF sy-subrc EQ 0.
            lv_struct = 'P' && ls_subtype-infotype.
            CREATE DATA lv_data TYPE (lv_struct).
            ASSIGN lv_data->* TO <fs_record>.

            io_fluxo->get_record(
              EXPORTING
                iv_api     = 'ABSENCES'
                is_data    = ls_absences
              IMPORTING
                es_record  = <fs_record>
            ).

            ASSIGN COMPONENT 'PERNR' OF STRUCTURE <fs_record> TO <fs_pernr>.
            ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <fs_record> TO <fs_subty>.
            ASSIGN COMPONENT 'AWART' OF STRUCTURE <fs_record> TO <fs_awart>.
            ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <fs_record> TO <fs_begda>.
            ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <fs_record> TO <fs_endda>.
            ASSIGN COMPONENT 'AEDTM' OF STRUCTURE <fs_record> TO <fs_aedtm>.

            IF ls_return-type IS INITIAL.
              ls_return-type = 'S'.
            ENDIF.

          ENDIF.

          READ TABLE gt_subtype INTO ls_subtype WITH KEY subtype = ls_absences_o-subty.
          IF lv_counter = lv_lines.
            IF sy-subrc = 0 .

              CONCATENATE ls_absences_o-begda(4) '-' ls_absences_o-begda+4(2)'-' ls_absences_o-begda+6(2)
              INTO ls_absences-absence_begin_date.

              io_fluxo->get_record(
                EXPORTING
                  iv_api     = 'ABSENCES'
                  is_data    = ls_absences
                IMPORTING
                  es_record  = <fs_record>
              ).

              ASSIGN COMPONENT 'PERNR' OF STRUCTURE <fs_record> TO <fs_pernr>.
              ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <fs_record> TO <fs_subty>.
              ASSIGN COMPONENT 'AWART' OF STRUCTURE <fs_record> TO <fs_awart>.
              ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <fs_record> TO <fs_begda>.
              ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <fs_record> TO <fs_endda>.
              ASSIGN COMPONENT 'AEDTM' OF STRUCTURE <fs_record> TO <fs_aedtm>.

              IF ls_absences-excluded_record IS NOT INITIAL.
                lv_operation = 'DEL'.

                zcl_hcm_ws_wfm=>infotype_operation_delete(
                  EXPORTING
                    iv_infotype        = ls_subtype-infotype                 " Infotipo
                    iv_employee_number = <fs_pernr>                 " Nº pessoal
                    iv_subty           = <fs_subty>                 " Subinfotipo
                    iv_begda           = ls_absences_o-begda                 " Início da validade
                    iv_endda           = ls_absences_o-endda                 " Fim da validade
                    iv_operation       = lv_operation                " Operação em infotipos
                    iv_commit          = lv_commit
                  IMPORTING
                    es_return          = ls_return                 " Parâmetro de retorno
                    es_key             = ls_key                 " Chave para dados mestre HR
                  CHANGING
                     cs_record = <fs_record>
                ).

              ELSE.
                lv_operation = 'INS'.

                zcl_hcm_ws_wfm=>infotype_operation_insert(
                  EXPORTING
                    iv_infotype        = ls_subtype-infotype                 " Infotipo
                    iv_employee_number = <fs_pernr>                 " Nº pessoal
                    iv_subty           = <fs_subty>                 " Subinfotipo
                    iv_begda           = ls_absences_o-begda                 " Início da validade
                    iv_endda           = ls_absences_o-endda                 " Fim da validade
                    iv_operation       = lv_operation                " Operação em infotipos
                    iv_commit          = lv_commit
                  IMPORTING
                    es_return          = ls_return                 " Parâmetro de retorno
                    es_key             = ls_key                 " Chave para dados mestre HR
                   CHANGING
                     cs_record = <fs_record>
                ).

              ENDIF.

              IF ls_return-type IS INITIAL OR ( ls_return-id EQ 'PG' AND ls_return-number EQ '080' ).
                ls_return-type = 'S'.
              ELSE.

                ls_absences_o-msgty = ls_return-type.
                ls_absences_o-msgid = ls_return-id.
                ls_absences_o-msgno = ls_return-number.
                MODIFY lt_absences_o FROM ls_absences_o.

              ENDIF.

              CONCATENATE
              ls_absences_o-operation
              'Pernr: '
              ls_absences_o-pernr
              'Infotype: '
              ls_subtype-infotype
              'Subty: '
              ls_absences_o-subty
              'Awart: '
              ls_absences_o-awart
              'Begda: '
              ls_absences_o-begda
              'Endda: '
              ls_absences_o-endda
              'Aedtm: '
              ls_absences_o-aedtm
              INTO DATA(lv_msg) SEPARATED BY space.

              io_fluxo->add_text_log(
              EXPORTING
                iv_text  = lv_msg           " Mensagem
                iv_msgty = ls_return-type   " Tipo de mensagem
              ).

            ELSE.
              IF ls_absences_o-subty IS NOT INITIAL.
                ls_return-id = gc_msgid.
                ls_return-number = '003'.
                ls_return-message_v1 = ls_absences_o-subty.
                ls_return-type = 'E'.
              ELSE.
                ls_return-id = gc_msgid.
                ls_return-number = '004'.
                ls_return-message_v1 = ls_absences_id.
                ls_return-type = 'S'.                                 "este tipo de rubricas faz parte de um grupo WFM "S" que não é tratado em SAP - S
              ENDIF.
            ENDIF.

            ls_msg-msgid = ls_return-id.
            ls_msg-msgty = ls_return-type.
            ls_msg-msgno = ls_return-number.
            ls_msg-msgv1 = ls_return-message_v1.
            ls_msg-msgv2 = ls_return-message_v2.
            ls_msg-msgv3 = ls_return-message_v3.
            ls_msg-msgv4 = ls_return-message_v4.

            io_fluxo->add_msg_log(
              EXPORTING
                us_msg    = ls_msg                 " Log de aplicação: dados de uma mensagem
              EXCEPTIONS
                error_log = 1                      " Erro em registo de Log
                OTHERS    = 2
            ).
          ENDIF.

          READ TABLE ls_json-data-absence_result INTO ls_absences WITH KEY id = ls_absences_id.
          IF sy-subrc EQ 0.
            "DADOS A INTEGRAR NA TABELA Z DE INTEGRAÇÃO
            APPEND INITIAL LINE TO lt_integ ASSIGNING FIELD-SYMBOL(<fs_integ>).
            <fs_integ>-mandt = sy-mandt.
            <fs_integ>-id = ls_absences_id.
            <fs_integ>-api = 'ABSENCES'.
            <fs_integ>-operation = ls_absences_o-operation.
            <fs_integ>-pernr = ls_absences_o-pernr.
            <fs_integ>-subty = ls_absences_o-subty.
            <fs_integ>-begda = ls_absences-absence_begin_date(4) && ls_absences-absence_begin_date+5(2) && ls_absences-absence_begin_date+8(2).
            <fs_integ>-endda = ls_absences-absence_end_date(4) && ls_absences-absence_end_date+5(2) && ls_absences-absence_end_date+8(2).

            CALL FUNCTION 'FORMAT_MESSAGE'
              EXPORTING
                id        = ls_return-id
                lang      = sy-langu
                no        = ls_return-number
                v1        = ls_return-message_v1
                v2        = ls_return-message_v2
                v3        = ls_return-message_v3
                v4        = ls_return-message_v4
              IMPORTING
                msg       = <fs_integ>-msg
              EXCEPTIONS
                not_found = 1
                OTHERS    = 2.

            "Se Sucesso + Teste a vazio -> Define se os dados são integrados na tabela ou não => Se enviamos resposta para WFM ou não
            IF ls_return-type EQ 'S' AND io_fluxo->gv_test IS INITIAL.
              <fs_integ>-integrated = abap_true.
            ELSEIF ls_return-type EQ 'E'.
              LOOP AT lt_integ ASSIGNING FIELD-SYMBOL(<fs_integracao>) WHERE pernr = ls_absences_o-pernr
                                                                         AND subty = ls_absences_o-subty
                                                                         AND begda BETWEEN ls_absences_o-begda AND ls_absences_o-endda.

                <fs_integracao>-integrated = abap_false.
              ENDLOOP.
              lv_error = 'E'.
            ENDIF.

            UNASSIGN <fs_integ>.
          ENDIF.

        ELSE.

          ls_return-type = 'S'.

          CONCATENATE
          'Ignorado'
          'Pernr: '
          ls_absences_o-pernr
          'Infotype: '
          ls_subtype-infotype
          'Subty: '
          ls_absences_o-subty
          'Awart: '
          ls_absences_o-awart
          'Begda: '
          ls_absences_o-begda
          'Endda: '
          ls_absences_o-endda
          'Aedtm: '
          ls_absences_o-aedtm
          INTO lv_msg SEPARATED BY space.

          io_fluxo->add_text_log(
          EXPORTING
            iv_text  = lv_msg           " Mensagem
            iv_msgty = ls_return-type   " Tipo de mensagem
          ).

        ENDIF.

        ADD 1 TO lv_counter.
        CLEAR ls_return.
      ENDLOOP.
    ENDLOOP.

    IF lv_error IS INITIAL AND sy-subrc = 0.
      lv_error = 'S'.
    ELSE.
      lv_error = 'E'.
    ENDIF.

    cv_error = lv_error.

    IF sy-subrc = 0.
      ASSIGN COMPONENT 'ABSENCE_RESULT' OF STRUCTURE cs_output TO FIELD-SYMBOL(<fs_absence_result>).
      <fs_absence_result> = ls_json-data-absence_result.


* >>> INI Inetum SAM EMP/SS HR 7000197093 04.06.2024
* Comentado porque se colocou no fim de tudo
      COMMIT WORK AND WAIT.
* <<< END Inetum SAM EMP/SS HR 7000197093 04.06.2024

      MODIFY ztwfm_hcm_integ FROM TABLE lt_integ .
      IF sy-subrc = 0.
* >>> INI Inetum SAM EMP/SS HR 7000206480 07.08.2024
        COMMIT WORK AND WAIT.
* <<< END Inetum SAM EMP/SS HR 7000206480 07.08.2024
        IF NOT line_exists( lt_integ[ integrated = abap_false ] ).
* >>> INI Inetum SAM EMP/SS HR 7000206480 07.08.2024
* Comentado porque não faz sentido os logs não ficarem com os registos de erro
*          COMMIT WORK AND WAIT.
* <<< END Inetum SAM EMP/SS HR 7000206480 07.08.2024
          "Envia novo resend com a confirmação, caso não exista nenhuma linha com integrated = abap_false
          cv_flag = abap_true.
          cv_confirm = ls_json-data-absence_resul_out_id.
          io_fluxo->delivery_confirmation(
            EXPORTING
              iv_conf_id = ls_json-data-absence_resul_out_id
              io_fluxo   = io_fluxo                 " Classe Interface wfm
          ).
* >>> INI Inetum SAM EMP/SS HR 7000206480 07.08.2024
* Comentado porque não faz sentido os logs não ficarem com os registos de erro
        ELSE.
          ROLLBACK WORK.
          lv_error_aux = 'E'.
* <<< END Inetum SAM EMP/SS HR 7000206480 07.08.2024
        ENDIF.
* >>> INI Inetum SAM EMP/SS HR 7000206480 07.08.2024
* Comentado porque não faz sentido os logs não ficarem com os registos de erro
      ELSE.
        ROLLBACK WORK.
        lv_error_aux = 'E'.
* <<< END Inetum SAM EMP/SS HR 7000206480 07.08.2024
      ENDIF.
    ENDIF.

**** >>> INI Inetum SAM EMP/SS HR 7000206480 07.08.2024
***    COMMIT WORK AND WAIT.
**** <<< END Inetum SAM EMP/SS HR 7000206480 07.08.2024

* >>> INI Inetum SAM EMP/SS HR 7000206480 07.08.2024
* Comentado porque se deu erro ali atrás, também não inseriu ou eliminou nada nos infotipos!!!
*    "Se der erro -> Delete na PAxxxx
    IF lv_error_aux = 'E'.
* >>> INI Inetum SAM EMP/SS HR 7000206480 07.08.2024
* Comentado, para que fique tudo ordenado pela ordenação original
**      "Ordenar lt_absences_o de forma a que primeiro tenha de reverter os deletes
*      SORT lt_absences_o BY pernr
*                            subty
*                            begda
*                            operation. "E que assim ordena alfabeticamente 1ºDEL -> INS
* <<< END Inetum SAM EMP/SS HR 7000206480 07.08.2024

      "Loop a todas as ausências que não são erro de 'Não é possível inserir devido a colisão'
      LOOP AT lt_absences_o INTO ls_absences_o WHERE msgid NE 'PG'
                                                 AND msgno NE 080.
        READ TABLE gt_subtype INTO ls_subtype WITH KEY subtype = ls_absences_o-subty.
        IF sy-subrc EQ 0.
          "NOS INSERTS BASTA CHAMAR O DELETE E REAGRUPA AUTOMATICAMENTE
          IF ls_absences_o-operation EQ 'INS'.

            lv_lines = lines( ls_absences_o-id ).
            lv_counter = 1.

            LOOP AT ls_absences_o-id INTO ls_absences_id.
              IF lv_counter = lv_lines.

                SHIFT ls_absences_id LEFT DELETING LEADING '0'.
                READ TABLE ls_json-data-absence_result INTO ls_absences WITH KEY id = ls_absences_id.
                IF sy-subrc EQ 0.

                  lv_struct = 'P' && ls_subtype-infotype.
                  CREATE DATA lv_data TYPE (lv_struct).
                  ASSIGN lv_data->* TO FIELD-SYMBOL(<fs_record_del>).

                  CONCATENATE ls_absences_o-begda(4) '-' ls_absences_o-begda+4(2)'-' ls_absences_o-begda+6(2)
                  INTO ls_absences-absence_begin_date.

                  "Para o endda não vale a pena porque o ultimo ID vai ser o do ENDDA

                  io_fluxo->get_record(
                    EXPORTING
                      iv_api     = 'ABSENCES'
                      is_data    = ls_absences
                    IMPORTING
                      es_record  = <fs_record_del>
                  ).

                  ASSIGN COMPONENT 'PERNR' OF STRUCTURE <fs_record_del> TO <fs_pernr>.
                  ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <fs_record_del> TO <fs_subty>.
                  ASSIGN COMPONENT 'AWART' OF STRUCTURE <fs_record_del> TO <fs_awart>.
                  ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <fs_record_del> TO <fs_begda>.
                  ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <fs_record_del> TO <fs_endda>.
                  ASSIGN COMPONENT 'AEDTM' OF STRUCTURE <fs_record_del> TO <fs_aedtm>.

                  zcl_hcm_ws_wfm=>infotype_operation_delete(
                    EXPORTING
                      iv_infotype        = ls_subtype-infotype                 " Infotipo
                      iv_employee_number = <fs_pernr>                 " Nº pessoal
                      iv_subty           = <fs_subty>                 " Subinfotipo
                      iv_begda           = ls_absences_o-begda                 " Início da validade
                      iv_endda           = ls_absences_o-endda                 " Fim da validade
                      iv_operation       = ls_absences_o-operation               " Operação em infotipos
                      iv_commit          = lv_commit
                    IMPORTING
                      es_return          = ls_return                 " Parâmetro de retorno
                      es_key             = ls_key                 " Chave para dados mestre HR
                    CHANGING
                       cs_record = <fs_record_del>
                  ).
                ENDIF.
              ENDIF.

              ADD 1 TO lv_counter.

            ENDLOOP.

            "NOS DELETES - FAZER O INSERT NOVAMENTE
          ELSE.
            "Temos de adicionar o <fs_record>
            lv_lines = lines( ls_absences_o-id ).
            lv_counter = 1.

            LOOP AT ls_absences_o-id INTO ls_absences_id.
              IF lv_counter = lv_lines.

                SHIFT ls_absences_id LEFT DELETING LEADING '0'.
                READ TABLE ls_json-data-absence_result INTO ls_absences WITH KEY id = ls_absences_id.
                IF sy-subrc EQ 0.

                  lv_struct = 'P' && ls_subtype-infotype.
                  CREATE DATA lv_data TYPE (lv_struct).
                  ASSIGN lv_data->* TO FIELD-SYMBOL(<fs_record_ins>).

                  CONCATENATE ls_absences_o-begda(4) '-' ls_absences_o-begda+4(2)'-' ls_absences_o-begda+6(2)
                  INTO ls_absences-absence_begin_date.
                  "Para o endda não vale a pena porque o ultimo ID vai ser o do ENDDA

                  io_fluxo->get_record(
                    EXPORTING
                      iv_api     = 'ABSENCES'
                      is_data    = ls_absences
                    IMPORTING
                      es_record  = <fs_record_ins>
                  ).

                  ASSIGN COMPONENT 'PERNR' OF STRUCTURE <fs_record_ins> TO <fs_pernr>.
                  ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <fs_record_ins> TO <fs_subty>.
                  ASSIGN COMPONENT 'AWART' OF STRUCTURE <fs_record_ins> TO <fs_awart>.
                  ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <fs_record_ins> TO <fs_begda>.
                  ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <fs_record_ins> TO <fs_endda>.
                  ASSIGN COMPONENT 'AEDTM' OF STRUCTURE <fs_record_ins> TO <fs_aedtm>.

                  "infotype_operation_insert -> inserir e reagrupar -> QUe era como estava antes do delete
                  zcl_hcm_ws_wfm=>infotype_operation_insert(
                    EXPORTING
                      iv_infotype        = ls_subtype-infotype                 " Infotipo
                      iv_employee_number = ls_absences_o-pernr                 " Nº pessoal
                      iv_subty           = ls_absences_o-subty                 " Subinfotipo
                      iv_begda           = ls_absences_o-begda                 " Início da validade
                      iv_endda           = ls_absences_o-endda                 " Fim da validade
                      iv_operation       = ls_absences_o-operation                " Operação em infotipos
                      iv_commit          = lv_commit
                    IMPORTING
                      es_return          = ls_return                 " Parâmetro de retorno
                      es_key             = ls_key                 " Chave para dados mestre HR
                     CHANGING
                       cs_record = <fs_record_ins>
                  ).

                ENDIF.
              ENDIF.

              ADD 1 TO lv_counter.

            ENDLOOP.

            "Não adiciona nada aos LOGs
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

* <<< END Inetum SAM EMP/SS HR 7000206480 07.08.2024

***********************************************************************************
* >>> INI Inetum SAM EMP/SS HR 7000202896 24.06.2024
* Comentado
*      "Se os colaboradores não existirem
*    ELSE.
*      lv_msg = 'Um ou mais colaboradores inexistentes em SAP.'.
*
*      io_fluxo->add_text_log(
*      EXPORTING
*        iv_text  = lv_msg           " Mensagem
*        iv_msgty = 'E'   " Tipo de mensagem
*      ).
*    ENDIF.
* <<< END Inetum SAM EMP/SS HR 7000202896 24.06.2024

  ENDMETHOD.


  METHOD get_colaboradores.

* >>> INI Inetum SAM EMP/SS HR 7000202896 24.06.2024
* Comentado
*    LOOP AT results INTO DATA(ls_results).
*
*      SELECT SINGLE * FROM pa0001
*        INTO @DATA(ls_pa0001)
*        WHERE pernr = @ls_results-employee_id.
*
*      IF sy-subrc NE 0.
*        value = abap_true.
*        EXIT.
*      ENDIF.
*    ENDLOOP.

    CLEAR: value.
    SELECT SINGLE * FROM pa0001
      INTO @DATA(ls_pa0001)
      WHERE pernr = @employee_id.
    IF sy-subrc NE 0.
      value = abap_true.
      EXIT.
    ENDIF.
* <<< END Inetum SAM EMP/SS HR 7000202896 24.06.2024

  ENDMETHOD.


  METHOD get_data_o.
    TYPES: BEGIN OF ty_s_data,
             employees_data       TYPE ztt_hcm_data_result,
             employee_data_out_id TYPE string,
           END OF ty_s_data.

    TYPES: BEGIN OF ty_s_schedule_o,
             token    TYPE string,
             sid      TYPE string,
             success  TYPE string,
             code     TYPE string,
             codetype TYPE string,
             message  TYPE string,
             data     TYPE ty_s_data,
           END OF ty_s_schedule_o.

    DATA: lt_integ     TYPE TABLE OF ztwfm_hcm_integ,

          lr_id        TYPE RANGE OF ztwfm_hcm_integ-id,

          ls_json      TYPE ty_s_schedule_o,
          ls_return    TYPE bapireturn1,
          ls_key       TYPE bapipakey,
          ls_msg       TYPE bal_s_msg,

          lv_subty     TYPE subty,
          lv_operation TYPE actio,
          lv_response  TYPE string,
          lv_error     TYPE zzca_statws,
          lv_data      TYPE REF TO data.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = iv_response
        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data             = ls_json
    ).

    LOOP AT ls_json-data-employees_data INTO DATA(ls_data_aux).
      APPEND INITIAL LINE TO lr_id ASSIGNING FIELD-SYMBOL(<fs_id>).
      <fs_id>-sign = 'I'.
      <fs_id>-option = 'EQ'.
      <fs_id>-low = ls_data_aux-id.
    ENDLOOP.

    SELECT id
      FROM ztwfm_hcm_integ
      INTO TABLE @DATA(lt_integrated)
      WHERE id IN @lr_id
        AND api = 'DATA'
        AND integrated = @abap_true.

    IF sy-subrc = 0.
      LOOP AT lt_integrated INTO DATA(ls_integrated).
        SHIFT ls_integrated-id LEFT DELETING LEADING '0'.
        DELETE ls_json-data-employees_data WHERE id = ls_integrated-id.
      ENDLOOP.
    ENDIF.

    LOOP AT ls_json-data-employees_data INTO DATA(ls_employees).
      IF io_fluxo->gv_test EQ abap_true.
        DATA(lv_commit) = abap_true.
      ELSE.
        lv_commit = abap_false.
      ENDIF.

      IF ls_employees-contingent_id IS NOT INITIAL.
        lv_subty = ls_employees-contingent_id.
      ELSEIF ls_employees-absence_reason IS NOT INITIAL.
        lv_subty = ls_employees-absence_reason.
      ELSE.
        lv_subty = ls_employees-wage_type.
      ENDIF.

      IF lv_subty = '3070'.
        SUBMIT zhcm_wfm_wages AND RETURN.
        CONTINUE.
      ENDIF.

      READ TABLE gt_subtype INTO DATA(ls_subtype) WITH KEY subtype = lv_subty.
      IF sy-subrc = 0.
        DATA(lv_struct) = 'P' && ls_subtype-infotype.
        CREATE DATA lv_data TYPE (lv_struct).
        ASSIGN lv_data->* TO FIELD-SYMBOL(<fs_record>).

        io_fluxo->get_record(
          EXPORTING
            iv_api     = 'DATA'
            is_data    = ls_employees
          IMPORTING
            es_record  = <fs_record>
        ).

        ASSIGN COMPONENT 'PERNR' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_pernr>).
        ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_subty>).
        ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_begda>).
        ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_endda>).
        ASSIGN COMPONENT 'AEDTM' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_aedtm>).

        IF ls_employees-exclude_record IS NOT INITIAL.
          lv_operation = 'DEL'.
        ELSE.
          lv_operation = 'INS'.
        ENDIF.

        zcl_hcm_ws_wfm=>infotype_operation(
          EXPORTING
            iv_infotype        = ls_subtype-infotype                 " Infotipo
            iv_employee_number = <fs_pernr>                 " Nº pessoal
            iv_subty           = <fs_subty>                 " Subinfotipo
            iv_begda           = <fs_begda>                 " Início da validade
            iv_endda           = <fs_endda>                 " Fim da validade
            iv_operation       = lv_operation                 " Operação em infotipos
            iv_commit          = lv_commit
            is_record          = <fs_record>
          IMPORTING
            es_return          = ls_return                 " Parâmetro de retorno
            es_key             = ls_key                 " Chave para dados mestre HR
        ).

        CONCATENATE
        lv_operation
        'Pernr: '
        <fs_pernr>
        'Infotype: '
        ls_subtype-infotype
        'Subty: '
        <fs_subty>
        'Begda: '
        <fs_begda>
        'Endda: '
        <fs_endda>
        'Aedtm: '
        <fs_aedtm>
        INTO DATA(lv_msg) SEPARATED BY space.

        IF ls_return-type IS INITIAL.
          ls_return-type = 'S'.
        ENDIF.

        io_fluxo->add_text_log(
          EXPORTING
            iv_text  = lv_msg           " Mensagem
            iv_msgty = ls_return-type   " Tipo de mensagem
        ).
      ELSE.
        IF lv_subty IS NOT INITIAL.
          ls_return-id = gc_msgid.
          ls_return-number = '003'.
          ls_return-message_v1 = lv_subty.
        ELSE.
          ls_return-id = gc_msgid.
          ls_return-number = '004'.
          ls_return-message_v1 = ls_employees-id.
        ENDIF.

        ls_return-type = 'E'.
      ENDIF.

      ls_msg-msgid = ls_return-id.
      ls_msg-msgty = ls_return-type.
      ls_msg-msgno = ls_return-number.
      ls_msg-msgv1 = ls_return-message_v1.
      ls_msg-msgv2 = ls_return-message_v2.
      ls_msg-msgv3 = ls_return-message_v3.
      ls_msg-msgv4 = ls_return-message_v4.

      io_fluxo->add_msg_log(
        EXPORTING
          us_msg    = ls_msg                 " Log de aplicação: dados de uma mensagem
        EXCEPTIONS
          error_log = 1                " Erro em registo de Log
          OTHERS    = 2
      ).

      APPEND INITIAL LINE TO lt_integ ASSIGNING FIELD-SYMBOL(<fs_integ>).
      <fs_integ>-mandt = sy-mandt.
      <fs_integ>-id = ls_employees-id.
      <fs_integ>-api = 'DATA'.
      <fs_integ>-operation = lv_operation.
      <fs_integ>-pernr = ls_employees-employee_id.
      <fs_integ>-subty = lv_subty.
      <fs_integ>-begda = ls_employees-record_begin_date(4) && ls_employees-record_begin_date+5(2) && ls_employees-record_begin_date+8(2).
      <fs_integ>-endda = ls_employees-record_end_date(4) && ls_employees-record_end_date+5(2) && ls_employees-record_end_date+8(2).

      CLEAR: lv_operation, lv_subty.

      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = ls_return-id
          lang      = sy-langu
          no        = ls_return-number
          v1        = ls_return-message_v1
          v2        = ls_return-message_v2
          v3        = ls_return-message_v3
          v4        = ls_return-message_v4
        IMPORTING
          msg       = <fs_integ>-msg
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.

      IF ls_return-type EQ 'S' AND io_fluxo->gv_test IS INITIAL.
        <fs_integ>-integrated = abap_true.
      ELSEIF ls_return-type EQ 'E'.
        <fs_integ>-integrated = abap_false.
        lv_error = 'E'.
      ENDIF.

      UNASSIGN <fs_integ>.
    ENDLOOP.

    IF lv_error IS INITIAL AND sy-subrc = 0.
      lv_error = 'S'.
    ELSE.
      lv_error = 'E'.
    ENDIF.

    cv_error = lv_error.

    IF sy-subrc = 0.
      ASSIGN COMPONENT 'DATA_RESULT' OF STRUCTURE cs_output TO FIELD-SYMBOL(<fs_data_result>).
      <fs_data_result> = ls_json-data-employees_data.

      MODIFY ztwfm_hcm_integ FROM TABLE lt_integ.
      IF sy-subrc = 0.
        IF NOT line_exists( lt_integ[ integrated = abap_false ] ).

          COMMIT WORK AND WAIT.
          cv_flag = abap_true.
          cv_confirm = ls_json-data-employee_data_out_id.
          io_fluxo->delivery_confirmation(
            EXPORTING
              iv_conf_id = ls_json-data-employee_data_out_id
              io_fluxo   = io_fluxo                 " Classe Interface wfm
          ).
        ENDIF.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_domain.
    DATA(lv_name) = 'WFM-' && sy-sysid.

    SELECT SINGLE low
      FROM tvarvc
      INTO gv_domain
      WHERE name = lv_name
        AND type = 'P'
        AND numb = '0000'.
  ENDMETHOD.


  METHOD get_info_in.
    ASSIGN gs_info_in->* TO FIELD-SYMBOL(<fs_info_in>).
    c_info = <fs_info_in>.
  ENDMETHOD.


  METHOD get_info_out.
    ASSIGN gs_info_out->* to FIELD-SYMBOL(<fs_info_out>).
    c_info = <fs_info_out>.
  ENDMETHOD.


  METHOD get_record.
    DATA: ls_absence   TYPE zstruct_hcm_absence_result,
          ls_schedule  TYPE zstruct_hcm_schedule_result,
          ls_wages     TYPE zstruct_hcm_wages_result,
          ls_employees TYPE zstruct_hcm_data_result.

    ASSIGN COMPONENT 'PERNR' OF STRUCTURE es_record TO FIELD-SYMBOL(<fs_pernr>).
    ASSIGN COMPONENT 'INFTY' OF STRUCTURE es_record TO FIELD-SYMBOL(<fs_infty>).
    ASSIGN COMPONENT 'SUBTY' OF STRUCTURE es_record TO FIELD-SYMBOL(<fs_subty>).
    ASSIGN COMPONENT 'AWART' OF STRUCTURE es_record TO FIELD-SYMBOL(<fs_awart>).
    ASSIGN COMPONENT 'BEGDA' OF STRUCTURE es_record TO FIELD-SYMBOL(<fs_begda>).
    ASSIGN COMPONENT 'ENDDA' OF STRUCTURE es_record TO FIELD-SYMBOL(<fs_endda>).
    ASSIGN COMPONENT 'AEDTM' OF STRUCTURE es_record TO FIELD-SYMBOL(<fs_aedtm>).
    ASSIGN COMPONENT 'KTART' OF STRUCTURE es_record TO FIELD-SYMBOL(<fs_ktart>).
    ASSIGN COMPONENT 'LGART' OF STRUCTURE es_record TO FIELD-SYMBOL(<fs_lgart>).
    ASSIGN COMPONENT 'ANZHL' OF STRUCTURE es_record TO FIELD-SYMBOL(<fs_anzhl>).
    ASSIGN COMPONENT 'ZEINH' OF STRUCTURE es_record TO FIELD-SYMBOL(<fs_zeinh>).
    ASSIGN COMPONENT 'ACCNU' OF STRUCTURE es_record TO FIELD-SYMBOL(<fs_accnu>).
    ASSIGN COMPONENT 'ACCOP' OF STRUCTURE es_record TO FIELD-SYMBOL(<fs_accop>).
    ASSIGN COMPONENT 'BEGUZ' OF STRUCTURE es_record TO FIELD-SYMBOL(<fs_beguz>).
    ASSIGN COMPONENT 'ENDUZ' OF STRUCTURE es_record TO FIELD-SYMBOL(<fs_enduz>).
    ASSIGN COMPONENT 'STDAZ' OF STRUCTURE es_record TO FIELD-SYMBOL(<fs_stdaz>).

    CASE iv_api.
      WHEN 'ABSENCES'.
        ls_absence = is_data.
        <fs_pernr> = ls_absence-employee_id.
        <fs_awart> = <fs_subty> = ls_absence-absence_reason.
        <fs_begda> = ls_absence-absence_begin_date(4) && ls_absence-absence_begin_date+5(2) && ls_absence-absence_begin_date+8(2).
        <fs_endda> = ls_absence-absence_end_date(4) && ls_absence-absence_end_date+5(2) && ls_absence-absence_end_date+8(2).
        <fs_aedtm> = ls_absence-record_date(4) && ls_absence-record_date+5(2) && ls_absence-record_date+8(2).
        <fs_stdaz> = ls_absence-operation_value.

      WHEN 'SCHEDULE'.
        ls_schedule = is_data.
        DATA(lv_daily_work_load) = ls_schedule-daily_work_load(2) && '.' && ls_schedule-daily_work_load+2(2).
        SHIFT lv_daily_work_load LEFT DELETING LEADING '0'.

        <fs_pernr> = ls_schedule-employee_id.
        <fs_begda> = ls_schedule-schedule_begin_date(4) && ls_schedule-schedule_begin_date+5(2) && ls_schedule-schedule_begin_date+8(2).
        <fs_endda> = ls_schedule-schedule_end_date(4) && ls_schedule-schedule_end_date+5(2) && ls_schedule-schedule_end_date+8(2).
        <fs_aedtm> = ls_schedule-record_date(4) && ls_schedule-record_date+5(2) && ls_schedule-record_date+8(2).

        IF ls_schedule-record_begin_time IS NOT INITIAL.
          <fs_beguz> = ls_schedule-record_begin_time(2) && ls_schedule-record_begin_time+3(2) && ls_schedule-record_begin_time+6(2).
          <fs_enduz> = ls_schedule-record_end_time(2) && ls_schedule-record_end_time+3(2) && ls_schedule-record_end_time+6(2).
        ENDIF.

      WHEN 'WAGES'.

        ls_wages = is_data.
        <fs_pernr> = ls_wages-employee_id.
        <fs_begda> = ls_wages-record_begin_date(4) && ls_wages-record_begin_date+5(2) && ls_wages-record_begin_date+8(2).
        <fs_endda> = ls_wages-record_end_date(4) && ls_wages-record_end_date+5(2) && ls_wages-record_end_date+8(2).
        <fs_aedtm> = ls_wages-record_date(4) && ls_wages-record_date+5(2) && ls_wages-record_date+8(2).

        IF ls_wages-contingent_id IS NOT INITIAL.
          <fs_ktart> = <fs_subty> = ls_wages-contingent_id.
          <fs_anzhl> = ls_wages-operation_value.
        ELSE.
          <fs_lgart> = <fs_subty> = ls_wages-wage_type.

          IF <fs_subty> = '1004' OR <fs_subty> = '1006'.
            <fs_anzhl> = CONV i( ls_wages-operation_value ) / 8.
            <fs_zeinh> = '010'. "Unidades em dias
          ELSEIF <fs_subty> = '3044'.
            <fs_stdaz> = ls_wages-operation_value.
*            <fs_zeinh> = '001'. "Unidades em horas
          ELSE.
            <fs_anzhl> = ls_wages-operation_value.
            <fs_zeinh> = '001'. "Unidades em horas
          ENDIF.
        ENDIF.

      WHEN 'DATA'.
        ls_employees = is_data.
        <fs_pernr> = ls_employees-employee_id.
        <fs_begda> = ls_employees-record_begin_date(4) && ls_employees-record_begin_date+5(2) && ls_employees-record_begin_date+8(2).
        <fs_endda> = ls_employees-record_end_date(4) && ls_employees-record_end_date+5(2) && ls_employees-record_end_date+8(2).
        <fs_aedtm> = ls_employees-record_date(4) && ls_employees-record_date+5(2) && ls_employees-record_date+8(2).

        IF <fs_accnu> IS ASSIGNED AND <fs_accop> IS ASSIGNED.
          <fs_accnu> = ls_employees-operation_value.
          <fs_accop> = ls_employees-operation_type.
        ENDIF.

        IF ls_employees-contingent_id IS NOT INITIAL.
          <fs_ktart> = <fs_subty> = ls_employees-contingent_id.
          <fs_anzhl> = ls_employees-operation_value.
        ELSE.
          IF ls_employees-absence_reason IS NOT INITIAL.
            <fs_awart> = <fs_subty> = ls_employees-absence_reason.
          ELSE.
            <fs_lgart> = <fs_subty> = ls_employees-wage_type.

            IF <fs_subty> = '1004' OR  <fs_subty> = '1006'.
              <fs_anzhl> = CONV i( ls_employees-operation_value ) / 8.
              <fs_zeinh> = '010'. "Unidades em dias
            ELSE.
              <fs_anzhl> = ls_employees-operation_value.
              <fs_zeinh> = '001'. "Unidades em horas
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD get_schedule_o.
*    TYPES: BEGIN OF ty_s_data,
*             schedule_result        TYPE ztt_hcm_schedule_result,
*             schedule_result_out_id TYPE string,
*           END OF ty_s_data.
*
*    TYPES: BEGIN OF ty_s_schedule_o,
*             token    TYPE string,
*             sid      TYPE string,
*             success  TYPE string,
*             code     TYPE string,
*             codetype TYPE string,
*             message  TYPE string,
*             data     TYPE ty_s_data,
*           END OF ty_s_schedule_o.
*
*    DATA: lt_integ       TYPE TABLE OF ztwfm_hcm_integ,
*          lt_sched       TYPE TABLE OF ztwfm_hcm_sched,
*
*          ls_json        TYPE ty_s_schedule_o,
*          ls_pa0015      TYPE p0015,
*          ls_return      TYPE bapireturn1,
*          ls_key         TYPE bapipakey,
*          ls_msg         TYPE bal_s_msg,
*
*          lv_operation   TYPE actio,
*          lv_response    TYPE string,
*          lv_error       TYPE zzca_statws,
*          lv_data        TYPE REF TO data,
*          lv_last_day    TYPE datum,
*          lv_is_last_day TYPE boolean VALUE abap_false.
*
*    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
*      EXPORTING
*        day_in            = sy-datum
*      IMPORTING
*        last_day_of_month = lv_last_day
*      EXCEPTIONS
*        day_in_no_date    = 1
*        OTHERS            = 2.
*
*    IF sy-datum EQ lv_last_day.
*      lv_is_last_day = abap_true.
*    ENDIF.
*
*    /ui2/cl_json=>deserialize(
*      EXPORTING
*        json             = iv_response
*        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
*      CHANGING
*        data             = ls_json
*    ).
*
*    LOOP AT ls_json-data-schedule_result INTO DATA(ls_schedule).
*      DATA(lv_commit) = abap_false.
*
*      READ TABLE gt_subtype INTO DATA(ls_subtype) WITH KEY subtype = '01'.
*      IF sy-subrc = 0.
*        DO 2 TIMES.
*          IF lv_error = 'S'.
*            IF io_fluxo->gv_test EQ abap_true.
*              lv_commit = abap_true.
*            ELSE.
*              lv_commit = abap_false.
*            ENDIF.
*          ENDIF.
*
*          DATA(lv_struct) = gc_struct && ls_subtype-infotype.
*          ASSIGN (lv_struct) TO FIELD-SYMBOL(<fs_record>).
*
*          io_fluxo->get_record(
*            EXPORTING
*              iv_api     = 'SCHEDULE'
*              is_data    = ls_schedule
*            IMPORTING
*              es_record  = <fs_record>
*          ).
*
*          ASSIGN COMPONENT 'PERNR' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_pernr>).
*          ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_subty>).
*          ASSIGN COMPONENT 'AWART' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_awart>).
*          ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_begda>).
*          ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_endda>).
*          ASSIGN COMPONENT 'AEDTM' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_aedtm>).
*
*          IF ls_schedule-exclude_record IS NOT INITIAL.
*            lv_operation = 'DEL'.
*          ELSE.
*            lv_operation = 'INS'.
*          ENDIF.
*
*          CLEAR lv_error.
*
*          zcl_hcm_ws_wfm=>infotype_operation(
*            EXPORTING
*              iv_infotype        = ls_subtype-infotype                 " Infotipo
*              iv_employee_number = <fs_pernr>                 " Nº pessoal
*              iv_subty           = <fs_subty>                 " Subinfotipo
*              iv_begda           = <fs_begda>                 " Início da validade
*              iv_endda           = <fs_endda>                 " Fim da validade
*              iv_operation       = lv_operation                 " Operação em infotipos
*              iv_commit          = lv_commit
*              is_record          = <fs_record>
*            IMPORTING
*              es_return          = ls_return                 " Parâmetro de retorno
*              es_key             = ls_key                 " Chave para dados mestre HR
*          ).
*
*          IF ls_key IS NOT INITIAL AND lv_is_last_day EQ abap_true.
**            MOVE-CORRESPONDING <fs_record> TO ls_pa0015.
**            CREATE DATA lv_data LIKE ls_pa0015.
**            ASSIGN lv_data->* TO <fs_record>.
**            ls_pa0015-lgart = ls_pa0015-subty = '9020'.
**            ls_pa0015-anzhl = 22.
**            ls_pa0015-zeinh = '010'.
**            <fs_record> = ls_pa0015.
**
**            IF lv_operation = 'INS'.
**              zcl_hcm_ws_wfm=>infotype_operation(
**                EXPORTING
**                  iv_infotype        = '0015'                 " Infotipo
**                  iv_employee_number = <fs_pernr>                 " Nº pessoal
**                  iv_subty           = ls_pa0015-subty                 " Subinfotipo
**                  iv_begda           = <fs_begda>                 " Início da validade
**                  iv_endda           = <fs_endda>                 " Fim da validade
**                  iv_operation       = lv_operation                 " Operação em infotipos
**                  iv_commit          = lv_commit
**                  is_record          = <fs_record>
**                IMPORTING
**                  es_return          = ls_return                 " Parâmetro de retorno
**                  es_key             = ls_key                 " Chave para dados mestre HR
**              ).
**            ENDIF.
**
**            IF ls_key IS NOT INITIAL.
*            DATA(lv_first_day) = lv_last_day(4) && lv_last_day+4(2) && '01'.
*
*            SELECT SINGLE anzhl
*              FROM pa0015
*              INTO @DATA(lv_anzhl)
*              WHERE pernr = @<fs_pernr>
*                AND begda >= @lv_first_day
*                AND begda <= @lv_last_day
*                AND endda >= @lv_first_day
*                AND endda <= @lv_last_day.
*
**            SELECT SINGLE SUM( arbst )
**              FROM ztwfm_hcm_sched
**              INTO ls_pa0015-anzhl
**              WHERE pernr = <fs_pernr>
**                AND begda >= lv_first_day
**                AND begda <= lv_last_day
**                AND endda >= lv_first_day
**                AND endda <= lv_last_day.
*
*            ls_pa0015-anzhl = lv_anzhl - ( ls_pa0015-anzhl / 8 ).
*            ls_pa0015-lgart = ls_pa0015-subty = '9053'.
*            ls_pa0015-zeinh = '010'.
*            <fs_record> = ls_pa0015.
*
*            zcl_hcm_ws_wfm=>infotype_operation(
*              EXPORTING
*                iv_infotype        = '0015'                 " Infotipo
*                iv_employee_number = <fs_pernr>                 " Nº pessoal
*                iv_subty           = ls_pa0015-subty                 " Subinfotipo
*                iv_begda           = <fs_begda>                 " Início da validade
*                iv_endda           = <fs_endda>                 " Fim da validade
*                iv_operation       = lv_operation                 " Operação em infotipos
*                iv_commit          = lv_commit
*                is_record          = <fs_record>
*              IMPORTING
*                es_return          = ls_return                 " Parâmetro de retorno
*                es_key             = ls_key                 " Chave para dados mestre HR
*            ).
*
*            IF ls_key IS INITIAL.
*              lv_error = 'E'.
*            ENDIF.
*          ELSEIF ls_key IS INITIAL.
*            lv_error = 'E'.
*          ENDIF.
*
*          IF lv_error EQ 'E' OR io_fluxo->gv_test EQ abap_true.
*            EXIT.
*          ELSE.
*            lv_error = 'S'.
*          ENDIF.
*        ENDDO.
*
*        CONCATENATE
*        lv_operation
*        'Pernr: '
*        <fs_pernr>
*        'Infotype: '
*        ls_subtype-infotype
*        'Begda: '
*        <fs_begda>
*        'Endda: '
*        <fs_endda>
*        'Aedtm: '
*        <fs_aedtm>
*        INTO DATA(lv_msg) SEPARATED BY space.
*
*        IF ls_return-type IS INITIAL.
*          ls_return-type = 'S'.
*        ENDIF.
*
*        io_fluxo->add_text_log(
*          EXPORTING
*            iv_text  = lv_msg           " Mensagem
*            iv_msgty = ls_return-type   " Tipo de mensagem
*        ).
*      ELSE.
*        ls_return-id = gc_msgid.
*        ls_return-type = 'E'.
*        ls_return-number = '004'.
*        ls_return-message_v1 = ls_schedule-id.
*      ENDIF.
*
*      ls_msg-msgid = ls_return-id.
*      ls_msg-msgty = ls_return-type.
*      ls_msg-msgno = ls_return-number.
*      ls_msg-msgv1 = ls_return-message_v1.
*      ls_msg-msgv2 = ls_return-message_v2.
*      ls_msg-msgv3 = ls_return-message_v3.
*      ls_msg-msgv4 = ls_return-message_v4.
*
*      io_fluxo->add_msg_log(
*        EXPORTING
*          us_msg    = ls_msg                 " Log de aplicação: dados de uma mensagem
*        EXCEPTIONS
*          error_log = 1                " Erro em registo de Log
*          OTHERS    = 2
*      ).
*
*      APPEND INITIAL LINE TO lt_integ ASSIGNING FIELD-SYMBOL(<fs_integ>).
*      <fs_integ>-mandt = sy-mandt.
*      <fs_integ>-id = ls_schedule-id.
*      <fs_integ>-api = 'SCHEDULE'.
*
*      IF ls_return-type EQ 'S' AND io_fluxo->gv_test IS INITIAL.
*        <fs_integ>-integrated = abap_true.
*      ELSEIF ls_return-type EQ 'E'.
*        <fs_integ>-integrated = abap_false.
*        lv_error = 'E'.
*      ENDIF.
*
*      IF ls_return-type EQ 'S' AND io_fluxo->gv_test IS INITIAL.
*        APPEND INITIAL LINE TO lt_sched ASSIGNING FIELD-SYMBOL(<fs_sched>).
*        <fs_sched>-mandt = sy-mandt.
*        <fs_sched>-pernr = <fs_pernr>.
*        <fs_sched>-begda = <fs_begda>.
*        <fs_sched>-endda = <fs_endda>.
*      ENDIF.
*    ENDLOOP.
*
*    IF lv_error IS INITIAL AND sy-subrc = 0.
*      lv_error = 'S'.
*    ELSE.
*      lv_error = 'E'.
*    ENDIF.
*
*    cv_error = lv_error.
*
*    IF sy-subrc = 0.
*      ASSIGN COMPONENT 'SCHEDULE_RESULT' OF STRUCTURE cs_output TO FIELD-SYMBOL(<fs_schedule_result>).
*      <fs_schedule_result> = ls_json-data-schedule_result.
*
*      IF line_exists( lt_integ[ integrated = abap_false ] ).
*        io_fluxo->delivery_confirmation(
*          EXPORTING
*            iv_conf_id = ls_json-data-schedule_result_out_id
*            io_fluxo   = io_fluxo                 " Classe Interface wfm
*        ).
*      ENDIF.
*
*      MODIFY ztwfm_hcm_integ FROM TABLE lt_integ.
*      IF sy-subrc = 0.
*        COMMIT WORK AND WAIT.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*
*      MODIFY ztwfm_hcm_sched FROM TABLE lt_sched.
*      IF sy-subrc = 0.
*        COMMIT WORK AND WAIT.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD get_subtype.
    SELECT *
      FROM ztwfm_hcm_subtyp
      INTO TABLE gt_subtype.
  ENDMETHOD.


  METHOD get_token.
    ev_token = gv_token.
  ENDMETHOD.


METHOD get_wages_o.
  TYPES: BEGIN OF ty_s_data,
           wages_result        TYPE ztt_hcm_wages_result,
           wages_result_out_id TYPE string,
         END OF ty_s_data.

  TYPES: BEGIN OF ty_s_wage_o,
           token    TYPE string,
           sid      TYPE string,
           success  TYPE string,
           code     TYPE string,
           codetype TYPE string,
           message  TYPE string,
           data     TYPE ty_s_data,
         END OF ty_s_wage_o.

  TYPES: BEGIN OF ty_9020,
           pernr TYPE pernr,
           begda TYPE datum,
           endda TYPE datum,
         END OF ty_9020.

  DATA: lt_integ     TYPE TABLE OF ztwfm_hcm_integ,
        lt_sched_ins TYPE TABLE OF ztwfm_hcm_sched,
        lt_sched_del TYPE TABLE OF ztwfm_hcm_sched,

        lt_output_w  TYPE zhcm_s_o_wages,

        lt_9053      TYPE TABLE OF pa0015,
        ls_9053      TYPE pa0015,
        ls_p0015     TYPE pa0015,
        lt_9020      TYPE TABLE OF ty_9020,
        ls_9020      TYPE ty_9020,
        ls_15_9020   TYPE pa0015,
        lv_last_day  TYPE datum,
        lv_first_day TYPE datum,
        lv_day       TYPE datum,

* >>> INI Inetum SAM EMP/SS HR 7000189109 06.03.2024
        lt_9053_old  TYPE TABLE OF pa0015,
        ls_9053_old  TYPE pa0015,
* <<< END Inetum SAM EMP/SS HR 7000189109 06.03.2024

        lr_id        TYPE RANGE OF ztwfm_hcm_integ-id,

        ls_json      TYPE ty_s_wage_o,
        ls_pa0015    TYPE p0015,
        ls_pa2006    TYPE p2006,
        ls_return    TYPE bapireturn1,
        ls_key       TYPE bapipakey,
        ls_msg       TYPE bal_s_msg,

        lv_subty     TYPE subty,
        lv_operation TYPE actio,
        lv_response  TYPE string,
        lv_error     TYPE zzca_statws,
        lv_error_aux TYPE c,
        lv_data      TYPE REF TO data,
        lv_end_date  TYPE datum.

* >>> INI Inetum SAM EMP/SS HR 7000189109 15.12.2023
  TYPES: BEGIN OF ty_new_sched,
           pernr TYPE persno,
           anzhl TYPE anzhl,
         END OF ty_new_sched.
  DATA: lt_new_sched TYPE TABLE OF ty_new_sched,
        ls_new_sched LIKE LINE OF lt_new_sched,
        last_pernr   TYPE pernr_d,
        new_anzhl    TYPE anzhl.

  DATA: lt_sched TYPE TABLE OF ztwfm_hcm_sched,
        ls_sched LIKE LINE OF lt_sched.
* <<< END Inetum SAM EMP/SS HR 7000189109 15.12.2023

  /ui2/cl_json=>deserialize(
    EXPORTING
      json             = iv_response
      pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
    CHANGING
      data             = ls_json
  ).

  LOOP AT ls_json-data-wages_result INTO DATA(ls_wages_aux).
    APPEND INITIAL LINE TO lr_id ASSIGNING FIELD-SYMBOL(<fs_id>).
    <fs_id>-sign = 'I'.
    <fs_id>-option = 'EQ'.
    <fs_id>-low = ls_wages_aux-id.
  ENDLOOP.

  SELECT id
    FROM ztwfm_hcm_integ
    INTO TABLE @DATA(lt_integrated)
    WHERE id IN @lr_id
      AND api = 'WAGES'
      AND integrated = @abap_true.

  IF sy-subrc = 0.
    LOOP AT lt_integrated INTO DATA(ls_integrated).
      SHIFT ls_integrated-id LEFT DELETING LEADING '0'.
      DELETE ls_json-data-wages_result WHERE id = ls_integrated-id.
    ENDLOOP.
  ENDIF.

  lt_output_w = cs_output.

  LOOP AT ls_json-data-wages_result INTO DATA(ls_wages).

    IF io_fluxo->gv_test EQ abap_true.
      DATA(lv_commit) = abap_true.
    ELSE.
      lv_commit = abap_false.
    ENDIF.

    IF ls_wages-contingent_id IS NOT INITIAL.
      lv_subty = ls_wages-contingent_id.
    ELSE.
      lv_subty = ls_wages-wage_type.
    ENDIF.

    IF ls_wages-exclude_record IS NOT INITIAL.
      lv_operation = 'DEL'.
    ELSE.
      lv_operation = 'INS'.
    ENDIF.

* APP 04.07.2023 - Automatismo 9020 no inicio do mês

    IF lv_subty NE '41'
* >>> INI Inetum SAM EMP/SS HR 7000189109 15.12.2023
   AND lv_subty NE '42'.
* <<< END Inetum SAM EMP/SS HR 7000189109 15.12.2023
      READ TABLE gt_subtype INTO DATA(ls_subtype) WITH KEY subtype = lv_subty.
      IF sy-subrc = 0.

        IF lv_subty EQ '3044'.
          ls_subtype-infotype = '2002'.
        ENDIF.

        DATA(lv_struct) = 'P' && ls_subtype-infotype.
        CREATE DATA lv_data TYPE (lv_struct).
        ASSIGN lv_data->* TO FIELD-SYMBOL(<fs_record>).

        io_fluxo->get_record(
          EXPORTING
            iv_api     = 'WAGES'
            is_data    = ls_wages
          IMPORTING
            es_record  = <fs_record>
        ).

        ASSIGN COMPONENT 'PERNR' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_pernr>).
        ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_subty>).
        ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_begda>).
        ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_endda>).
        ASSIGN COMPONENT 'AEDTM' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_aedtm>).

*          IF ls_wages-exclude_record IS NOT INITIAL.
*            lv_operation = 'DEL'.
*          ELSE.
*            lv_operation = 'INS'.
*          ENDIF.

* >>> INI Inetum SAM EMP/SS HR 7000195680 01.03.2024
        IF ls_subtype-infotype EQ '2002'.
          ASSIGN COMPONENT 'LGART' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_lgart>).
          IF sy-subrc EQ 0
        AND <fs_lgart> IS ASSIGNED.
            CLEAR: <fs_lgart>.
            FREE: <fs_lgart>.
          ENDIF.
        ENDIF.
* <<< END Inetum SAM EMP/SS HR 7000195680 01.03.2024

        IF ls_subtype-subtype <> '9053'.

          zcl_hcm_ws_wfm=>infotype_operation(
            EXPORTING
              iv_infotype        = ls_subtype-infotype                " Infotipo
              iv_employee_number = <fs_pernr>                 " Nº pessoal
              iv_subty           = <fs_subty>                 " Subinfotipo
              iv_begda           = <fs_begda>                 " Início da validade
              iv_endda           = <fs_endda>                 " Fim da validade
              iv_operation       = lv_operation                 " Operação em infotipos
              iv_commit          = lv_commit
              is_record          = <fs_record>
            IMPORTING
              es_return          = ls_return                 " Parâmetro de retorno
              es_key             = ls_key                 " Chave para dados mestre HR
          ).

        ENDIF.

        CONCATENATE
        lv_operation
        'Pernr: '
        <fs_pernr>
        'Infotype: '
        ls_subtype-infotype
        'Subty: '
        <fs_subty>
        'Begda: '
        <fs_begda>
        'Endda: '
        <fs_endda>
        'Aedtm: '
        <fs_aedtm>
        INTO DATA(lv_msg) SEPARATED BY space.

        IF ls_return-type IS INITIAL.
          ls_return-type = 'S'.
        ENDIF.

        io_fluxo->add_text_log(
          EXPORTING
            iv_text  = lv_msg           " Mensagem
            iv_msgty = ls_return-type   " Tipo de mensagem
        ).
      ELSE.
        IF lv_subty IS NOT INITIAL.
          ls_return-id = gc_msgid.
          ls_return-number = '003'.
          ls_return-message_v1 = lv_subty.
          ls_return-type = 'E'.
        ELSE.
          ls_return-id = gc_msgid.
          ls_return-number = '004'.
          ls_return-message_v1 = lv_subty.
          ls_return-type = 'S'.
        ENDIF.

* >>> INI Inetum SAM EMP/SS HR 7000197743 25.03.2024
        IF lv_subty IS INITIAL AND ls_wages-record_type EQ 'S'.
          DATA: lv_day1 TYPE datum,
                lv_day2 TYPE datum.
          lv_day  = ls_wages-record_begin_date(4) && ls_wages-record_begin_date+5(2) && ls_wages-record_begin_date+8(2).
          lv_day1 = ls_wages-record_end_date(4) && ls_wages-record_end_date+5(2) && ls_wages-record_end_date+8(2).
          lv_day2 = ls_wages-record_date(4) && ls_wages-record_date+5(2) && ls_wages-record_date+8(2).

          CONCATENATE
          lv_operation
          'Pernr: '
          ls_wages-employee_id
          'Infotype: '
          '0015' " ls_subtype-infotype
          'Subty: '
          'S - 9053'
          'Begda: '
          lv_day
          'Endda: '
          lv_day1
          'Aedtm: '
          lv_day2
          INTO lv_msg SEPARATED BY space.

          ls_return-type = 'S'.

          io_fluxo->add_text_log(
            EXPORTING
              iv_text  = lv_msg           " Mensagem
              iv_msgty = ls_return-type   " Tipo de mensagem
          ).
        ENDIF.
* <<< END Inetum SAM EMP/SS HR 7000197743 25.03.2024
      ENDIF.

      ls_msg-msgid = ls_return-id.
      ls_msg-msgty = ls_return-type.
      ls_msg-msgno = ls_return-number.
      ls_msg-msgv1 = ls_return-message_v1.
      ls_msg-msgv2 = ls_return-message_v2.
      ls_msg-msgv3 = ls_return-message_v3.
      ls_msg-msgv4 = ls_return-message_v4.

      io_fluxo->add_msg_log(
        EXPORTING
          us_msg    = ls_msg                 " Log de aplicação: dados de uma mensagem
        EXCEPTIONS
          error_log = 1                " Erro em registo de Log
          OTHERS    = 2
      ).

      IF lv_subty = '9053'.
* Inetum - APP - Ajuste PA0015 - 9053 - Bloco de mês e não diario
        lv_last_day = <fs_endda>.

        CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
          EXPORTING
            day_in            = <fs_begda>
          IMPORTING
            last_day_of_month = lv_last_day
          EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.

        CONCATENATE lv_last_day+0(6) '01' INTO lv_first_day.

        IF lv_operation = 'INS'.
          APPEND INITIAL LINE TO lt_sched_ins ASSIGNING FIELD-SYMBOL(<fs_sched_ins>).
          <fs_sched_ins>-mandt = sy-mandt.
          <fs_sched_ins>-pernr = <fs_pernr>.
          <fs_sched_ins>-begda = <fs_begda>.
          <fs_sched_ins>-endda = <fs_endda>.
          <fs_sched_ins>-anzhl = ls_wages-operation_value.

* >>> INI Inetum SAM EMP/SS HR 7000206480 29.07.2024
          <fs_sched_ins>-aedtm = sy-datum.
          <fs_sched_ins>-uzeit = sy-uzeit.
          <fs_sched_ins>-uname = sy-uname.
* <<< END Inetum SAM EMP/SS HR 7000206480 29.07.2024

**** >>> INI Inetum SAM EMP/SS HR 7000189109 15.12.2023
***            ls_sched = <fs_sched_ins>.
***            ls_sched-begda = lv_first_day.
***            ls_sched-endda = lv_last_day.
***            COLLECT ls_sched INTO lt_sched.
**** <<< END Inetum SAM EMP/SS HR 7000189109 15.12.2023
        ELSE.
          APPEND INITIAL LINE TO lt_sched_del ASSIGNING FIELD-SYMBOL(<fs_sched_del>).
          <fs_sched_del>-mandt = sy-mandt.
          <fs_sched_del>-pernr = <fs_pernr>.
          <fs_sched_del>-begda = <fs_begda>.
          <fs_sched_del>-endda = <fs_endda>.
          <fs_sched_del>-anzhl = ls_wages-operation_value.
        ENDIF.

* >>> INI Inetum SAM EMP/SS HR 7000191601 15.02.2024
* Comentado, porque o registo vai passar a ser feito no fim do mês
*          SELECT SINGLE * FROM pa0015
*            INTO CORRESPONDING FIELDS OF ls_9053
*            WHERE pernr = <fs_pernr>
*              AND subty = lv_subty
*              AND begda = lv_first_day.

        SELECT * FROM pa0015
          INTO CORRESPONDING FIELDS OF ls_9053
          WHERE pernr = <fs_pernr>
            AND subty = lv_subty
            AND begda LE lv_last_day
            AND endda GE lv_first_day.
        ENDSELECT.
* <<< END Inetum SAM EMP/SS HR 7000191601 15.02.2024

        IF sy-subrc EQ 0.
          READ TABLE lt_9053 ASSIGNING FIELD-SYMBOL(<fs_9053>) WITH KEY pernr = ls_9053-pernr
                                                                        subty = ls_9053-subty
                                                                        begda = ls_9053-begda.
          IF sy-subrc EQ 0.
            ASSIGN COMPONENT 'UNAME' OF STRUCTURE <fs_9053> TO FIELD-SYMBOL(<fs_uname>).
            ASSIGN COMPONENT 'AEDTM' OF STRUCTURE <fs_9053> TO FIELD-SYMBOL(<fs_aedtm2>).
            ASSIGN COMPONENT 'ANZHL' OF STRUCTURE <fs_9053> TO FIELD-SYMBOL(<fs_anzhl>).

            IF lv_operation EQ 'INS'.
              ADD ls_wages-operation_value TO <fs_anzhl>.
              <fs_uname> = sy-uname.
              <fs_aedtm2> = sy-datum.
            ELSE.
              SUBTRACT ls_wages-operation_value FROM <fs_anzhl>.
              <fs_uname> = sy-uname.
              <fs_aedtm2> = sy-datum.
            ENDIF.
* >>> INI Inetum SAM EMP/SS HR 7000189109 06.03.2024
            MODIFY lt_9053_old FROM <fs_9053>
                               TRANSPORTING anzhl
                               WHERE pernr = ls_9053-pernr
                                 AND subty = ls_9053-subty
                                 AND begda = ls_9053-begda.
* <<< END Inetum SAM EMP/SS HR 7000189109 06.03.2024
          ELSE.
            IF lv_operation EQ 'INS'.
              ADD ls_wages-operation_value TO ls_9053-anzhl.
              ls_9053-uname = sy-uname.
              ls_9053-aedtm = sy-datum.
              APPEND ls_9053 TO lt_9053.
            ELSE.
              SUBTRACT ls_wages-operation_value FROM ls_9053-anzhl.
              ls_9053-uname = sy-uname.
              ls_9053-aedtm = sy-datum.
              APPEND ls_9053 TO lt_9053.
            ENDIF.
* >>> INI Inetum SAM EMP/SS HR 7000189109 06.03.2024
            APPEND ls_9053 TO lt_9053_old.
* <<< END Inetum SAM EMP/SS HR 7000189109 06.03.2024
          ENDIF.
        ELSE.
          READ TABLE lt_9053 ASSIGNING FIELD-SYMBOL(<fs_9053_0015>) WITH KEY pernr = <fs_pernr>
                                                                             subty = <fs_subty>
* >>> INI Inetum SAM EMP/SS HR 7000191601 15.02.2024
* Comentado, porque o registo vai passar a ser feito no fim do mês
*                                                                               begda = lv_first_day.
                                                                             begda = lv_last_day.
* >>> INI Inetum SAM EMP/SS HR 7000191601 15.02.2024

          IF sy-subrc EQ 0.
            IF lv_operation EQ 'INS'.
              ADD ls_wages-operation_value TO <fs_9053_0015>-anzhl.
              <fs_9053_0015>-uname = sy-uname.
              <fs_9053_0015>-aedtm = sy-datum.
            ELSE.
              SUBTRACT ls_wages-operation_value FROM <fs_9053_0015>-anzhl.
              <fs_9053_0015>-uname = sy-uname.
              <fs_9053_0015>-aedtm = sy-datum.
            ENDIF.
          ELSE.
            APPEND INITIAL LINE TO lt_9053 ASSIGNING <fs_9053_0015>.
            <fs_9053_0015>-pernr = ls_wages-employee_id.
            <fs_9053_0015>-lgart = <fs_9053_0015>-subty = lv_subty.
* >>> INI Inetum SAM EMP/SS HR 7000191601 15.02.2024
* Comentado, porque o registo vai passar a ser feito no fim do mês
**** >>> INI Inetum SAM EMP/SS HR 7000189109 15.12.2023
**** Comentado
****              <fs_9053_0015>-begda = lv_first_day.
****              <fs_9053_0015>-endda = lv_last_day.
***              <fs_9053_0015>-begda = lv_first_day.
***              <fs_9053_0015>-endda = lv_first_day.
**** <<< END Inetum SAM EMP/SS HR 7000189109 15.12.2023

            <fs_9053_0015>-begda = lv_last_day.
            <fs_9053_0015>-endda = lv_last_day.
* >>> INI Inetum SAM EMP/SS HR 7000191601 15.02.2024

            <fs_9053_0015>-aedtm = sy-datum.
            <fs_9053_0015>-uname = sy-uname.
            <fs_9053_0015>-waers = 'EUR'.
            <fs_9053_0015>-anzhl = ls_wages-operation_value.
            "T511 -  Validar por datas begda -> Para evitar registos anteriores a 01.03.2023 = 900
            <fs_9053_0015>-zeinh = '001'.
            "T511 -  Validar por datas begda -> Para evitar registos anteriores a 01.03.2023 = 900

          ENDIF.
        ENDIF.
      ENDIF.

* >>> INI Inetum SAM EMP/SS HR 7000197743 25.03.2024
      IF ls_wages-record_type EQ 'S'
     AND ls_wages-wage_type IS INITIAL
     AND ls_wages-exclude_record EQ abap_false.
* Quando vem um registo do tipo 'S' e não exista registo do tipo 'P',
* indica que deve ser zerada a rubrica 9053no IT0015
        READ TABLE ls_json-data-wages_result INTO ls_wages_aux
                                             WITH KEY employee_id    = ls_wages-employee_id
                                                      wage_type      = '9053'
                                                      exclude_record = abap_false
                                                      record_type    = 'P'.
*                                                      record_end_date(6) = ls_wages-record_end_date(6).
        "Não encontrar nenhuma entrada P com INS
        IF sy-subrc NE 0.
          READ TABLE lt_output_w-wages_result INTO DATA(ls_output_w2) WITH KEY employee_id     = ls_wages-employee_id
                                                                               wage_type       = '9053'
                                                                               exclude_record  = abap_false
                                                                               record_type     = 'P'.
*                                                                               record_end_date = ls_wages-record_end_date.
          "Na tabela interna auxiliar 9053, não encontrar dados -> Zerar ANZHL
          IF sy-subrc NE 0.
            lv_day = ls_wages-record_begin_date(4) && ls_wages-record_begin_date+5(2) && ls_wages-record_begin_date+8(2).
            lv_day+6(2) ='01'.
            CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
              EXPORTING
                day_in            = lv_day
              IMPORTING
                last_day_of_month = lv_last_day
              EXCEPTIONS
                day_in_no_date    = 1
                OTHERS            = 2.

            CLEAR: ls_p0015.
            SELECT SINGLE * INTO ls_p0015
                            FROM pa0015
                            WHERE pernr EQ ls_wages-employee_id
                              AND subty EQ '9053'
                              AND begda LE lv_last_day
                              AND endda GE lv_day.
            IF sy-subrc EQ 0.
              "elimina-se o registo que exista
              DELETE pa0015 FROM ls_p0015.
            ENDIF.
            READ TABLE lt_9053 ASSIGNING <fs_9053_0015> WITH KEY pernr = ls_wages-employee_id
                                                                 subty = '9053'
                                                                 begda = lv_last_day.
            IF sy-subrc EQ 0.
              IF lv_operation EQ 'INS'.
                <fs_9053_0015>-anzhl = '0'.
                <fs_9053_0015>-uname = sy-uname.
                <fs_9053_0015>-aedtm = sy-datum.
              ENDIF.
            ELSE.
              APPEND INITIAL LINE TO lt_9053 ASSIGNING <fs_9053_0015>.
              <fs_9053_0015>-pernr = ls_wages-employee_id.
              <fs_9053_0015>-lgart = <fs_9053_0015>-subty = '9053'.
              <fs_9053_0015>-begda = lv_last_day.
              <fs_9053_0015>-endda = lv_last_day.
              <fs_9053_0015>-aedtm = sy-datum.
              <fs_9053_0015>-uname = sy-uname.
              <fs_9053_0015>-waers = 'EUR'.
              <fs_9053_0015>-anzhl = '0'.
              <fs_9053_0015>-zeinh = '001'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
* <<< END Inetum SAM EMP/SS HR 7000197743 25.03.2024

* >>> INI Inetum SAM EMP/SS HR 7000189109 06.03.2024
* Comentado porque o que vai determinar o que vai ser inserido, eliminado ou alterado
* é o que está a ser feito no fim deste loop aos resultados que vêm do sistema origem
**********************************************************
* Inetum - APP - Ajuste PA0015 - 9053/Tipo S- Zerar ANZHL
****      "Se o Tipo = S e a rubrica vier a vazio -> Significa que não existem mais dados do lado de WFM para puxar (fim de mês)
***      IF ls_wages-record_type EQ 'S' AND ls_wages-wage_type IS INITIAL AND ls_wages-exclude_record EQ abap_false.
***
***        READ TABLE ls_json-data-wages_result INTO ls_wages_aux WITH KEY employee_id = ls_wages-employee_id
***                                                                        wage_type = '9053'
***                                                                        exclude_record = abap_false
***                                                                        record_type = 'P'
****                                                                          record_end_date = lv_last_day.
***                                                                        record_end_date = ls_wages-record_end_date.
***        "Não encontrar nenhuma entrada P com INS
***        IF sy-subrc NE 0.
***
****            READ TABLE gt_9053 INTO gs_9053 WITH KEY pernr = ls_wages-employee_id
****                                                     subty = '9053'
****                                                     endda = lv_last_day.
***
***
***          READ TABLE lt_output_w-wages_result INTO DATA(ls_output_w) WITH KEY employee_id = ls_wages-employee_id
***                                                                              wage_type = '9053'
***                                                                              exclude_record = abap_false
***                                                                              record_type = 'P'
***                                                                              record_end_date = ls_wages-record_end_date.
***
***          "Na tabela interna auxiliar 9053, não encontrar dados -> Zerar ANZHL
***          IF sy-subrc NE 0.
***
***            lv_day = ls_wages-record_begin_date(4) && ls_wages-record_begin_date+5(2) && ls_wages-record_begin_date+8(2).
***
***            CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
***              EXPORTING
***                day_in            = lv_day
***              IMPORTING
***                last_day_of_month = lv_last_day
***              EXCEPTIONS
***                day_in_no_date    = 1
***                OTHERS            = 2.
***
***            SELECT SINGLE * FROM pa0015
***            INTO @ls_p0015
***            WHERE pernr  = @ls_wages-employee_id
***              AND subty = '9053'
****                and begda le @ls_wages-record_begin_date
****                and endda ge @ls_wages-record_end_date.
***** >>> INI Inetum SAM EMP/SS HR 7000189109 15.12.2023
***** Comentado
***              AND endda = @lv_last_day.    " INS 7000191601 15.01.2024
****                AND endda = @lv_first_day.
***** <<< END Inetum SAM EMP/SS HR 7000189109 15.12.2023
***
***            IF sy-subrc EQ 0.
***              "Passamos o valor dessa entrada para 0
***              ls_p0015-anzhl = 0.
***              MODIFY pa0015 FROM ls_p0015.
***            ELSE.
***              lv_day = ls_wages-record_begin_date(4) && ls_wages-record_begin_date+5(2) && ls_wages-record_begin_date+8(2).
***
***              CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
***                EXPORTING
***                  day_in            = lv_day
***                IMPORTING
***                  last_day_of_month = lv_last_day
***                EXCEPTIONS
***                  day_in_no_date    = 1
***                  OTHERS            = 2.
***
***              CONCATENATE lv_last_day+0(6) '01' INTO lv_first_day.
***
***              READ TABLE lt_9053 ASSIGNING <fs_9053_0015> WITH KEY pernr = ls_wages-employee_id
***                                                                   subty = '9053'
**** >>> INI Inetum SAM EMP/SS HR 7000189109 15.12.2023
**** Comentado
****                                                                     begda = lv_first_day.
***                                                                   begda = lv_last_day.
**** <<< END Inetum SAM EMP/SS HR 7000189109 15.12.2023
***
***              IF sy-subrc NE 0.
***                APPEND INITIAL LINE TO lt_9053 ASSIGNING <fs_9053_0015>.
***                <fs_9053_0015>-pernr = ls_wages-employee_id.
***                <fs_9053_0015>-lgart = <fs_9053_0015>-subty = '9053'.
****                  <fs_9053_0015>-begda = lv_first_day.
****                  <fs_9053_0015>-endda = lv_last_day.
**** >>> INI Inetum SAM EMP/SS HR 7000189109 15.12.2023
**** Comentado
****                  <fs_9053_0015>-begda = lv_first_day.
****                  <fs_9053_0015>-endda = lv_last_day.
***                <fs_9053_0015>-begda = lv_last_day.
***                <fs_9053_0015>-endda = lv_last_day.
**** <<< END Inetum SAM EMP/SS HR 7000189109 15.12.2023
***                <fs_9053_0015>-aedtm = sy-datum.
***                <fs_9053_0015>-uname = sy-uname.
***                <fs_9053_0015>-waers = 'EUR'.
***                <fs_9053_0015>-anzhl = '0'.
***                <fs_9053_0015>-zeinh = '001'.
***              ENDIF.
***
***
****                IF sy-subrc EQ 0.
****                  "Alteramos o valor dessa entrada interna para 0
****                  <fs_9053_0015>-anzhl = 0.
****                ELSE.
****                  APPEND INITIAL LINE TO lt_9053 ASSIGNING <fs_9053_0015>.
****                  <fs_9053_0015>-pernr = ls_wages-employee_id.
****                  <fs_9053_0015>-lgart = <fs_9053_0015>-subty = '9053'.
****                  <fs_9053_0015>-begda = lv_first_day.
****                  <fs_9053_0015>-endda = lv_last_day.
****                  <fs_9053_0015>-aedtm = sy-datum.
****                  <fs_9053_0015>-uname = sy-uname.
****                  <fs_9053_0015>-waers = 'EUR'.
****                  <fs_9053_0015>-anzhl = '0'.
****                  <fs_9053_0015>-zeinh = '001'.
****                ENDIF.
***            ENDIF.
***          ENDIF.
***        ENDIF.
***      ENDIF.
**********************************************************
* <<< END Inetum SAM EMP/SS HR 7000189109 06.03.2024


*          READ TABLE lt_9053 ASSIGNING <fs_9053_0015> WITH KEY pernr = ls_wages-employee_id
*                                                               subty = '9053'
*                                                               begda = lv_first_day.

* Inetum - APP - Ajuste PA0015 - 9053/Tipo S- Zerar ANZHL

* Inetum - APP - Ajuste PA0015 - 9053 - Bloco de mês e não diario

      APPEND INITIAL LINE TO lt_integ ASSIGNING FIELD-SYMBOL(<fs_integ>).
      <fs_integ>-mandt = sy-mandt.
      <fs_integ>-id = ls_wages-id.
      <fs_integ>-api = 'WAGES'.
      <fs_integ>-operation = lv_operation.

* Alterado 02.05.23 - APP - Passar o PERNR para a tab integr, caso contrário os valores saem sempre a vazio
      " Se der erro report para contingent_id

*        <fs_integ>-pernr = ls_wages-contingent_id.
      <fs_integ>-pernr = ls_wages-employee_id.

* Alterado 02.05.23 - APP - Passar o PERNR para a tab integr, caso contrário os valores saem sempre a vazio

      <fs_integ>-subty = lv_subty.
      <fs_integ>-begda = ls_wages-record_begin_date(4) && ls_wages-record_begin_date+5(2) && ls_wages-record_begin_date+8(2).
      <fs_integ>-endda = ls_wages-record_end_date(4) && ls_wages-record_end_date+5(2) && ls_wages-record_end_date+8(2).

      CLEAR: lv_operation, lv_subty.

      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = ls_return-id
          lang      = sy-langu
          no        = ls_return-number
          v1        = ls_return-message_v1
          v2        = ls_return-message_v2
          v3        = ls_return-message_v3
          v4        = ls_return-message_v4
        IMPORTING
          msg       = <fs_integ>-msg
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.

      IF ls_return-type EQ 'S' AND io_fluxo->gv_test IS INITIAL.
        <fs_integ>-integrated = abap_true.
      ELSEIF ls_return-type EQ 'E'.
        <fs_integ>-integrated = abap_false.
        lv_error = 'E'.
      ENDIF.

    ELSE.

      READ TABLE gt_subtype INTO ls_subtype WITH KEY subtype = lv_subty.
      IF sy-subrc = 0.

        ls_return-type = 'S'.

        CONCATENATE
          'Ignorado'
          'Pernr: '
          ls_wages-employee_id
          'Infotype: '
          ls_subtype-infotype
          'Subty: '
          lv_subty
          'Begda: '
          ls_wages-record_begin_date
          'Endda: '
          ls_wages-record_end_date
          'Aedtm: '
          ls_wages-record_date
          INTO lv_msg SEPARATED BY space.

        io_fluxo->add_text_log(
          EXPORTING
            iv_text  = lv_msg           " Mensagem
            iv_msgty = ls_return-type   " Tipo de mensagem
        ).

      ENDIF.
    ENDIF.

    UNASSIGN <fs_integ>.

    CLEAR: ls_return, ls_key.
  ENDLOOP.

  IF lv_error IS INITIAL AND sy-subrc = 0.
    lv_error = 'S'.
  ELSE.
    lv_error = 'E'.
  ENDIF.

  cv_error = lv_error.

  IF sy-subrc = 0.
    ASSIGN COMPONENT 'WAGES_RESULT' OF STRUCTURE cs_output TO FIELD-SYMBOL(<fs_wages_result>).
    <fs_wages_result> = ls_json-data-wages_result.

    DELETE ztwfm_hcm_sched FROM TABLE lt_sched_del.
    IF sy-subrc = 0.

      MODIFY ztwfm_hcm_sched FROM TABLE lt_sched_ins.
      IF sy-subrc = 0.


* >>> INI Inetum SAM EMP/SS HR 7000189109 06.03.2024
        DATA: lv_index TYPE sy-tabix.

        LOOP AT lt_9053_old ASSIGNING FIELD-SYMBOL(<fs_9053_old>)
                            WHERE begda NE lv_last_day.
          lv_index = sy-tabix.
          LOOP AT lt_9053 ASSIGNING <fs_9053>
                          WHERE begda NE lv_last_day.
            <fs_9053>-begda = lv_last_day.
            <fs_9053>-endda = lv_last_day.
          ENDLOOP.
          IF sy-subrc EQ 0.
            CLEAR: <fs_9053_old>-anzhl.
          ELSE.
            DELETE lt_9053_old INDEX lv_index.
          ENDIF.
        ENDLOOP.
        IF sy-subrc EQ 0.
          MODIFY pa0015 FROM TABLE lt_9053_old.
          IF sy-subrc EQ 0.
            COMMIT WORK AND WAIT.
          ENDIF.
        ENDIF.
* <<< END Inetum SAM EMP/SS HR 7000189109 06.03.2024

* >>> INI Inetum SAM EMP/SS HR 7000189109 15.12.2023
* Validar se o que vai ser atualizado no IT0015 é efetivamente
* aquilo que veio no inteface, guardado na tabela ZTWFM_HCM_SCHED
        COMMIT WORK AND WAIT.

        REFRESH: lt_sched.
        SELECT * INTO TABLE lt_sched
                 FROM ztwfm_hcm_sched
                 FOR ALL ENTRIES IN lt_9053
                 WHERE pernr EQ lt_9053-pernr
                   AND begda LE lv_last_day
                   AND endda GE lv_first_day.
        LOOP AT lt_sched INTO ls_sched.
          MOVE-CORRESPONDING ls_sched TO ls_new_sched.
          COLLECT ls_new_sched INTO lt_new_sched.
        ENDLOOP.

        CLEAR: new_anzhl.
        LOOP AT lt_9053 ASSIGNING <fs_9053>.
          IF last_pernr IS INITIAL
          OR last_pernr NE <fs_9053>-pernr.
            CLEAR: new_anzhl.
          ENDIF.
          IF <fs_9053>-anzhl IS NOT INITIAL.
            LOOP AT lt_new_sched INTO ls_new_sched
                                 WHERE pernr EQ <fs_9053>-pernr.
              ADD ls_new_sched-anzhl TO new_anzhl.
            ENDLOOP.
            IF sy-subrc EQ 0
           AND <fs_9053>-anzhl NE new_anzhl.
              <fs_9053>-anzhl = new_anzhl.
            ENDIF.
          ENDIF.
          last_pernr = <fs_9053>-pernr.
        ENDLOOP.
* <<< END Inetum SAM EMP/SS HR 7000189109 15.12.2023

        MODIFY pa0015 FROM TABLE lt_9053.
        IF sy-subrc EQ 0.
* >>> INI Inetum SAM EMP/SS HR 7000189109 06.03.2024
          COMMIT WORK AND WAIT.
* <<< END Inetum SAM EMP/SS HR 7000189109 06.03.2024

          MODIFY ztwfm_hcm_integ FROM TABLE lt_integ.
          IF sy-subrc = 0.
            IF NOT line_exists( lt_integ[ integrated = abap_false ] ).
              COMMIT WORK AND WAIT.
              cv_flag = abap_true.
              cv_confirm = ls_json-data-wages_result_out_id.
              io_fluxo->delivery_confirmation(
                EXPORTING
                  iv_conf_id = ls_json-data-wages_result_out_id
                  io_fluxo   = io_fluxo                 " Classe Interface wfm
              ).
            ENDIF.

          ELSE.
            ROLLBACK WORK.
            lv_error_aux = 'E'.
          ENDIF.
        ELSE.
          ROLLBACK WORK.
          lv_error_aux = 'E'.
        ENDIF.
      ELSE.
        ROLLBACK WORK.
        lv_error_aux = 'E'.
      ENDIF.
    ELSE.
      ROLLBACK WORK.
      lv_error_aux = 'E'.
    ENDIF.
  ENDIF.

  "Caso seja necessário reverter manualmente o 9020 - Em caso de erro
  IF lv_error_aux = 'E'.
    LOOP AT lt_9020 INTO ls_9020.

      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
*         day_in            = ls_wages-record_end_date
          day_in            = ls_9020-endda
        IMPORTING
          last_day_of_month = lv_last_day
        EXCEPTIONS
          day_in_no_date    = 1
          OTHERS            = 2.

      SELECT SINGLE * FROM pa0015
        INTO @ls_15_9020
        WHERE pernr = @ls_9020
          AND subty = '9020'
          AND begda >= @ls_9020-begda
          AND endda <= @ls_9020-endda.

      IF sy-subrc EQ 0.

        READ TABLE gt_subtype INTO ls_subtype WITH KEY subtype = ls_15_9020-subty.
        IF sy-subrc EQ 0.

          lv_struct = 'P' && ls_subtype-infotype.
          CREATE DATA lv_data TYPE (lv_struct).
          ASSIGN lv_data->* TO FIELD-SYMBOL(<fs_record_del>).

          zcl_hcm_ws_wfm=>infotype_operation_delete(
            EXPORTING
              iv_infotype        = ls_subtype-infotype                 " Infotipo
              iv_employee_number = ls_15_9020-pernr                 " Nº pessoal
              iv_subty           = ls_15_9020-subty                 " Subinfotipo
              iv_begda           = ls_15_9020-begda                 " Início da validade
              iv_endda           = ls_15_9020-endda                 " Fim da validade
              iv_operation       = 'DEL'               " Operação em infotipos
              iv_commit          = lv_commit
            IMPORTING
              es_return          = ls_return                 " Parâmetro de retorno
              es_key             = ls_key                 " Chave para dados mestre HR
            CHANGING
               cs_record = <fs_record_del>
          ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


  METHOD http_request.
    DATA: lo_http_client TYPE REF TO if_http_client.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = iv_url
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4
    ).

    lo_http_client->request->set_method( method = iv_request ).
    lo_http_client->request->set_content_type( content_type = 'application/json' ).
    lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.
    lo_http_client->request->set_header_fields( fields = it_fields ).

    lo_http_client->request->set_cdata(
      EXPORTING
        data   = iv_data
    ).

    lo_http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5
    ).

    lo_http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4
    ).

    lo_http_client->response->get_status(
      IMPORTING
        code   = ev_status_code
        reason = ev_operation_info
    ).

    ev_response = lo_http_client->response->get_cdata( ).

    lo_http_client->close(
      EXCEPTIONS
        http_invalid_state = 1
        OTHERS             = 2
    ).
  ENDMETHOD.


  METHOD infotype_operation.
    DATA: lv_dialog   TYPE c VALUE '0',

          ls_return   TYPE bapireturn1,

          lr_infotype TYPE RANGE OF infty,
          lr_subtype  TYPE RANGE OF subty.

* >>> INI Inetum SAM EMP/SS HR 7000197093 04.06.2024
    DATA: ls_pa0015 TYPE zwfm_hr_pa0015,
          ls_pa2001 TYPE zwfm_hr_pa2001,
          ls_pa2002 TYPE zwfm_hr_pa2002.
* <<< END Inetum SAM EMP/SS HR 7000197093 04.06.2024

    CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = iv_employee_number
      IMPORTING
        return = ls_return.

    IF ls_return IS NOT INITIAL.
* >>> INI Inetum SAM EMP/SS HR 7000197093 04.06.2024
      es_return = ls_return.

      CASE iv_infotype.
        WHEN '0015'.
          CLEAR: ls_pa0015.
          MOVE-CORRESPONDING is_record TO ls_pa0015.
          ls_pa0015-pernr     = iv_employee_number.
          ls_pa0015-subty     = iv_subty.
          ls_pa0015-sprps     = iv_sprps.
          ls_pa0015-new_endda = iv_endda.
          ls_pa0015-new_begda = iv_begda.
          ls_pa0015-seqnr     = iv_seqnr.
          ls_pa0015-operation = iv_operation.
          CLEAR: ls_pa0015-integrado.
          IF ls_pa0015 IS NOT INITIAL.
            MODIFY zwfm_hr_pa0015 FROM ls_pa0015.
          ENDIF.
        WHEN '2001'.
          CLEAR: ls_pa2001.
          MOVE-CORRESPONDING is_record TO ls_pa2001.
          ls_pa2001-pernr     = iv_employee_number.
          ls_pa2001-subty     = iv_subty.
          ls_pa2001-sprps     = iv_sprps.
          ls_pa2001-new_endda = iv_endda.
          ls_pa2001-new_begda = iv_begda.
          ls_pa2001-seqnr     = iv_seqnr.
          ls_pa2001-operation = iv_operation.
          CLEAR: ls_pa2001-integrado.
          IF ls_pa2001 IS NOT INITIAL.
            MODIFY zwfm_hr_pa2001 FROM ls_pa2001.
          ENDIF.
        WHEN '2002'.
          CLEAR: ls_pa2002.
          MOVE-CORRESPONDING is_record TO ls_pa2002.
          ls_pa2002-pernr     = iv_employee_number.
          ls_pa2002-subty     = iv_subty.
          ls_pa2002-sprps     = iv_sprps.
          ls_pa2002-new_endda = iv_endda.
          ls_pa2002-new_begda = iv_begda.
          ls_pa2002-seqnr     = iv_seqnr.
          ls_pa2002-operation = iv_operation.
          CLEAR: ls_pa2002-integrado.
          IF ls_pa2002 IS NOT INITIAL.
            MODIFY zwfm_hr_pa2002 FROM ls_pa2002.
          ENDIF.
      ENDCASE.
* <<< END Inetum SAM EMP/SS HR 7000197093 04.06.2024
      RETURN.
    ENDIF.

    lr_infotype = VALUE #( sign = 'I' option = 'EQ'
                         ( low = '2001' ) ).

    lr_subtype = VALUE #( sign = 'I' option = 'EQ'
                        ( low = '1001' )
                        ( low = '1013' )
                        ( low = '1022' )
                        ( low = '1038' )
                        ( low = '1039' ) ).

    IF iv_infotype IN lr_infotype AND
       iv_subty    IN lr_subtype.
      "Check before online execution
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = iv_infotype
          number        = iv_employee_number
          subtype       = iv_subty
          lockindicator = iv_sprps
          validityend   = iv_endda
          validitybegin = iv_begda
          record        = is_record
          dialog_mode   = lv_dialog
          nocommit      = iv_commit
          recordnumber  = iv_seqnr
          operation     = iv_operation
        IMPORTING
          return        = es_return
          key           = es_key
        EXCEPTIONS
          OTHERS        = 0.

      IF es_key IS NOT INITIAL.
        CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.
        lv_dialog = '1'.
      ELSE.
        CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
          EXPORTING
            number = iv_employee_number.

        RETURN.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = iv_infotype
        number        = iv_employee_number
        subtype       = iv_subty
        lockindicator = iv_sprps
        validityend   = iv_endda
        validitybegin = iv_begda
        record        = is_record
        dialog_mode   = lv_dialog
        nocommit      = iv_commit
        recordnumber  = iv_seqnr
        operation     = iv_operation
      IMPORTING
        return        = es_return
        key           = es_key
      EXCEPTIONS
        OTHERS        = 0.

    IF iv_commit = abap_true.
      ROLLBACK WORK.
      PERFORM initialize_ps IN PROGRAM sapfp50p.
    ENDIF.

    CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = iv_employee_number.
  ENDMETHOD.


  METHOD infotype_operation_delete.
    DATA:
      o_ref         TYPE REF TO data,
      lt_absences   TYPE TABLE OF zhcm_absences,

      lv_error_aux  TYPE zzca_statws,
      lv_auxiliar   TYPE tabname,
      ls_return     TYPE bapireturn1,
      ls_key        TYPE bapipakey,

      lv_diff_days  TYPE datum,
      lt_range      TYPE datum_dda,
      ls_range      TYPE dats,
      lt_absences_o TYPE TABLE OF zhcm_absences,
      ls_absences_o TYPE zhcm_absences,
      lv_prev_day   TYPE datum.

    FIELD-SYMBOLS:
      <fs_t_table>    TYPE STANDARD TABLE.

***********************************************************************************

    CONCATENATE 'PA' iv_infotype
           INTO lv_auxiliar.

    CREATE DATA o_ref TYPE TABLE OF (lv_auxiliar).
    ASSIGN o_ref->* TO <fs_t_table>.

    TRY.
        "Validar em debug se os intervalos de datas vão corresponder à query
        SELECT * FROM (lv_auxiliar)
          INTO TABLE <fs_t_table>
          WHERE pernr = iv_employee_number
            AND subty = iv_subty
            AND begda LE iv_begda
            AND endda GE iv_endda.
      CATCH cx_sy_dynamic_osql_semantics.
    ENDTRY.

    LOOP AT <fs_t_table> ASSIGNING FIELD-SYMBOL(<fs_s_table>).

      ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <fs_s_table> TO FIELD-SYMBOL(<fs_begda>).
      ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <fs_s_table> TO FIELD-SYMBOL(<fs_endda>).

      "Necessário para alterar valores do <fS_record> e eliminar a ausência correta
      ASSIGN COMPONENT 'BEGDA' OF STRUCTURE cs_record TO FIELD-SYMBOL(<fs_rec_begda>).
      ASSIGN COMPONENT 'ENDDA' OF STRUCTURE cs_record TO FIELD-SYMBOL(<fs_rec_endda>).
      <fs_rec_begda> = <fs_begda>.
      <fs_rec_endda> = <fs_endda>.

      zcl_hcm_ws_wfm=>infotype_operation(
        EXPORTING
          iv_infotype        = iv_infotype              " Infotipo
          iv_employee_number = iv_employee_number       " Nº pessoal
          iv_subty           = iv_subty                 " Subinfotipo
          iv_begda           = <fs_begda>               " Início da validade
          iv_endda           = <fs_endda>               " Fim da validade
          iv_operation       = 'DEL'                    " Operação em infotipos
          iv_commit          = iv_commit
          is_record          = cs_record
        IMPORTING
          es_return          = ls_return                 " Parâmetro de retorno
          es_key             = ls_key ).                 " Chave para dados mestre HR

      DATA(ls_ret_aux) = ls_return.

      "Tabela de datas dentro do intervalo que envio/preciso
      CALL FUNCTION 'J_1B_GET_DATE_RANGE_DDA'
        EXPORTING
          date_from  = <fs_begda>
          date_to    = <fs_endda>
        TABLES
          date_range = lt_range.

      "Calcular novo range de datas para o periodo eliminado
      LOOP AT lt_range INTO ls_range.
        IF ls_range NOT BETWEEN iv_begda AND iv_endda.

          IF ls_range+6(2) NE '01'.
            "Valida dia anterior para saber se já existe
            CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
              EXPORTING
                date      = ls_range
                days      = 1
                signum    = '-'
                months    = 0
                years     = 0
              IMPORTING
                calc_date = lv_prev_day.
          ELSE.
            lv_prev_day = ls_range.
          ENDIF.

          READ TABLE lt_absences_o ASSIGNING FIELD-SYMBOL(<fs_absences>) WITH KEY pernr = iv_employee_number
                                                                                  subty = iv_subty
                                                                                  endda = lv_prev_day.

          "SE SIM -> Atualizo o endda
          IF sy-subrc EQ 0.
            <fs_absences>-endda = ls_range.

            "SE NÃO -> Adiciono à tabela com informação do <fs_t_table> e altero as datas
          ELSE.
            MOVE-CORRESPONDING <fs_s_table> TO ls_absences_o.
            ls_absences_o-begda = ls_range.
            ls_absences_o-endda = ls_range.
            APPEND ls_absences_o TO lt_absences_o.
          ENDIF.

        ENDIF.
      ENDLOOP.

      "Para cada linha de periodos diferentes, calculados anteriormente -> Chamar o infotype_operation para integrar em SAP
      LOOP AT lt_absences_o INTO ls_absences_o.

        <fs_rec_begda> = ls_absences_o-begda.
        <fs_rec_endda> = ls_absences_o-endda.

        "Chamar o infotype_operation
        zcl_hcm_ws_wfm=>infotype_operation(
          EXPORTING
            iv_infotype        = iv_infotype                 " Infotipo
            iv_employee_number = iv_employee_number                " Nº pessoal
            iv_subty           = iv_subty                 " Subinfotipo
            iv_begda           = ls_absences_o-begda                 " Início da validade
            iv_endda           = ls_absences_o-endda                 " Fim da validade
            iv_operation       = 'INS'                " Operação em infotipos
            iv_commit          = iv_commit
            is_record          = cs_record
          IMPORTING
            es_return          = ls_return                 " Parâmetro de retorno
            es_key             = ls_key ).                 " Chave para dados mestre HR

*        "Como o ls_return vai ser da ultima operação -> Temos de definir uma mensagem caso dê erro
*        " SENÃO -> Vamos ter mensagem de erro de INS para uma operação de DEL
*        IF ls_ret_aux-type EQ 'E'.
*          ls_return-type = ls_ret_aux-type.
*          ls_return-id = ls_ret_aux-id.
*          ls_return-number = ls_ret_aux-number.
*        ENDIF.

        MOVE ls_return TO es_return.
        MOVE ls_key TO es_key.

      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD infotype_operation_insert.

    DATA:
      o_ref         TYPE REF TO data,
      lt_absences   TYPE TABLE OF zhcm_absences,

      lv_error_aux  TYPE zzca_statws,
      lv_auxiliar   TYPE tabname,
      ls_return     TYPE bapireturn1,
      ls_key        TYPE bapipakey,

      lv_diff_days  TYPE datum,
      lt_range      TYPE datum_dda,
      ls_range      TYPE dats,
      lt_absences_o TYPE TABLE OF zhcm_absences,
      ls_absences_o TYPE zhcm_absences,
      lv_prev_day   TYPE datum,
      lv_next_day   TYPE datum,
      lv_last_day   TYPE datum,
      lv_ins        TYPE abap_bool.

    FIELD-SYMBOLS:
      <fs_t_table>    TYPE STANDARD TABLE.

***********************************************************************************

    CONCATENATE 'PA' iv_infotype
           INTO lv_auxiliar.

    CREATE DATA o_ref TYPE TABLE OF (lv_auxiliar).
    ASSIGN o_ref->* TO <fs_t_table>.

    IF iv_begda+6(2) NE '01'.
      "Valida dia anterior para saber se já existe
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = iv_begda
          days      = 1
          signum    = '-'
          months    = 0
          years     = 0
        IMPORTING
          calc_date = lv_prev_day.
    ELSE.
      lv_prev_day = iv_begda.
    ENDIF.

    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = iv_endda
      IMPORTING
        last_day_of_month = lv_last_day
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.

    IF iv_endda NE lv_last_day.
      "Valida dia seguinte para saber se já existe
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = iv_endda
          days      = 1
          signum    = '+'
          months    = 0
          years     = 0
        IMPORTING
          calc_date = lv_next_day.
    ELSE.
      lv_next_day = iv_endda.
    ENDIF.

    TRY.
        "Validar em debug se os intervalos de datas vão corresponder à query
        SELECT * FROM (lv_auxiliar)
          INTO TABLE <fs_t_table>
          WHERE pernr = iv_employee_number
            AND subty = iv_subty
            AND ( begda EQ lv_next_day          "Neste caso 07.03(08.03) eq 08.03
            OR endda EQ lv_prev_day ).          "05.03(04.03) eq BEGDA 04.03

      CATCH cx_sy_dynamic_osql_semantics.
    ENDTRY.

*>>APP STDAZ - dia inteiro 18/07/2023
    IF sy-subrc EQ 0.
      ASSIGN COMPONENT 'STDAZ' OF STRUCTURE cs_record TO FIELD-SYMBOL(<fs_value>).

*    IF sy-subrc EQ 0 AND ( <fs_value> IS INITIAL OR <fs_value> = '8' ).
      IF <fs_value> IS INITIAL OR <fs_value> = '8' .
*<<APP STDAZ - dia inteiro 18/07/2023

        LOOP AT <fs_t_table> ASSIGNING FIELD-SYMBOL(<fs_s_table>).

          ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <fs_s_table> TO FIELD-SYMBOL(<fs_begda>).
          ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <fs_s_table> TO FIELD-SYMBOL(<fs_endda>).

          "Necessário para alterar valores do <fS_record> e eliminar a ausência correta
          ASSIGN COMPONENT 'BEGDA' OF STRUCTURE cs_record TO FIELD-SYMBOL(<fs_rec_begda>).
          ASSIGN COMPONENT 'ENDDA' OF STRUCTURE cs_record TO FIELD-SYMBOL(<fs_rec_endda>).
          <fs_rec_begda> = <fs_begda>.
          <fs_rec_endda> = <fs_endda>.

          zcl_hcm_ws_wfm=>infotype_operation(
            EXPORTING
              iv_infotype        = iv_infotype              " Infotipo
              iv_employee_number = iv_employee_number       " Nº pessoal
              iv_subty           = iv_subty                 " Subinfotipo
              iv_begda           = <fs_begda>               " Início da validade
              iv_endda           = <fs_endda>               " Fim da validade
              iv_operation       = 'DEL'                    " Operação em infotipos
              iv_commit          = iv_commit
              is_record          = cs_record
            IMPORTING
              es_return          = ls_return                 " Parâmetro de retorno
              es_key             = ls_key ).                 " Chave para dados mestre HR

          "Seria até ao final do método
          DATA(ls_ret_aux) = ls_return.

*      "Tabela de datas dentro do intervalo que envio/preciso
          CALL FUNCTION 'J_1B_GET_DATE_RANGE_DDA'
            EXPORTING
              date_from  = <fs_begda>
              date_to    = <fs_endda>
            TABLES
              date_range = lt_range.

*      "Calcular novo range de datas para o periodo eliminado
          LOOP AT lt_range INTO ls_range.

            IF ls_range+6(2) NE '01'.
              "Valida dia anterior para saber se já existe
              CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
                EXPORTING
                  date      = ls_range
                  days      = 1
                  signum    = '-'
                  months    = 0
                  years     = 0
                IMPORTING
                  calc_date = lv_prev_day.
            ELSE.
              lv_prev_day = ls_range.
            ENDIF.

            READ TABLE lt_absences_o ASSIGNING FIELD-SYMBOL(<fs_absences>) WITH KEY pernr = iv_employee_number
                                                                                    subty = iv_subty
                                                                                    endda = lv_prev_day.

            "SE SIM -> Atualizo o endda
            IF sy-subrc EQ 0.
              <fs_absences>-endda = ls_range.

              "SE NÃO -> Adiciono à tabela com informação do <fs_t_table> e altero as datas
            ELSE.
              MOVE-CORRESPONDING <fs_s_table> TO ls_absences_o.
              ls_absences_o-begda = ls_range.
              ls_absences_o-endda = ls_range.
              APPEND ls_absences_o TO lt_absences_o.
            ENDIF.

          ENDLOOP.

          IF lv_ins EQ abap_false.

            CLEAR lt_range.

*      "Tabela de datas dentro do intervalo que envio/preciso
            CALL FUNCTION 'J_1B_GET_DATE_RANGE_DDA'
              EXPORTING
                date_from  = iv_begda
                date_to    = iv_endda     "ESTE ENDDA ESTÁ A SER ALTERADO DE ALGUMA FORMA E NÃO DEVE SER
              TABLES
                date_range = lt_range.

*      "Calcular novo range de datas para o periodo eliminado
            LOOP AT lt_range INTO ls_range.
              IF ls_range+6(2) NE '01'.
                "Valida dia anterior para saber se já existe
                CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
                  EXPORTING
                    date      = ls_range
                    days      = 1
                    signum    = '-'
                    months    = 0
                    years     = 0
                  IMPORTING
                    calc_date = lv_prev_day.
              ELSE.
                lv_prev_day = ls_range.
              ENDIF.

              READ TABLE lt_absences_o ASSIGNING <fs_absences> WITH KEY pernr = iv_employee_number
                                                                                      subty = iv_subty
                                                                                      endda = lv_prev_day.

              "SE SIM -> Atualizo o endda
              IF sy-subrc EQ 0.
                <fs_absences>-endda = ls_range.
                lv_ins = abap_true.

                "SE NÃO -> Adiciono à tabela com informação do <fs_t_table> e altero as datas
              ELSE.
                MOVE-CORRESPONDING <fs_s_table> TO ls_absences_o.
                ls_absences_o-begda = ls_range.
                ls_absences_o-endda = ls_range.
                APPEND ls_absences_o TO lt_absences_o.
                lv_ins = abap_true.
              ENDIF.
            ENDLOOP.
            CLEAR lt_range.
          ENDIF.
        ENDLOOP.

        "Para cada linha de periodos diferentes, calculados anteriormente -> Chamar o infotype_operation para integrar em SAP
        LOOP AT lt_absences_o INTO ls_absences_o.

          <fs_rec_begda> = ls_absences_o-begda.
          <fs_rec_endda> = ls_absences_o-endda.

          zcl_hcm_ws_wfm=>infotype_operation(
            EXPORTING
              iv_infotype        = iv_infotype                 " Infotipo
              iv_employee_number = iv_employee_number                " Nº pessoal
              iv_subty           = iv_subty                 " Subinfotipo
              iv_begda           = ls_absences_o-begda                 " Início da validade
              iv_endda           = ls_absences_o-endda                 " Fim da validade
              iv_operation       = 'INS'                  " Operação em infotipos
              iv_commit          = iv_commit
              is_record          = cs_record
            IMPORTING
              es_return          = ls_return                 " Parâmetro de retorno
              es_key             = ls_key ).                 " Chave para dados mestre HR

*        "Como o ls_return vai ser da ultima operação -> Temos de definir uma mensagem caso dê erro
*        " SENÃO -> Vamos ter mensagem de erro de DEL para uma operação de INS
*          IF ls_ret_aux-type EQ 'E'.
*            ls_return-type = ls_ret_aux-type.
*            ls_return-id = ls_ret_aux-id.
*            ls_return-number = ls_ret_aux-number.
*          ENDIF.

          MOVE ls_return TO es_return.
          MOVE ls_key TO es_key.

        ENDLOOP.
      ELSE.

        zcl_hcm_ws_wfm=>infotype_operation(
             EXPORTING
               iv_infotype        = iv_infotype                 " Infotipo
               iv_employee_number = iv_employee_number                " Nº pessoal
               iv_subty           = iv_subty                 " Subinfotipo
               iv_begda           = iv_begda                 " Início da validade
               iv_endda           = iv_endda                 " Fim da validade
               iv_operation       = 'INS'                " Operação em infotipos
               iv_commit          = iv_commit
               is_record          = cs_record
             IMPORTING
               es_return          = ls_return                 " Parâmetro de retorno
               es_key             = ls_key ).                 " Chave para dados mestre HR

        MOVE ls_return TO es_return.
        MOVE ls_key TO es_key.
      ENDIF.
    ELSE.

      zcl_hcm_ws_wfm=>infotype_operation(
           EXPORTING
             iv_infotype        = iv_infotype                 " Infotipo
             iv_employee_number = iv_employee_number                " Nº pessoal
             iv_subty           = iv_subty                 " Subinfotipo
             iv_begda           = iv_begda                 " Início da validade
             iv_endda           = iv_endda                 " Fim da validade
             iv_operation       = 'INS'                " Operação em infotipos
             iv_commit          = iv_commit
             is_record          = cs_record
           IMPORTING
             es_return          = ls_return                 " Parâmetro de retorno
             es_key             = ls_key ).                 " Chave para dados mestre HR

      MOVE ls_return TO es_return.
      MOVE ls_key TO es_key.
    ENDIF.

  ENDMETHOD.


  METHOD initialize_interface.
    DATA: lo_fluxo        TYPE REF TO zcl_hcm_ws_wfm,

          ls_input        TYPE zhcm_s_i_resend,

          lv_error        TYPE zzca_statws,
          lv_backup_error TYPE zzca_statws,
          lv_output_error TYPE zzca_statws,
          lv_flag         TYPE abap_bool,
          lv_confirm      TYPE string,
          lt_integ        TYPE zttwfm_hcm_integ,
          lv_response     TYPE string,

          lr_pernr        TYPE RANGE OF pernr,

          lv_status       TYPE numc5.
***********************************************************************************
    DATA: lv_data     TYPE REF TO data,
          lv_type     TYPE string,
          structdescr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <fs> TYPE any.

***********************************************************************************

*-->Registo de Interface
    CREATE OBJECT lo_fluxo
      EXPORTING
        iv_id_if = iv_id.                 " Id interface

    ASSIGN COMPONENT 'TEST' OF STRUCTURE is_input TO FIELD-SYMBOL(<fs_test>).
    lo_fluxo->gv_test = <fs_test>.

    CALL METHOD lo_fluxo->set_info_in
      EXPORTING
        is_info = is_input.

    CALL METHOD lo_fluxo->begin_interface
      EXCEPTIONS
        not_ative           = 1
        if_config_missing   = 2
        error_log           = 3
        serealization_error = 4
        number_range_error  = 5
        OTHERS              = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.
*<--Registo de Interface

    IF iv_method IS NOT INITIAL.
      SPLIT iv_method AT '_' INTO TABLE DATA(lt_split).
      DATA(lv_api) = lt_split[ 2 ].

* Evitar log de Resend -> Lista de ID's que não estão integrados

** Filtrar PERNR
*      SELECT id
*        FROM ztwfm_hcm_integ
*        INTO TABLE ls_input-records_id
*        WHERE api = lv_api
**          AND pernr in lr_pernr
**          AND begda ge iv_date
*          AND integrated = abap_false.
*
*      IF sy-subrc = 0.
** Record_Id's - Remover os zeros à esquerda
*        LOOP AT ls_input-records_id ASSIGNING FIELD-SYMBOL(<fs_record_id>).
*          SHIFT <fs_record_id> LEFT DELETING LEADING '0'.
*        ENDLOOP.
*
** PUT -> Resend
*        CALL METHOD lo_fluxo->request
*          EXPORTING
*            is_input    = ls_input
*            io_fluxo    = lo_fluxo
*            iv_url      = '/Integrations/api/IntegrationOut/Resend'
*            iv_request  = 'PUT'
*          IMPORTING
*            ev_response = lv_response
*          CHANGING
*            cs_output   = cs_output
*            cv_error    = lv_error.
*      ENDIF.
* Evitar log de Resend -> Lista de ID's que não estão integrados

      CLEAR: lv_error, lv_response, cs_output.
    ENDIF.

* 1 - DEFINIR UMA ESTRUTURA (TYPE ANY) = CS_OUTPUT

    structdescr ?= cl_abap_structdescr=>describe_by_data( cs_output ).

    DATA(typedescr) = structdescr->absolute_name.
    SPLIT typedescr AT '=' INTO TABLE DATA(lt_aux).
    lv_type = lt_aux[ 2 ].

    CREATE DATA lv_data TYPE (lv_type).
    ASSIGN lv_data->* TO <fs>.

    CASE iv_request.
      WHEN 'POST'.
        DO 2 TIMES.

          CALL METHOD lo_fluxo->request_status
            EXPORTING
              is_input    = is_input
              io_fluxo    = lo_fluxo                 " Classe Interface wfm
              iv_url      = iv_url                   " GET
              iv_request  = iv_request               " ABSENCE/WAGES,etc
            IMPORTING
              ev_response = lv_response
            CHANGING
              cs_output   = cs_output
              cv_error    = lv_error
              cv_status   = lv_status.

*<--Seleção de dados
*Tratamento dos dados do iv_method(GET_ABSENCE_O ; GET_WAGES_O, etc)
          IF iv_method IS NOT INITIAL.
            CALL METHOD lo_fluxo->(iv_method)
              EXPORTING
                io_fluxo    = lo_fluxo
                iv_response = lv_response
              CHANGING
                cs_output   = cs_output
                cv_error    = lv_error
                cv_flag     = lv_flag
                cv_confirm  = lv_confirm.




            IF lv_error EQ 'E'.
              lv_backup_error = lv_error.
*          lv_output_error = lv_error.
            ENDIF.
          ENDIF.

*<- APP - 14.11.2023
*>- APP - 14.11.2023
          CASE lv_status.
            WHEN 200.
              EXIT.
            WHEN 415.
              CLEAR: cs_output, lv_error.
            WHEN OTHERS.
          ENDCASE.
        ENDDO.

      WHEN 'GET'.
        WHILE lv_status NE 204.
          CALL METHOD lo_fluxo->request_status
            EXPORTING
              is_input    = is_input
              io_fluxo    = lo_fluxo                 " Classe Interface wfm
              iv_url      = iv_url                   " GET
              iv_request  = iv_request               " ABSENCE/WAGES,etc
            IMPORTING
              ev_response = lv_response
            CHANGING
              cs_output   = cs_output
              cv_error    = lv_error
              cv_status   = lv_status.

          IF lv_status EQ 204.
            IF lv_backup_error IS NOT INITIAL.
              lv_error = lv_backup_error.
            ENDIF.
            "Tem de ser um FS que permita alterar o cs_output (E-No content -> S-OK, se for esse o caso)
            EXIT.
          ENDIF.

*<--Seleção de dados
*Tratamento dos dados do iv_method(GET_ABSENCE_O ; GET_WAGES_O, etc)
          IF iv_method IS NOT INITIAL.
            CALL METHOD lo_fluxo->(iv_method)
              EXPORTING
                io_fluxo    = lo_fluxo
                iv_response = lv_response
              CHANGING
                cs_output   = cs_output
                cv_error    = lv_error
                cv_flag     = lv_flag
                cv_confirm  = lv_confirm.

            IF lv_status EQ 200.
              IF lv_error EQ 'E'.
                lv_backup_error = lv_error.
*          lv_output_error = lv_error.
              ENDIF.
            ENDIF.

* 2 - FAZER APPEND DA CS_OUTPUT PARA A NOVA ESTRUTURA (TYPE ANY)
            "Adicionar try catch
            CALL METHOD lo_fluxo->adiciona_output
              EXPORTING
                iv_method = iv_method
                is_output = cs_output
              CHANGING
                cs_output = <fs>.
          ENDIF.
        ENDWHILE.

* 3 - ANTES DE CHAMAR O FINAL INTERFACE -> CS_OUTPUT = ESTRUTURA (TYPE ANY)
        "Adicionar try catch
        ASSIGN cs_output TO FIELD-SYMBOL(<fs_output>).
        <fs_output> = <fs>.

        IF lv_backup_error IS INITIAL.
          lv_error = 'S'.
        ENDIF.
    ENDCASE.

    "Envia novo resend com a confirmação, caso não exista nenhuma linha com integrated = abap_false
*    IF lv_flag IS NOT INITIAL.
*      lo_fluxo->delivery_confirmation(
*        EXPORTING
*          iv_conf_id = lv_confirm
*          io_fluxo   = lo_fluxo                 " Classe Interface wfm
*      ).
*    ENDIF.

*-->Registo Final de Interface
    CALL METHOD lo_fluxo->set_info_out
      EXPORTING
        is_info = cs_output.

    CALL METHOD lo_fluxo->set_if_stat
      EXPORTING
        iv_stat = lv_error.

    CALL METHOD lo_fluxo->end_interface
      EXCEPTIONS
        error_log           = 1
        serealization_error = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.
*<--Registo Final de Interface
  ENDMETHOD.


  METHOD request.
    DATA: lt_fields      TYPE tihttpnvp,

          ls_msg         TYPE bal_s_msg,

          lv_data        TYPE string,
          lv_status_code TYPE i.

    APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).
    <fs_fields>-name = 'Tlan-Bff-name'.
    <fs_fields>-value = 'WFMINTEGRATION'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Tlan-Bff-enrolment-id'.
    <fs_fields>-value = 'd7771511d69848cb08db28785de57ef6cf1dbedf'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Tlan-Bff-channel'.
    <fs_fields>-value = 'EXT_APIS'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Tlan-Bff-culture-code'.
    <fs_fields>-value = 'pt-PT'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Tlan-Bff-app-os'.
    <fs_fields>-value = 'hd2kLym9pM4tNXv8Byfu7R7rrUyB2uQt'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Authorization'.

    io_fluxo->get_token(
      IMPORTING
        ev_token = <fs_fields>-value
    ).

    CONCATENATE 'Bearer ' <fs_fields>-value INTO <fs_fields>-value RESPECTING BLANKS.

    IF is_input IS NOT INITIAL.
      /ui2/cl_json=>serialize(
        EXPORTING
          data             = is_input
          pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
        RECEIVING
          r_json           = lv_data
      ).

      REPLACE ALL OCCURRENCES OF ',"test":"X"' in lv_data WITH space.
      REPLACE ALL OCCURRENCES OF ',"test":""' in lv_data WITH space.
      REPLACE ALL OCCURRENCES OF '"test":"X"' in lv_data WITH space.
      REPLACE ALL OCCURRENCES OF '"test":""' in lv_data WITH space.
      REPLACE ALL OCCURRENCES OF '""' IN lv_data WITH 'null'.

      IF strlen( lv_data ) = 2.
        CLEAR lv_data.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT 'OPERATION_INFO' OF STRUCTURE cs_output TO FIELD-SYMBOL(<fs_operation_info>).

    io_fluxo->http_request(
      EXPORTING
        iv_url            = io_fluxo->gv_domain && iv_url
        iv_request        = iv_request
        iv_data           = lv_data
        it_fields         = lt_fields
      IMPORTING
        ev_status_code    = lv_status_code
        ev_operation_info = <fs_operation_info>
        ev_response       = ev_response
    ).

    ASSIGN COMPONENT 'STATUS_CODE' OF STRUCTURE cs_output TO FIELD-SYMBOL(<fs_status_code>).
    SPLIT iv_url AT '/' INTO TABLE DATA(lt_data).
    ls_msg-msgid = 'ZWFM'.
    ls_msg-msgv1 = lt_data[ lines( lt_data ) ].

    CASE lv_status_code.
      WHEN 200.
        ls_msg-msgty = cv_error = <fs_status_code> = 'S'.
        ls_msg-msgno = '001'.
      WHEN OTHERS.
        ls_msg-msgty = cv_error = <fs_status_code> = 'E'.
        ls_msg-msgno = '002'.
    ENDCASE.

    SPLIT ev_response AT '"message":["' INTO TABLE DATA(lt_split).

    IF lines( lt_split ) > 1.
      SPLIT lt_split[ 2 ] AT '"' INTO TABLE lt_split.
    ELSE.
      CLEAR lt_split.
    ENDIF.

    LOOP AT lt_split INTO DATA(ls_split).
      IF strlen( ls_split ) > 3.
        io_fluxo->add_text_log(
          EXPORTING
            iv_text  = ls_split                 " Mensagem
            iv_msgty = ls_msg-msgty                 " Tipo de mensagem
        ).
      ENDIF.
    ENDLOOP.

    io_fluxo->add_msg_log(
      EXPORTING
        us_msg    = ls_msg                 " Log de aplicação: dados de uma mensagem
      EXCEPTIONS
        error_log = 1                " Erro em registo de Log
        OTHERS    = 2
    ).
  ENDMETHOD.


  METHOD request_status.
    DATA: lt_fields      TYPE tihttpnvp,

          ls_msg         TYPE bal_s_msg,

          lv_data        TYPE string,
          lv_status_code TYPE i.

    APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).
    <fs_fields>-name = 'Tlan-Bff-name'.
    <fs_fields>-value = 'WFMINTEGRATION'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Tlan-Bff-enrolment-id'.
    <fs_fields>-value = 'd7771511d69848cb08db28785de57ef6cf1dbedf'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Tlan-Bff-channel'.
    <fs_fields>-value = 'EXT_APIS'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Tlan-Bff-culture-code'.
    <fs_fields>-value = 'pt-PT'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Tlan-Bff-app-os'.
    <fs_fields>-value = 'hd2kLym9pM4tNXv8Byfu7R7rrUyB2uQt'.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <fs_fields>.
    <fs_fields>-name  = 'Authorization'.

    io_fluxo->get_token(
      IMPORTING
        ev_token = <fs_fields>-value
    ).

    CONCATENATE 'Bearer ' <fs_fields>-value INTO <fs_fields>-value RESPECTING BLANKS.

    IF is_input IS NOT INITIAL.
      /ui2/cl_json=>serialize(
        EXPORTING
          data             = is_input
          pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
        RECEIVING
          r_json           = lv_data
      ).

      REPLACE ALL OCCURRENCES OF ',"test":"X"' IN lv_data WITH space.
      REPLACE ALL OCCURRENCES OF ',"test":""' IN lv_data WITH space.
      REPLACE ALL OCCURRENCES OF '"test":"X"' IN lv_data WITH space.
      REPLACE ALL OCCURRENCES OF '"test":""' IN lv_data WITH space.
      REPLACE ALL OCCURRENCES OF '""' IN lv_data WITH 'null'.

      IF strlen( lv_data ) = 2.
        CLEAR lv_data.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT 'OPERATION_INFO' OF STRUCTURE cs_output TO FIELD-SYMBOL(<fs_operation_info>).

    io_fluxo->http_request(
      EXPORTING
        iv_url            = io_fluxo->gv_domain && iv_url
        iv_request        = iv_request
        iv_data           = lv_data
        it_fields         = lt_fields
      IMPORTING
        ev_status_code    = lv_status_code
        ev_operation_info = <fs_operation_info>
        ev_response       = ev_response
    ).

    ASSIGN COMPONENT 'STATUS_CODE' OF STRUCTURE cs_output TO FIELD-SYMBOL(<fs_status_code>).
    SPLIT iv_url AT '/' INTO TABLE DATA(lt_data).
    ls_msg-msgid = 'ZWFM'.
    ls_msg-msgv1 = lt_data[ lines( lt_data ) ].

    cv_status = lv_status_code.

    CASE lv_status_code.
      WHEN 200.
        ls_msg-msgty = cv_error = <fs_status_code> = 'S'.
        ls_msg-msgno = '001'.
      WHEN OTHERS.
        ls_msg-msgty = cv_error = <fs_status_code> = 'E'.
        ls_msg-msgno = '002'.
    ENDCASE.

    SPLIT ev_response AT '"message":["' INTO TABLE DATA(lt_split).

    IF lines( lt_split ) > 1.
      SPLIT lt_split[ 2 ] AT '"' INTO TABLE lt_split.
    ELSE.
      CLEAR lt_split.
    ENDIF.

    LOOP AT lt_split INTO DATA(ls_split).
      IF strlen( ls_split ) > 3.
        io_fluxo->add_text_log(
          EXPORTING
            iv_text  = ls_split                 " Mensagem
            iv_msgty = ls_msg-msgty                 " Tipo de mensagem
        ).
      ENDIF.
    ENDLOOP.

    io_fluxo->add_msg_log(
      EXPORTING
        us_msg    = ls_msg                 " Log de aplicação: dados de uma mensagem
      EXCEPTIONS
        error_log = 1                " Erro em registo de Log
        OTHERS    = 2
    ).
  ENDMETHOD.


  METHOD set_info_in.
    CREATE DATA gs_info_in LIKE is_info.
    ASSIGN gs_info_in->* to FIELD-SYMBOL(<fs_info_in>).
    <fs_info_in> = is_info.
    go_obj_ref = me.
  ENDMETHOD.


  method SET_INFO_OUT.
    CREATE DATA gs_info_out LIKE is_info.
    ASSIGN gs_info_out->* to FIELD-SYMBOL(<fs_info_out>).
    <fs_info_out> = is_info.
    go_obj_ref = me.
  endmethod.
ENDCLASS.

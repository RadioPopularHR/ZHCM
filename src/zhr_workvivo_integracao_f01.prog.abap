*&---------------------------------------------------------------------*
*&  Include           ZHR_WORKVIVO_INTEGRACAO_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_INITIALIZATION
*&---------------------------------------------------------------------*
FORM f_initialization .

  SELECT SINGLE low
    FROM tvarvc
    INTO gv_bearer
    WHERE name EQ 'ZHR_WORKVIVO_BEARER'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_WORKVIVO_INFO
*&---------------------------------------------------------------------*
FORM f_workvivo_info.

  DATA: lv_text            TYPE string,
        lv_first_word(100) TYPE c,
        lv_the_rest(100)   TYPE c,
        lt_pre_record      TYPE hrpad_prelp_tab,
        is_exist           TYPE boole_d,
        is_missing_auth    TYPE boole_d,
        lt_p9010           TYPE TABLE OF p9010,
        lt_p0000           TYPE TABLE OF p0000,
        lt_p0001           TYPE TABLE OF p0001,
        lt_p0002           TYPE TABLE OF p0002,
        lt_p0016           TYPE TABLE OF p0016,
        lt_p0302           TYPE TABLE OF p0302.

  CLEAR gs_workvivo.

  PERFORM infty_reader USING peras-pernr
                             '9010'
                             space
                             pn-begda
                             pn-endda
                    CHANGING lt_pre_record
                             is_exist
                             is_missing_auth.

  IF is_exist IS NOT INITIAL.
    cl_hr_pnnnn_type_cast=>prelp_to_pnnnn_tab(
         EXPORTING
           prelp_tab = lt_pre_record
         IMPORTING
           pnnnn_tab = lt_p9010
       ).
  ENDIF.

  IF lt_p9010 IS NOT INITIAL.
    SORT lt_p9010 BY endda DESCENDING.
    READ TABLE lt_p9010 INTO gs_p9010 INDEX 1.
    gs_workvivo-pernr      = gs_p9010-pernr.
    gs_workvivo-idchefia   = gs_p9010-idchefia.
    gs_workvivo-email      = gs_p9010-email.
    gs_workvivo-id_externo = gs_p9010-idworkvivo.
    gs_workvivo-ativo      = gs_p9010-ativo.

    PERFORM infty_reader USING gs_p9010-pernr
                               '0016'
                               space
                               pn-begda
                               pn-endda
                      CHANGING lt_pre_record
                               is_exist
                               is_missing_auth.

    IF is_exist IS NOT INITIAL.
      cl_hr_pnnnn_type_cast=>prelp_to_pnnnn_tab(
           EXPORTING
             prelp_tab = lt_pre_record
           IMPORTING
             pnnnn_tab = lt_p0016
         ).
    ENDIF.

    SORT lt_p0016 BY begda DESCENDING.
    READ TABLE lt_p0016 INTO DATA(ls_p0016) INDEX 1.
    gs_workvivo-admissao = ls_p0016-eindt.

    PERFORM infty_reader USING gs_p9010-pernr
                               '0000'
                               space
                               '19000101'
                               '99991231'
                      CHANGING lt_pre_record
                               is_exist
                               is_missing_auth.

    IF is_exist IS NOT INITIAL.
      cl_hr_pnnnn_type_cast=>prelp_to_pnnnn_tab(
           EXPORTING
             prelp_tab = lt_pre_record
           IMPORTING
             pnnnn_tab = lt_p0000
         ).
    ENDIF.

    SORT lt_p0000 BY begda DESCENDING.
    READ TABLE lt_p0000[] INTO DATA(ls_0000) INDEX 1.
    IF ls_0000-massn EQ 'Z4'.
      CLEAR gs_workvivo-ativo.
    ENDIF.

    SORT lt_p0000 BY begda ASCENDING.
    READ TABLE lt_p0000[] INTO ls_0000 INDEX 1.

    IF gs_workvivo-admissao IS INITIAL.

      READ TABLE lt_p0000[] INTO DATA(ls_0000_aux) WITH KEY massn = 'Z3'.
      IF sy-subrc IS NOT INITIAL.
        READ TABLE lt_p0000[] INTO ls_0000_aux WITH KEY massn = 'Z1'.
        IF sy-subrc IS NOT INITIAL.
          READ TABLE lt_p0000[] INTO ls_0000_aux INDEX 1.
        ENDIF.
      ENDIF.
      ls_0000 = ls_0000_aux.

      PERFORM infty_reader USING gs_p9010-pernr
                                 '0302'
                                 space
                                 ls_0000-begda
                                 ls_0000-endda
                        CHANGING lt_pre_record
                                 is_exist
                                 is_missing_auth.

      IF is_exist IS NOT INITIAL.
        cl_hr_pnnnn_type_cast=>prelp_to_pnnnn_tab(
             EXPORTING
               prelp_tab = lt_pre_record
             IMPORTING
               pnnnn_tab = lt_p0302
           ).
      ENDIF.

      SORT lt_p0302 BY begda DESCENDING.
      READ TABLE lt_p0302 INTO DATA(ls_p0302) INDEX 1.
      gs_workvivo-admissao = ls_p0302-begda.
    ENDIF.

    PERFORM infty_reader USING gs_p9010-pernr
                               '0001'
                               space
                               ls_0000-begda
                               ls_0000-endda
                      CHANGING lt_pre_record
                               is_exist
                               is_missing_auth.

    IF is_exist IS NOT INITIAL.
      cl_hr_pnnnn_type_cast=>prelp_to_pnnnn_tab(
           EXPORTING
             prelp_tab = lt_pre_record
           IMPORTING
             pnnnn_tab = lt_p0001
         ).
    ENDIF.

    SORT lt_p0001 BY begda DESCENDING.
    READ TABLE lt_p0001 INTO DATA(ls_p0001) INDEX 1.
    gs_workvivo-orgeh = ls_p0001-orgeh.

    PERFORM infty_reader USING gs_p9010-pernr
                               '0002'
                               space
                               ls_0000-begda
                               ls_0000-endda
                      CHANGING lt_pre_record
                               is_exist
                               is_missing_auth.

    IF is_exist IS NOT INITIAL.
      cl_hr_pnnnn_type_cast=>prelp_to_pnnnn_tab(
           EXPORTING
             prelp_tab = lt_pre_record
           IMPORTING
             pnnnn_tab = lt_p0002
         ).
    ENDIF.

    SORT lt_p0002 BY begda DESCENDING.
    READ TABLE lt_p0002 INTO DATA(ls_p0002) INDEX 1.
    gs_workvivo-gbdat = ls_p0002-gbdat.
    lv_text = ls_p0002-cname. gs_workvivo-cname = cl_hrpayus_format_string=>conv_first_chars_to_upper_case( lv_text ).
    lv_the_rest = ls_p0002-vorna.
    CALL FUNCTION 'STPU1_FIRST_WORD'
      EXPORTING
        my_string  = lv_the_rest
      IMPORTING
        first_word = lv_first_word
        the_rest   = lv_the_rest.
    lv_text = lv_first_word. gs_workvivo-vorna = cl_hrpayus_format_string=>conv_first_chars_to_upper_case( lv_text ).
    lv_the_rest = ls_p0002-nachn.
    WHILE lv_the_rest IS NOT INITIAL.
      CALL FUNCTION 'STPU1_FIRST_WORD'
        EXPORTING
          my_string  = lv_the_rest
        IMPORTING
          first_word = lv_first_word
          the_rest   = lv_the_rest.
    ENDWHILE.
    lv_text = lv_first_word. gs_workvivo-nachn = cl_hrpayus_format_string=>conv_first_chars_to_upper_case( lv_text ).

    SELECT *
      FROM zhr_centro_custo
      INTO TABLE @DATA(lt_centro_custo)
      WHERE kostl EQ @gs_p9010-centro_cst.

    IF sy-subrc IS INITIAL.
      SORT lt_centro_custo BY endda DESCENDING.
      LOOP AT lt_centro_custo INTO DATA(ls_centro_custo).
        IF ( ( gs_p9010-begda BETWEEN ls_centro_custo-begda AND ls_centro_custo-endda ) AND
             ( gs_p9010-endda BETWEEN ls_centro_custo-begda AND ls_centro_custo-endda ) ) OR
           ( ( ls_centro_custo-begda BETWEEN gs_p9010-begda AND gs_p9010-endda ) AND
             ( ls_centro_custo-endda BETWEEN gs_p9010-begda AND gs_p9010-endda ) ).
          gs_workvivo-centro = ls_centro_custo-descricao.
          CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SELECT *
      FROM zhr_subarea
      INTO TABLE @DATA(lt_subarea)
      WHERE btrtl EQ @gs_p9010-subarea.

    IF sy-subrc IS INITIAL.
      SORT lt_subarea BY endda DESCENDING.
      LOOP AT lt_subarea INTO DATA(ls_subarea).
        IF ( ( gs_p9010-begda BETWEEN ls_subarea-begda AND ls_subarea-endda ) AND
             ( gs_p9010-endda BETWEEN ls_subarea-begda AND ls_subarea-endda ) ) OR
           ( ( ls_subarea-begda BETWEEN gs_p9010-begda AND gs_p9010-endda ) AND
             ( ls_subarea-endda BETWEEN gs_p9010-begda AND gs_p9010-endda ) ).
          gs_workvivo-subarea = ls_subarea-descricao.
          CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SELECT e~endda, e~begda, t~descricao
      INTO TABLE @DATA(lt_equipa)
      FROM zhr_des_equipa_t AS t
      INNER JOIN zhr_des_equipa AS e
      ON t~codigo EQ e~codigo
      AND t~endda EQ e~endda
      WHERE t~codigo EQ @gs_p9010-equipa.

    IF sy-subrc IS INITIAL.
      SORT lt_equipa BY endda DESCENDING.
      LOOP AT lt_equipa INTO DATA(ls_equipa).
        IF ( ( gs_p9010-begda BETWEEN ls_equipa-begda AND ls_equipa-endda ) AND
             ( gs_p9010-endda BETWEEN ls_equipa-begda AND ls_equipa-endda ) ) OR
           ( ( ls_equipa-begda BETWEEN gs_p9010-begda AND gs_p9010-endda ) AND
             ( ls_equipa-endda BETWEEN gs_p9010-begda AND gs_p9010-endda ) ).
          gs_workvivo-equipa = ls_equipa-descricao.
          CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SELECT *
      FROM zhr_funcao
      INTO TABLE @DATA(lt_funcao)
      WHERE plans EQ @gs_p9010-titulo_trabalho.

    IF sy-subrc IS INITIAL.
      SORT lt_funcao BY endda DESCENDING.
      LOOP AT lt_funcao INTO DATA(ls_funcao).
        IF ( ( gs_p9010-begda BETWEEN ls_funcao-begda AND ls_funcao-endda ) AND
             ( gs_p9010-endda BETWEEN ls_funcao-begda AND ls_funcao-endda ) ) OR
           ( ( ls_funcao-begda BETWEEN gs_p9010-begda AND gs_p9010-endda ) AND
             ( ls_funcao-endda BETWEEN gs_p9010-begda AND gs_p9010-endda ) ).
          gs_workvivo-funcao = ls_funcao-descricao.
          CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.

    gs_workvivo-json = icon_protocol.

    PERFORM f_envia_workvivo.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ENVIA_WORKVIVO
*&---------------------------------------------------------------------*
FORM f_envia_workvivo .

  CONSTANTS: c_true(4)  TYPE c VALUE 'true',
             c_false(5) TYPE c VALUE 'false',
             c_uri      TYPE string VALUE '/scim/v2/scim/Users'.

  DATA: lv_date(10)         TYPE c,
        lv_lastmodified(10) TYPE c,
        lv_pos_i            TYPE i,
        lv_pos_f            TYPE i,
        lv_name(100)        TYPE c,

        lt_header_fields    TYPE tihttpnvp,
        mv_response         TYPE string,
        mo_http_client      TYPE REF TO if_http_client,
        lv_json             TYPE /ui2/cl_json=>json,
        lo_response         TYPE REF TO if_rest_entity,
        lv_bearer           TYPE ihttpval.

  CALL METHOD cl_http_client=>create_by_destination
    EXPORTING
      destination              = 'ZHR_WORKVIVO'
    IMPORTING
      client                   = mo_http_client
    EXCEPTIONS
      argument_not_found       = 1 " Connection Parameter (Destination) Not Available
      destination_not_found    = 2 " Destination not found
      destination_no_authority = 3 " No Authorization to Use HTTP Destination
      plugin_not_active        = 4 " HTTP/HTTPS communication not available
      internal_error           = 5 " Internal error (e.g. name too long)
      OTHERS                   = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          INTO gs_workvivo-msg
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    gs_workvivo-status = icon_red_light.
  ELSE.

    CONCATENATE 'Bearer' gv_bearer INTO lv_bearer SEPARATED BY space.
    APPEND VALUE #( name = 'Content-Type' value = 'application/json' ) TO lt_header_fields.
    APPEND VALUE #( name = 'accept' value = 'application/json' ) TO lt_header_fields.
    APPEND VALUE #( name = 'Authorization' value = lv_bearer ) TO lt_header_fields.

    mo_http_client->request->set_header_fields( fields = lt_header_fields ).
    mo_http_client->request->set_method( if_http_request=>co_request_method_post ) .

    IF gs_workvivo-equipa IS INITIAL.
      CONCATENATE lv_json
      '{"schemas":["urn:ietf:params:scim:schemas:core:2.0:User","urn:ietf:params:scim:schemas:extension:enterprise:2.0:User"],"id":"&id","externalId":"&externalId","userName":"&userName","name":{"formatted":"&formatted",'
      '"familyName":"&familyName","givenName":"&givenName"},"emails":[{"value": "&email","type": "work","primary": true}],"phoneNumbers":[],"addresses":[],"title":"&title","active":&active,"meta":{"resourceType":"User",'
      '"created":"&created","lastModified":"&lastModified","version":"1","location":"https:\/\/radiopopular.workvivo.com\/okta\/v2\/scim\/Users\/&id"},'
      '"urn:ietf:params:scim:schemas:extension:enterprise:2.0:User":{"department":null,"authRoles":[],"manager":{"displayName": "&managerName","value": "&managerValue" }, '
      '"taxonomies":[{"displayName": "&orgeh","type": "Organization Unit"},{"displayName": "&subarea","type": "Subarea"},'
      '{"displayName": "&area","type": "Area"}],"hireDate":"&hireDateT00:00:00Z","dateOfBirth":"&dateOfBirthT00:00:00Z"}}'
      INTO lv_json.
    ELSE.
      CONCATENATE lv_json
      '{"schemas":["urn:ietf:params:scim:schemas:core:2.0:User","urn:ietf:params:scim:schemas:extension:enterprise:2.0:User"],"id":"&id","externalId":"&externalId","userName":"&userName","name":{"formatted":"&formatted",'
      '"familyName":"&familyName","givenName":"&givenName"},"emails":[{"value": "&email","type": "work","primary": true}],"phoneNumbers":[],"addresses":[],"title":"&title","active":&active,"meta":{"resourceType":"User",'
      '"created":"&created","lastModified":"&lastModified","version":"1","location":"https:\/\/radiopopular.workvivo.com\/okta\/v2\/scim\/Users\/&id"},'
      '"urn:ietf:params:scim:schemas:extension:enterprise:2.0:User":{"department":null,"authRoles":[],"manager":{"displayName": "&managerName","value": "&managerValue" }, '
      '"taxonomies":[{"displayName": "&orgeh","type": "Organization Unit"},{"displayName": "&subarea","type": "Subarea"},'
      '{"displayName": "&area","type": "Area"},{"displayName": "&team","type": "Equipa"}],"hireDate":"&hireDateT00:00:00Z","dateOfBirth":"&dateOfBirthT00:00:00Z"}}'
      INTO lv_json.
    ENDIF.

    REPLACE ALL OCCURRENCES OF '&id'           IN lv_json WITH space.
    DATA(lv_externalid) = gs_workvivo-email.
    TRANSLATE lv_externalid TO UPPER CASE. "03/07/2024
    REPLACE ALL OCCURRENCES OF '.PT' IN lv_externalid WITH space.
    REPLACE ALL OCCURRENCES OF '&externalId'   IN lv_json WITH lv_externalid. "03/07/2024
    REPLACE ALL OCCURRENCES OF '&userName'     IN lv_json WITH gs_workvivo-email.
    REPLACE ALL OCCURRENCES OF '&formatted'    IN lv_json WITH gs_workvivo-cname.
    REPLACE ALL OCCURRENCES OF '&familyName'   IN lv_json WITH gs_workvivo-nachn.
    REPLACE ALL OCCURRENCES OF '&givenName'    IN lv_json WITH gs_workvivo-vorna.
    CONCATENATE gs_workvivo-vorna gs_workvivo-nachn INTO lv_name SEPARATED BY space.
    REPLACE ALL OCCURRENCES OF '&displayName'  IN lv_json WITH lv_name.
    REPLACE ALL OCCURRENCES OF '&email'        IN lv_json WITH gs_workvivo-email.
    REPLACE ALL OCCURRENCES OF '&title'        IN lv_json WITH gs_workvivo-funcao. "03/07/2024
    CASE gs_workvivo-ativo.
      WHEN abap_true. REPLACE ALL OCCURRENCES OF '&active' IN lv_json WITH c_true.
      WHEN OTHERS.    REPLACE ALL OCCURRENCES OF '&active' IN lv_json WITH c_false.
    ENDCASE.
    REPLACE ALL OCCURRENCES OF '&managerName'  IN lv_json WITH gs_workvivo-nome_chefia. "03/07/2024
    DATA(lv_id_chefia) = gs_workvivo-idchefia.
    TRANSLATE lv_id_chefia TO UPPER CASE.
    REPLACE ALL OCCURRENCES OF '&managerValue' IN lv_json WITH lv_id_chefia. "03/07/2024
    REPLACE ALL OCCURRENCES OF '&orgeh'        IN lv_json WITH gs_workvivo-orgeh.
    REPLACE ALL OCCURRENCES OF '&subarea'      IN lv_json WITH gs_workvivo-centro. "03/07/2024
    REPLACE ALL OCCURRENCES OF '&area'         IN lv_json WITH gs_workvivo-subarea. "03/07/2024
    REPLACE ALL OCCURRENCES OF '&team'         IN lv_json WITH gs_workvivo-equipa.
    CONCATENATE gs_workvivo-admissao(4) gs_workvivo-admissao+4(2) gs_workvivo-admissao+6(2) INTO lv_date SEPARATED BY '-'.
    REPLACE ALL OCCURRENCES OF '&hireDate'     IN lv_json WITH lv_date.
    CONCATENATE gs_workvivo-gbdat(4) gs_workvivo-gbdat+4(2) gs_workvivo-gbdat+6(2) INTO lv_date SEPARATED BY '-'.
    REPLACE ALL OCCURRENCES OF '&dateOfBirth'  IN lv_json WITH lv_date.
    REPLACE ALL OCCURRENCES OF '&created'      IN lv_json WITH space.
    REPLACE ALL OCCURRENCES OF '&lastModified' IN lv_json WITH space.
    lv_json = /ui2/cl_json=>serialize( data = lv_json pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    mo_http_client->request->set_cdata(
      EXPORTING
        data   =   lv_json       ).

    cl_http_utility=>set_request_uri(
    EXPORTING
     request = mo_http_client->request
     uri     = c_uri
    ).

    mo_http_client->send(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3 ).
    mo_http_client->receive(
     EXCEPTIONS
       http_communication_failure = 1
       http_invalid_state         = 2
       http_processing_failed     = 3 ).

    IF sy-subrc IS INITIAL.
      mo_http_client->response->get_status( IMPORTING code   = DATA(lv_code)
                                                      reason = DATA(lv_reason) ).
      IF lv_code EQ '200' OR lv_code EQ '201'.
        gs_workvivo-status = icon_green_light.
      ELSE.
        CALL METHOD mo_http_client->close
          EXCEPTIONS
            http_invalid_state = 1
            OTHERS             = 2.
        IF sy-subrc IS INITIAL.
          DATA: v_code TYPE char3. v_code = lv_code.
          CONCATENATE v_code lv_reason INTO gs_workvivo-msg SEPARATED BY ' - ' RESPECTING BLANKS.
          gs_workvivo-status = icon_red_light.
        ELSE.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                INTO gs_workvivo-msg
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          gs_workvivo-status = icon_red_light.
        ENDIF.
      ENDIF.

      CLEAR mv_response.
      DATA(lo_rest_client) = NEW cl_rest_http_client( io_http_client = mo_http_client  ).

      lo_response = lo_rest_client->if_rest_client~get_response_entity( ).

      mv_response = lo_response->get_string_data( ).

      SEARCH mv_response FOR '"id":"'.
      IF sy-subrc = 0.
        lv_pos_i = sy-fdpos + 7.
        SEARCH mv_response FOR '"' STARTING AT lv_pos_i.
        lv_pos_f = sy-fdpos.
        lv_pos_i = lv_pos_i - 1.
        gs_workvivo-id_externo = mv_response+lv_pos_i(lv_pos_f).
      ENDIF.

      IF gs_workvivo-id_externo IS NOT INITIAL OR gs_workvivo-ativo IS INITIAL.
        PERFORM f_update_idexterno.
      ENDIF.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            INTO gs_workvivo-msg
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      gs_workvivo-status = icon_red_light.
    ENDIF.
  ENDIF.

  APPEND gs_workvivo TO gt_workvivo.
  APPEND INITIAL LINE TO gt_json ASSIGNING FIELD-SYMBOL(<fs_json>).
  <fs_json>-pernr = gs_workvivo-pernr.
  <fs_json>-line = lv_json.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_LOG
*&---------------------------------------------------------------------*
FORM f_exibe_log .

  DATA: lo_cols_tab TYPE REF TO cl_salv_columns_table,
        lo_col_tab  TYPE REF TO cl_salv_column_table,
        lo_events   TYPE REF TO cl_salv_events_table.

  TRY.
      cl_salv_table=>factory(  EXPORTING list_display = sy-batch
                               IMPORTING r_salv_table = DATA(lo_tab)
                               CHANGING  t_table      = gt_workvivo ).

      lo_tab->get_functions( )->set_all( ).
      lo_tab->get_columns( )->set_optimize( ).
      DATA(lo_columns) = lo_tab->get_columns( ).
      DATA(lo_column) = lo_columns->get_column( columnname = 'ATIVO' ).
      lo_column->set_short_text( 'Ativo' ).
      lo_column->set_medium_text( 'Ativo' ).
      lo_column->set_long_text( 'Ativo' ).
      lo_column = lo_columns->get_column( columnname = 'NOME_CHEFIA' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column->set_short_text( 'NmChefia' ).
      lo_column->set_medium_text( 'Nome Chefia' ).
      lo_column->set_long_text( 'Nome Chefia' ).
      lo_column = lo_columns->get_column( columnname = 'JSON' ).
      lo_column->set_short_text( 'JSON' ).
      lo_column->set_medium_text( 'Exibir JSON' ).
      lo_column->set_long_text( 'Exibir JSON' ).
      lo_column = lo_columns->get_column( columnname = 'STATUS' ).
      lo_column->set_short_text( 'Status' ).

      lo_cols_tab = lo_tab->get_columns( ).
      lo_col_tab ?= lo_cols_tab->get_column( 'ATIVO' ).
      lo_col_tab->set_cell_type( value = if_salv_c_cell_type=>checkbox ).
      lo_col_tab ?= lo_cols_tab->get_column( 'JSON' ).
      lo_col_tab->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

      lo_events = lo_tab->get_event( ).

      CREATE OBJECT gr_event_handler.
      SET HANDLER gr_event_handler->on_link_click FOR lo_events.

      lo_tab->display( ).
    CATCH cx_root.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_IDEXTERNO
*&---------------------------------------------------------------------*
FORM f_update_idexterno.

  DATA: ls_return TYPE bapireturn1.

  CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
    EXPORTING
      number = gs_p9010-pernr
    IMPORTING
      return = ls_return.

  IF ls_return-id EQ space.

    gs_p9010-idworkvivo = gs_workvivo-id_externo.
    gs_p9010-ativo      = gs_workvivo-ativo.

    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = '9010'
        number        = gs_p9010-pernr
        subtype       = gs_p9010-subty
        validityend   = gs_p9010-endda
        validitybegin = gs_p9010-begda
        record        = gs_p9010
        operation     = 'MOD'
        dialog_mode   = '0'
      IMPORTING
        return        = ls_return.

    IF ls_return-type NE 'S' AND ls_return-type IS NOT INITIAL.
      APPEND gs_workvivo TO gt_workvivo.
      gs_workvivo-json = icon_protocol.
      gs_workvivo-status = icon_red_light.
      gs_workvivo-msg = 'Erro ao atualizar infotipo 9010'.
      APPEND gs_workvivo TO gt_workvivo.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
          INTO gs_workvivo-msg
          WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
      gs_workvivo-json = icon_protocol.
      gs_workvivo-status = icon_red_light.
    ENDIF.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = gs_p9010-pernr
      IMPORTING
        return = ls_return.
  ELSE.

    APPEND gs_workvivo TO gt_workvivo.
    MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
        INTO gs_workvivo-msg
        WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
    gs_workvivo-json = icon_protocol.
    gs_workvivo-status = icon_red_light.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INFTY_READER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM infty_reader  USING    p_pernr TYPE pernr_d
                            p_infty TYPE infty
                            p_subty TYPE subty
                            p_begda TYPE begda
                            p_endda TYPE endda
                   CHANGING lt_pre_record TYPE hrpad_prelp_tab
                            is_exist TYPE boole_d
                            is_missing_auth TYPE boole_d.

  TRY.
      cl_hrpa_read_infotype=>get_instance(
        IMPORTING
          infotype_reader = DATA(infty_reader)
      ).
    CATCH cx_hrpa_violated_assertion.
  ENDTRY.

  TRY.
      infty_reader->read(
        EXPORTING
          tclas         = 'A'
          pernr         = p_pernr
          infty         = p_infty
          subty         = p_subty
          begda         = p_begda
          endda         = p_endda
          no_auth_check = abap_true
        IMPORTING
          infotype_tab  = lt_pre_record
          data_exists   = is_exist
          missing_auth  = is_missing_auth
      ).
    CATCH cx_hrpa_violated_assertion.
  ENDTRY.

ENDFORM.

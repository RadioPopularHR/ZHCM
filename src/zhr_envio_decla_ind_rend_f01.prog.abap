*&---------------------------------------------------------------------*
*&  Include           ZHR_ENVIO_DECLA_IND_REND_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_PAYROLL_PERNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_payroll_pernr .
  DATA: ls_pa0000 LIKE pa0000,
        it_result TYPE hrpay99_tab_of_results,
        wa_result TYPE pay99_result,
        it_res    TYPE hrpay99_rt,
        wa_res    TYPE pc207.
*    lt_result TYPE TABLE OF paypt_result."Est.Resultados do cálculo salários.

  FREE: gt_pernr[].

  CONCATENATE gv_year '0101' INTO gv_iniano.
  CONCATENATE gv_year '1231' INTO gv_fimano.

  LOOP AT p0000 INTO wa_pa0000.
    CALL FUNCTION 'HRCM_PAYROLL_RESULTS_GET'
      EXPORTING
        pernr              = wa_pa0000-pernr
        begda              = gv_iniano
        endda              = gv_fimano
      IMPORTING
*       SUBRC              =
*       MOLGA              =
        payroll_result_tab = it_result.

    IF sy-subrc <> 0.
**      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT it_result INTO wa_result.
      REFRESH: it_res.
      it_res[] =  wa_result-inter-rt[].
      READ TABLE it_res INTO wa_res WITH KEY lgart = '/106'.
      IF sy-subrc EQ 0.
        APPEND wa_pa0000-pernr TO gt_pernr.
      ENDIF.
    ENDLOOP.

    CLEAR ls_pa0000.
  ENDLOOP.

  SORT gt_pernr.

  DELETE ADJACENT DUPLICATES FROM gt_pernr.

ENDFORM.                    " GET_PAYROLL_PERNR
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_alv .

  DATA: wa_pa0105 TYPE p0105.
  DATA: lv_anoseg TYPE pabrj.

  CLEAR: gv_ini_marco, gv_fim_marco.
  lv_anoseg = gv_year + 1.
  CONCATENATE lv_anoseg '0301' INTO gv_ini_marco.
  CONCATENATE lv_anoseg '0331' INTO gv_fim_marco.


  SELECT * FROM icon INTO TABLE gt_icon
    WHERE name = 'ICON_GREEN_LIGHT' OR
          name = 'ICON_YELLOW_LIGHT' OR
          name = 'ICON_RED_LIGHT'.

  SELECT pernr stat2
   FROM pa0000 INTO TABLE lt_pernr_activos
   FOR ALL ENTRIES IN gt_pernr
   WHERE pernr = gt_pernr-pernr
     AND begda LE sy-datum
     AND endda GE sy-datum.


  LOOP AT gt_pernr INTO wa_pernr.
    wa_final_alv-pernr = wa_pernr-pernr.
    wa_final_alv-cname = p0331-fname.

    SELECT SINGLE btext
       INTO btrtl_aux
       FROM t001p
       WHERE werks = p0001-werks
       AND   btrtl = p0001-btrtl.

    CONCATENATE p0001-btrtl btrtl_aux
    INTO wa_final_alv-subarea
      SEPARATED BY '-'.

    wa_final_alv-year_sel = gv_year.
    wa_final_alv-stat2 = p0000-stat2.
    wa_final_alv-bukrs = p0001-bukrs.
    wa_final_alv-preview = '@X__PDF@'. "icon_pdf. "


    LOOP AT p0105 INTO wa_pa0105 WHERE  pernr = wa_pernr-pernr
                                    AND subty = '0010'
                                    AND endda GE sy-datum
                                    AND begda LE sy-datum.
      wa_final_alv-email = wa_pa0105-usrid_long. "icon_pdf. "
    ENDLOOP.


*   Verificar se colaborador está activo
    READ TABLE lt_pernr_activos INTO ls_pernr_activos
      WITH KEY pernr = wa_pernr-pernr.

    IF sy-subrc = 0
      AND ls_pernr_activos-stat2 = 0. "Não está activo
      READ TABLE gt_icon INTO wa_icon WITH KEY
       name = 'ICON_RED_LIGHT'.
      wa_final_alv-icon = wa_icon-id.
    ELSEIF sy-subrc = 0
      AND ls_pernr_activos-stat2 = 3.

      IF wa_final_alv-email IS NOT INITIAL.
        READ TABLE gt_icon INTO wa_icon WITH KEY
           name = 'ICON_GREEN_LIGHT'.
        wa_final_alv-icon = wa_icon-id.
      ELSE.
        READ TABLE gt_icon INTO wa_icon WITH KEY
           name = 'ICON_YELLOW_LIGHT'.
        wa_final_alv-icon = wa_icon-id.
      ENDIF.

    ENDIF.

    APPEND wa_final_alv TO gt_final_alv.

    CLEAR: wa_final_alv.
  ENDLOOP.

*MRC@ROFF - 26.01.2012 - Inicio
  IF pnpbtrtl-low IS NOT INITIAL.
    LOOP AT gt_final_alv INTO wa_gt_final_alv.
      IF wa_gt_final_alv-subarea(4) NOT IN pnpbtrtl.
        DELETE gt_final_alv INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
  ENDIF.
*MRC@ROFF - 26.01.2012 - Fim

ENDFORM.                    " GET_DATA_ALV
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .
  DATA: i_title TYPE lvc_title.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = gr_salv_table
        CHANGING
          t_table      = gt_final_alv.

    CATCH cx_salv_msg .
  ENDTRY.

  IF gr_salv_table IS INITIAL.
    MESSAGE TEXT-002 TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  gr_salv_table->set_screen_status(
                   pfstatus      = 'SALV_TABLE_STANDARD'
                   report        = sy-repid
                   set_functions = gr_salv_table->c_functions_all ).

* Criar objecto de capturar eventos e definir método para evento
  gr_events = gr_salv_table->get_event( ).
  CREATE OBJECT event_handler.
  SET HANDLER event_handler->on_user_command FOR gr_events.
  SET HANDLER event_handler->on_link_click FOR gr_events.

* Adicionar título
  i_title = TEXT-012.
  gr_display = gr_salv_table->get_display_settings( ).
  gr_display->set_list_header( i_title ).

* Definir atributos de colunas
  gr_columns = gr_salv_table->get_columns( ).

* Optimizar colunas
  gr_columns->set_optimize( 'X' ).

  TRY .
      gr_column ?= gr_columns->get_column( 'ICON' ).
      gr_column->set_long_text( 'Icon' ).
      gr_column->set_medium_text( 'Icon' ).
      gr_column->set_short_text( 'Icon' ).

      gr_column ?= gr_columns->get_column( 'PERNR' ).
      gr_column->set_long_text( 'Número Pessoal' ).
      gr_column->set_medium_text( 'Nº Pessoal' ).
      gr_column->set_short_text( 'Nº Pessoal' ).

      gr_column ?= gr_columns->get_column( 'CNAME' ).
      gr_column->set_long_text( 'Nome' ).
      gr_column->set_medium_text( 'Nome' ).
      gr_column->set_short_text( 'Nome' ).

      gr_column ?= gr_columns->get_column( 'SUBAREA' ).
      gr_column->set_long_text( 'Subárea' ).
      gr_column->set_medium_text( 'Subárea' ).
      gr_column->set_short_text( 'Subárea' ).

      gr_column ?= gr_columns->get_column( 'BUKRS' ).
      gr_column->set_long_text( 'Empresa' ).
      gr_column->set_medium_text( 'Empresa' ).
      gr_column->set_short_text( 'Empresa' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).

      gr_column ?= gr_columns->get_column( 'YEAR_SEL' ).
      gr_column->set_long_text( 'Ano Correspondente' ).
      gr_column->set_medium_text( 'Ano Correspondente' ).
      gr_column->set_short_text( 'Ano' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).

      gr_column ?= gr_columns->get_column( 'STAT2' ).
      gr_column->set_long_text( 'Status de Ocupação' ).
      gr_column->set_medium_text( 'Status Ocup.' ).
      gr_column->set_short_text( 'St.Oc.' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).

      gr_column ?= gr_columns->get_column( 'PREVIEW' ).
      gr_column->set_short_text( 'Preview' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      gr_column->set_icon( if_salv_c_bool_sap=>true ).

      gr_column ?= gr_columns->get_column( 'EMAIL' ).
      gr_column->set_long_text( 'Email' ).
      gr_column->set_medium_text( 'Email' ).
      gr_column->set_short_text( 'Email' ).

    CATCH cx_salv_not_found.
  ENDTRY.

* Permitir seleção de várias linhas da ALV
  gr_selections = gr_salv_table->get_selections( ).
  gr_selections->set_selection_mode( 4 ). "Seleção de linha e coluna

* Display da tabela
  gr_salv_table->display( ).

ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  ON_LINK_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROW  text
*      -->P_COLUMN  text
*----------------------------------------------------------------------*
FORM on_link_click  USING     p_row
                             p_column.

  FIELD-SYMBOLS: <fs_final_alv> TYPE ty_final_alv,
                 <fs_x>         TYPE x.

  DATA: lv_spoolid   TYPE rspoid,
        lv_fpcontent TYPE fpcontent.

  DATA: BEGIN OF lt_pdf_output OCCURS 0.
          INCLUDE STRUCTURE tline.
        DATA: END OF lt_pdf_output.

  DATA: ls_pdf_output LIKE LINE OF lt_pdf_output.

  DATA: it_pdf TYPE TABLE OF tline.

  READ TABLE gt_final_alv ASSIGNING <fs_final_alv>
     INDEX p_row.

  IF <fs_final_alv> IS ASSIGNED.
    CLEAR lv_spoolid.

    PERFORM f_submit_hrforms_dir USING    gv_year
                                         <fs_final_alv>-pernr
                                         <fs_final_alv>-bukrs
                                CHANGING lv_spoolid.

    IF lv_spoolid IS NOT INITIAL.

      CALL FUNCTION 'CONVERT_OTFSPOOLJOB_2_PDF'
        EXPORTING
          src_spoolid              = lv_spoolid
        TABLES
          pdf                      = lt_pdf_output
        EXCEPTIONS
          err_no_otf_spooljob      = 1
          err_no_spooljob          = 2
          err_no_permission        = 3
          err_conv_not_possible    = 4
          err_bad_dstdevice        = 5
          user_cancelled           = 6
          err_spoolerror           = 7
          err_temseerror           = 8
          err_btcjob_open_failed   = 9
          err_btcjob_submit_failed = 10
          err_btcjob_close_failed  = 11
          OTHERS                   = 12.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      CHECK sy-subrc EQ 0.

*       Convert pdf to xstring string
      LOOP AT lt_pdf_output INTO ls_pdf_output.
        ASSIGN ls_pdf_output TO <fs_x> CASTING.
        CONCATENATE gv_content <fs_x> INTO gv_content IN BYTE MODE.
      ENDLOOP.

      CALL SCREEN 100.

*       Apaga a Ordem de Spool com o HRForm
      PERFORM delete_spool USING lv_spoolid.

*     Erro ao gerar a ordem de spool com a DIR
    ELSE.
      PERFORM app_log_new_message USING 'E'      "Tipo de Msg
                                        'ZHCM'    "ID da Msg
                                        '001'    "Número da Mensagem
                                        <fs_final_alv>-pernr
                                        ''
                                        ''
                                        ''.
    ENDIF.
  ENDIF.

ENDFORM.                    " ON_LINK_CLICK
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_pdf .

  TYPES: BEGIN OF ty_binary,
           binary_field(1000) TYPE c,
         END OF ty_binary.

  DATA: lt_binary TYPE TABLE OF ty_binary.

  DATA: lv_spoolid TYPE rspoid,
        lv_buffer  TYPE string,
        lv_fich    TYPE string.

  DATA: BEGIN OF lt_pdf_output OCCURS 0.
          INCLUDE STRUCTURE tline.
        DATA: END OF lt_pdf_output.

  DATA: lt_rows TYPE salv_t_row,
        ls_rows TYPE i.

  DATA: ls_pdf_output LIKE LINE OF lt_pdf_output,
        numbytes      TYPE i,
        cancel,
        pdfspoolid    LIKE tsp01-rqident.

  FIELD-SYMBOLS: <fs_final_alv> TYPE ty_final_alv,
                 <fs_x>         TYPE x.

  CLEAR: gv_cancel_pdf, gv_dir.

* Obter registos da ALV que foram selecionados
  gr_selections = gr_salv_table->get_selections( ).
  lt_rows = gr_selections->get_selected_rows( ).

* Não foi selecionado nenhum registo para download do PDF
  IF lt_rows IS INITIAL.
    MESSAGE TEXT-005 TYPE 'W'.
    EXIT.
  ENDIF.

  CALL SCREEN 0300 STARTING AT 20 1 ENDING AT 100 5.

  IF gv_cancel_pdf IS NOT INITIAL.
    MESSAGE TEXT-006 TYPE 'W'.
    EXIT.
  ENDIF.

  IF gv_dir IS INITIAL.
    MESSAGE TEXT-007 TYPE 'W'.
    PERFORM download_pdf.
  ENDIF.

  LOOP AT lt_rows INTO ls_rows.

    READ TABLE gt_final_alv ASSIGNING <fs_final_alv>
       INDEX ls_rows.

    IF <fs_final_alv> IS ASSIGNED.
      CLEAR lv_spoolid.

      PERFORM f_submit_hrforms_dir USING gv_year
                                         <fs_final_alv>-pernr
                                         <fs_final_alv>-bukrs
                                CHANGING lv_spoolid.

      IF lv_spoolid IS NOT INITIAL.

        CALL FUNCTION 'CONVERT_OTFSPOOLJOB_2_PDF'
          EXPORTING
            src_spoolid              = lv_spoolid
            no_dialog                = ' '
          IMPORTING
            pdf_bytecount            = numbytes
            pdf_spoolid              = pdfspoolid
          TABLES
            pdf                      = lt_pdf_output
          EXCEPTIONS
            err_no_otf_spooljob      = 1
            err_no_spooljob          = 2
            err_no_permission        = 3
            err_conv_not_possible    = 4
            err_bad_dstdevice        = 5
            user_cancelled           = 6
            err_spoolerror           = 7
            err_temseerror           = 8
            err_btcjob_open_failed   = 9
            err_btcjob_submit_failed = 10
            err_btcjob_close_failed  = 11
            OTHERS                   = 12.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.


        CHECK sy-subrc EQ 0.

*       Erro ao Gerar a ordem de spool com a DIR
      ELSE.
        PERFORM app_log_new_message USING 'E'      "Tipo de Msg
                                          'ZHCM'    "ID da Msg
                                          '001'    "Número da Mensagem
                                          <fs_final_alv>-pernr
                                          ''
                                          ''
                                          ''.
        RETURN.
      ENDIF.

      CONCATENATE gv_dir '\DIR_' <fs_final_alv>-pernr
                  '.pdf' INTO lv_fich.


      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          bin_filesize            = numbytes
          filename                = lv_fich
          filetype                = 'BIN'
        TABLES
          data_tab                = lt_pdf_output
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          OTHERS                  = 22.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

*     Apaga a Ordem de Spool com o HRForm
      PERFORM delete_spool USING lv_spoolid.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " DOWNLOAD_PDF
*&---------------------------------------------------------------------*
*&      Form  DELETE_SPOOL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_SPOOLID  text
*----------------------------------------------------------------------*
FORM delete_spool  USING    p_lv_spoolid.

  DATA: lv_spool_nr TYPE tsp01_sp0r-rqid_char.

  lv_spool_nr = p_lv_spoolid.
* >>> INI ROFF SAM HR MN/EMP 7000050142 07.02.2018
* Comentado
*  CALL FUNCTION 'RSPO_R_RDELETE_SPOOLREQ'
*    EXPORTING
*      spoolid = lv_spool_nr.

  DATA: lv_rc            TYPE rspotype-rc,
        lv_status        TYPE sy-subrc,
        lv_error_message TYPE rspoemsg.

  CALL FUNCTION 'RSPO_R_RDELETE_SPOOLREQ'
    EXPORTING
      spoolid       = lv_spool_nr
      allow_commit  = 'X'
      ignore_locks  = 'X'
    IMPORTING
      rc            = lv_rc
      status        = lv_status
      error_message = lv_error_message.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
* <<< END ROFF SAM HR MN/EMP 7000050142 07.02.2018

ENDFORM.                    " DELETE_SPOOL
*&---------------------------------------------------------------------*
*&      Form  F_SUBMIT_HRFORMS_DIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_YEAR  text
*      -->P_<FS_FINAL_ALV>_PERNR  text
*      -->P_<FS_FINAL_ALV>_BUKRS  text
*      <--P_LV_SPOOLID  text
*      <--P_GV_PDF  text
*----------------------------------------------------------------------*
FORM f_submit_hrforms_dir  USING    pu_year TYPE pnppabrj
                                    pu_pernr  TYPE p_pernr
                                    pu_bukrs   TYPE bukrs
                           CHANGING pc_spoolid.

  TABLES: tbtcp.

  CONSTANTS: c_datasetname(6) TYPE c VALUE 'HRFORM',   " Nome do spool file
             c_reportid(8)    TYPE c VALUE 'RPCIIDP0'.    " Programa de impressão

  DATA: lt_params          TYPE TABLE OF rsparams,
        lt_spoolreq        TYPE TABLE OF rsporq,
        ls_params          TYPE rsparams,
        ls_printparameters TYPE pri_params,
        ls_arcparameters   TYPE arc_params,
        ls_spoolreq        TYPE rsporq,
        lt_tsp01           TYPE TABLE OF tsp01,
        ls_tsp01           TYPE tsp01,
        lv_spoolid_list    TYPE rspoid.

  DATA: lv_spoolname TYPE syplist,
        lv_jobname   TYPE btcjob, "Nome de um job em background
        lv_jobnumber TYPE btcjobcnt, "Nºidentificação de um job
        lv_uzeit     TYPE sy-uzeit.

  DATA: lv_aborted      LIKE tbtcv-abort,
        lv_finished     LIKE tbtcv-fin,
        lv_preliminary  LIKE tbtcv-prelim,
        lv_ready        LIKE tbtcv-ready,
        lv_running      LIKE tbtcv-run,
        lv_scheduled    LIKE tbtcv-sched,
        lv_text         TYPE string,
        lv_spoolid      TYPE rspoid,
        lv_reportid(40).

  DATA: l_pdfoutput TYPE fpformoutput,
        l_pdf       TYPE fpcontent.
** >>> INI ROFF SAM HR MN/EMP 7000050142 07.02.2018
** Comentado
**        pc_pdf   TYPE fpcontent.
** <<< END ROFF SAM HR MN/EMP 7000050142 07.02.2018


* Set parameters for HRFORMS Driver program
  REFRESH lt_params.

* Set the Import DataPersonnel number
  ls_params-sign    = 'I'.
  ls_params-option  = 'EQ'.

* Nº colaborador
  ls_params-selname = 'PNPPERNR'.
  ls_params-low     =  pu_pernr.
  APPEND ls_params  TO lt_params.

  ls_params-selname = 'PPDFFORM'.
  ls_params-low     =  'ZPYXXFO_ZHR_DIR'. "Nome do formulário
  APPEND ls_params  TO lt_params.

* Status de ocupação
  ls_params-sign    = 'I'.
  ls_params-option  = 'NE'.
  ls_params-selname = 'PNPSTAT2'.
  ls_params-low     =  '0'.
  APPEND ls_params  TO lt_params.

* >>> INI ROFF SAM EMP/SM HR 7000089707 21.02.2020
* Subcategorias.
  ls_params-selname = 'SUBCAT'.
  ls_params-low     = subcat. " 'X'.
  APPEND ls_params  TO lt_params.
* <<< END ROFF SAM EMP/SM HR 7000089707 21.02.2020

* Obter parâmetros de impressão
* Nome da ordem spool
* >>> INI ROFF SAM HR MN/EMP 7000050142 07.02.2018
* Comentado
*  CONCATENATE sy-datum sy-uzeit INTO lv_spoolname.
  CONCATENATE sy-datum sy-uzeit pu_pernr INTO lv_spoolname.
* <<< END ROFF SAM HR MN/EMP 7000050142 07.02.2018
  CONDENSE lv_spoolname.

  lv_uzeit = sy-uzeit.

* Em qualquer programa com escalonamento job devemos chamar esta FM
* de forma a obter os parâmetros de impressão
  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      data_set       = c_datasetname "Nome do spool file
      expiration     = '1'           "Período de retenção spool
      immediately    = space         "Saída imediata
      list_name      = lv_spoolname  "Nome da ordem spool
      new_list_id    = 'X'           "Nova ordem spool
      no_dialog      = 'X'
      mode           = 'BATCH' "Local de início de impressão da lista
    IMPORTING
      out_parameters = ls_printparameters
    EXCEPTIONS
      OTHERS         = 4.

* >>> INI ROFF SAM HR MN/EMP 7000050142 07.02.2018
*  CONCATENATE sy-uname 'HR_DIR'  INTO lv_jobname.
  CONCATENATE sy-uname 'HR_DIR' pu_pernr INTO lv_jobname.
* <<< END ROFF SAM HR MN/EMP 7000050142 07.02.2018

* Criar um background job
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
*     delanfrep        = 'X'
      jobname          = lv_jobname
    IMPORTING
      jobcount         = lv_jobnumber
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  CHECK sy-subrc = 0.

* Chamada do programa de impressão em modo background
  SUBMIT (c_reportid) WITH SELECTION-TABLE lt_params
                      TO SAP-SPOOL
                      SPOOL PARAMETERS ls_printparameters
                      WITHOUT SPOOL DYNPRO
                      VIA JOB lv_jobname NUMBER lv_jobnumber
                      AND RETURN
                      WITH ccode = pu_bukrs
                      WITH year  = pu_year.

  IF sy-subrc = 0.
    COMMIT WORK.

*   JOB_CLOSE to pass a background job to the background processing
*   system to be run
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = lv_jobnumber
        jobname              = lv_jobname
        strtimmed            = 'X' "Processamento background
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

* Só conseguimos obter o nº spool quando o job estiver finalizado
  DO 100 TIMES.

    WAIT UP TO 1 SECONDS.

    CALL FUNCTION 'SHOW_JOBSTATE'
      EXPORTING
        jobcount         = lv_jobnumber
        jobname          = lv_jobname
      IMPORTING
        aborted          = lv_aborted
        finished         = lv_finished
        preliminary      = lv_preliminary
        ready            = lv_ready
        running          = lv_running
        scheduled        = lv_scheduled
      EXCEPTIONS
        jobcount_missing = 1
        jobname_missing  = 2
        job_notex        = 3
        OTHERS           = 4.


    CONCATENATE lv_jobname '(' lv_jobnumber ')' INTO lv_text.

    CHECK lv_finished IS NOT INITIAL.

    SELECT SINGLE *
      FROM tbtcp
     WHERE jobname = lv_jobname
       AND jobcount = lv_jobnumber
       AND progname = c_reportid
       AND sdldate = sy-datum
       AND sdluname = sy-uname.

*   Eliminar um job que correu em background e o seu log
    CALL FUNCTION 'BP_JOB_DELETE'
      EXPORTING
        jobcount                 = lv_jobnumber
        jobname                  = lv_jobname
      EXCEPTIONS
        cant_delete_event_entry  = 1
        cant_delete_job          = 2
        cant_delete_joblog       = 3
        cant_delete_steps        = 4
        cant_delete_time_entry   = 5
        cant_derelease_successor = 6
        cant_enq_predecessor     = 7
        cant_enq_successor       = 8
        cant_enq_tbtco_entry     = 9
        cant_update_predecessor  = 10
        cant_update_successor    = 11
        commit_failed            = 12
        jobcount_missing         = 13
        jobname_missing          = 14
        job_does_not_exist       = 15
        job_is_already_running   = 16
        no_delete_authority      = 17
        OTHERS                   = 18.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    IF sy-subrc EQ 0.
      EXIT. " Exit from do..enddo loop
    ENDIF.

  ENDDO.

  pc_spoolid = tbtcp-listident.

** >>> INI ROFF SAM HR MN/EMP 7000050142 07.02.2018
*  CLEAR: gs_ctrl.
*  gs_ctrl-sender      = sy-uname.
*  gs_ctrl-spool       = pc_spoolid.
*  gs_ctrl-attach_type = 'RAW'.
*
*  REFRESH: gt_attach_bin.
*  PERFORM get_abapspool TABLES   gt_attach_bin
*                        CHANGING gs_ctrl.
** <<< END ROFF SAM HR MN/EMP 7000050142 07.02.2018

ENDFORM.                    " F_SUBMIT_HRFORMS_DIR
** >>> INI ROFF SAM HR MN/EMP 7000050142 07.02.2018
**&---------------------------------------------------------------------*
**&      Form  GET_ABAPSPOOL
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_IT_ATTACH_BIN  text
**      <--P_LS_CTRL  text
**----------------------------------------------------------------------*
*FORM get_abapspool  TABLES   pt_attach_bin
*                    CHANGING ps_ctrl TYPE zhr_ctrl.
*
*  REFRESH: pt_attach_bin.
*  CALL FUNCTION 'RSPO_RETURN_SPOOLJOB'
*    EXPORTING
*      rqident              = ps_ctrl-spool
*    IMPORTING
*      real_type            = ps_ctrl-attach_type
*    TABLES
*      buffer               = pt_attach_bin
*    EXCEPTIONS
*      no_such_job          = 1
*      job_contains_no_data = 2
*      selection_empty      = 3
*      no_permission        = 4
*      can_not_access       = 5
*      read_error           = 6
*      type_no_match        = 7
*      OTHERS               = 8.
*
*  ps_ctrl-subrc = sy-subrc.
*
*  IF ps_ctrl-subrc <> 0.
*    EXIT.
*  ENDIF.
*
** -- Comprime os dados em format 'RAW' para binário
*  IF ps_ctrl-attach_type = 'RAW'.
*    PERFORM compress_table  TABLES pt_attach_bin.
*  ENDIF.
*
*ENDFORM.                    " get_abapspool
*
**&---------------------------------------------------------------------*
**&      Form  compress_table
**&---------------------------------------------------------------------*
*FORM compress_table  TABLES   pt_attach.
*
*  CALL FUNCTION 'TABLE_COMPRESS'
*    TABLES
*      in     = pt_attach
*      out    = pt_attach
*    EXCEPTIONS
*      OTHERS = 1.
*
*ENDFORM.                    " compress_table
** <<< END ROFF SAM HR MN/EMP 7000050142 07.02.2018
*&---------------------------------------------------------------------*
*&      Form  GET_ERROR_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_error_log .

  TYPES: sx_addr_type LIKE sxaddrtype-addr_type, "R/3 Addresstype
         sx_addr      LIKE soextreci1-receiver.  "Address in plain string

  TYPES: BEGIN OF sx_address,                     "SAPconnect general addr
           type    TYPE sx_addr_type,
           address TYPE sx_addr,
         END OF sx_address,

         BEGIN OF ty_pernr_actv,
           pernr TYPE p0000-pernr,
           stat2 TYPE p0000-stat2,
         END OF ty_pernr_actv.

  DATA: lt_address TYPE sx_address,
        i_domain   TYPE sx_addr,
*        lt_pernr_activos TYPE TABLE OF ty_pernr_actv,
*        ls_pernr_activos TYPE ty_pernr_actv,
        ls_pa0105  LIKE pa0105,
        lv_ini_seg TYPE p0001-begda,
        lv_fim_seg TYPE p0001-endda,
        lv_mes_seg TYPE pabrp.

* Objecto e Sub Objecto p/ Log Aplicação
  DATA: lv_extnum LIKE balhdr-extnumber.

  FIELD-SYMBOLS: <fs_pernr> TYPE ty_pernr.

*  SELECT pernr stat2
*  FROM pa0000 INTO TABLE lt_pernr_activos
*  FOR ALL ENTRIES IN gt_pernr
*  WHERE pernr = gt_pernr-pernr
*    AND begda LE gv_fim_marco
*    AND endda GE gv_ini_marco.

  LOOP AT gt_pernr ASSIGNING <fs_pernr>.

    IF <fs_pernr> IS ASSIGNED.

*     Verificar se colaborador está activo
      READ TABLE lt_pernr_activos INTO ls_pernr_activos
        WITH KEY pernr = <fs_pernr>-pernr.

      IF sy-subrc = 0
        AND ls_pernr_activos-stat2 = 0. "Não está activo
        PERFORM app_log_new_message USING 'E'      "Tipo de Msg
                                          'ZHCM'    "ID da Msg
                                          '002'    "Número da Mensagem
                                          <fs_pernr>-pernr
                                          ''
                                          ''
                                          ''.
      ENDIF.


*      READ TABLE gt_pa0105 INTO ls_pa0105
*        WITH KEY pernr = wa_pernr-pernr.
*      IF p0105 IS NOT INITIAL.
*     Verificar se endereço de email é válido
      lt_address-type = 'INT'.
      lt_address-address = p0105-usrid_long.

      CALL FUNCTION 'SX_INTERNET_ADDRESS_TO_NORMAL'
        EXPORTING
          address_unstruct    = lt_address
        IMPORTING
          domain              = i_domain
        EXCEPTIONS
          error_address_type  = 1
          error_address       = 2
          error_group_address = 3
          OTHERS              = 4.
      IF sy-subrc <> 0.
        PERFORM app_log_new_message USING 'E'       "Tipo de Msg
                                          'ZHCM'     "ID da Msg
                                          '003'     "Número da Mensagem
                                          <fs_pernr>-pernr
                                          ''
                                          ''
                                          ''.
      ENDIF.
*      ENDIF.
    ENDIF.

  ENDLOOP.

* Finalização do Log
  PERFORM app_end_log USING gc_object
                            gc_subobj.

  CALL FUNCTION 'APPL_LOG_DISPLAY'
    EXPORTING
      object                    = gc_object
      subobject                 = gc_subobj
      external_number           = lv_extnum
      date_from                 = gv_datum
      time_from                 = gv_uzeit
      date_to                   = sy-datum
      time_to                   = sy-uzeit
      suppress_selection_dialog = 'X'
    IMPORTING
      number_of_protocols       = sy-dbcnt
    EXCEPTIONS
      no_authority              = 1
      OTHERS                    = 2.

  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.



ENDFORM.                    " GET_ERROR_LOG

*&---------------------------------------------------------------------*
*&      Form  WRITE_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_mail .

  DATA: lt_rows TYPE salv_t_row,
        ls_rows TYPE i.

  DATA: lv_nemail   TYPE i,
        lv_nemailc  TYPE char10,
        lv_cont     TYPE char10,
        lv_progress TYPE string,
        lv_spoolid  TYPE rspoid.

  FIELD-SYMBOLS: <fs_final_alv> TYPE ty_final_alv,
                 <fs_pa0105>    LIKE p0105.

* Obter registos da ALV que foram selecionados
  gr_selections = gr_salv_table->get_selections( ).
  lt_rows = gr_selections->get_selected_rows( ).

* Não foi selecionado nenhum colaborador para envio de Email
  IF lt_rows IS INITIAL.
    MESSAGE TEXT-004 TYPE 'W'.
    EXIT.
  ENDIF.

  DESCRIBE TABLE lt_rows LINES lv_nemail.
  lv_nemailc = lv_nemail.
  CONDENSE lv_nemailc NO-GAPS.

  CLEAR lv_cont.

  CLEAR: gv_mail_subject,
         gt_mail_body,
         gv_cancel_mail.


  CONCATENATE TEXT-016 gv_year INTO
              gv_mail_subject SEPARATED BY space.

  CONCATENATE TEXT-017  gv_year INTO
              wa_mail_body SEPARATED BY space.

  APPEND wa_mail_body TO gt_mail_body.

  CALL SCREEN 0200 STARTING AT 20 1 ENDING AT 109 18.

  IF gv_cancel_mail IS NOT INITIAL.
    MESSAGE TEXT-006 TYPE 'W'.
    EXIT.
  ENDIF.

  LOOP AT lt_rows INTO ls_rows.

    lv_cont = lv_cont + 1.
    CONCATENATE TEXT-008 lv_cont TEXT-009 lv_nemailc
        TEXT-010 INTO lv_progress SEPARATED BY space.

    PERFORM sapgui_progress_indicator USING lv_progress.

    READ TABLE gt_final_alv ASSIGNING <fs_final_alv>
         INDEX ls_rows.

    IF <fs_final_alv> IS ASSIGNED.

      IF <fs_final_alv>-email IS NOT INITIAL.

        CLEAR lv_spoolid.

        PERFORM f_submit_hrforms_dir USING gv_year
                                           <fs_final_alv>-pernr
                                           <fs_final_alv>-bukrs
                                  CHANGING lv_spoolid.

*       Gerou a ordem de spool com a DIR
        IF lv_spoolid IS NOT INITIAL.

          PERFORM process_email_dir USING <fs_final_alv>-pernr
                                          lv_spoolid
                                          <fs_final_alv>-email
                                          gv_pdf.

*         Apaga a Ordem de Spool com o HRForm
          PERFORM delete_spool USING lv_spoolid.

*       Erro ao Gerar a ordem de spool com a DIR
        ELSE.
          PERFORM app_log_new_message USING 'E'      "Tipo de Msg
                                            'ZHCM'    "ID da Msg
                                            '001'    "Número da Mensagem
                                            <fs_final_alv>-pernr
                                            ''
                                            ''
                                            ''.

        ENDIF.
      ENDIF.
    ENDIF.

    UNASSIGN <fs_final_alv>.
  ENDLOOP.

ENDFORM.                    " WRITE_MAIL
*&---------------------------------------------------------------------*
*&      Form  PROCESS_EMAIL_DIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_FINAL_ALV>_PERNR  text
*      -->P_LV_SPOOLID  text
*      -->P_LV_OBJECT_ID  text
*      -->P_<FS_PA0105>_USRID_LONG  text
*      -->P_GV_PDF  text
*----------------------------------------------------------------------*
FORM process_email_dir  USING pu_pernr
                              pu_spoolid
                              pu_email
                              pu_pdf.


  DATA: lt_receivers TYPE TABLE OF somlreci1,
        ls_receivers TYPE somlreci1,
        lv_attachdes TYPE so_obj_des,
        lv_object_id TYPE so_obj_id.

  lv_attachdes = TEXT-011.

  ls_receivers-receiver = pu_email.
  ls_receivers-rec_type = 'U'. "Tipo de destinatário Endereço Internet
  APPEND ls_receivers TO lt_receivers.

  CALL FUNCTION 'ZHR_FM_SEND_MAIL'
    EXPORTING
      iv_sender                  = sy-uname
      iv_subject                 = gv_mail_subject
** >>> INI ROFF SAM HR MN/EMP 7000050142 07.02.2018
** Comentado
      iv_spool                   = pu_spoolid
**      iv_spool                   = lv_spoolid
**      iv_attach_as_msg           = 'X'
** <<< END ROFF SAM HR MN/EMP 7000050142 07.02.2018
      iv_attach_type             = 'RAW'
      iv_attachment_desc         = lv_attachdes
** >>> INI ROFF SAM HR MN/EMP 7000050142 07.02.2018
*     iv_ctrl                    = gs_ctrl
** <<< END ROFF SAM HR MN/EMP 7000050142 07.02.2018
    IMPORTING
      new_object_id              = lv_object_id
    TABLES
      it_message                 = gt_mail_body
      it_receivers               = lt_receivers
** >>> INI ROFF SAM HR MN/EMP 7000050142 07.02.2018
*     it_attach_bin              = gt_attach_bin
** <<< END ROFF SAM HR MN/EMP 7000050142 07.02.2018
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      sap_user_error             = 8
      no_such_job                = 9
      job_contains_no_data       = 10
      selection_empty            = 11
      no_permission              = 12
      can_not_access             = 13
      read_error                 = 14
      type_no_match              = 15
      empty_tables               = 16
      attach_table_empty         = 17
      receivers_table_empty      = 18
      rec_type_empty             = 19
      OTHERS                     = 20.


* Email enviado com sucesso
  IF sy-subrc = 0.
    PERFORM app_log_new_message USING 'S'      "Tipo de Msg
                                      'ZHCM'    "ID da Msg
                                      '004'    "Número da Mensagem
                                      pu_pernr
                                      ''
                                      ''
                                      ''.
  ELSE.
    PERFORM app_log_new_message USING 'E'      "Tipo de Msg
                                      'ZHCM'    "ID da Msg
                                      '005'    "Número da Mensagem
                                      pu_pernr
                                      ''
                                      ''
                                      ''.
  ENDIF.

ENDFORM.                    " PROCESS_EMAIL_DIR
*&---------------------------------------------------------------------*
*&      Form  SAPGUI_PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_PROGRESS  text
*----------------------------------------------------------------------*
FORM sapgui_progress_indicator  USING    pu_lv_progress.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 0
      text       = pu_lv_progress.

ENDFORM.                    " SAPGUI_PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  CHECK_MAIL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_STOP  text
*----------------------------------------------------------------------*
FORM check_mail_data  CHANGING p_stop.
  DATA: lv_question TYPE string,
        lv_answer   TYPE c.

  CALL METHOD gr_text_editor->get_text_as_stream
    IMPORTING
      text = gt_mail_body.

  IF gv_mail_subject IS INITIAL
    AND gt_mail_body[]  IS INITIAL.
    lv_question = TEXT-013.
  ELSEIF gv_mail_subject IS INITIAL.
    lv_question = TEXT-014.
  ELSEIF gt_mail_body[]  IS INITIAL.
    lv_question = TEXT-015.
  ENDIF.

  IF lv_question IS NOT INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question = lv_question
      IMPORTING
        answer        = lv_answer.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF lv_answer NE '1'.
      p_stop = 'X'.
      EXIT.
    ENDIF.

  ENDIF.
ENDFORM.                    " CHECK_MAIL_DATA

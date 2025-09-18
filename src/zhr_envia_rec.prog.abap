*&---------------------------------------------------------------------*
*& Report  ZHR_ENVIA_REC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zhr_envia_rec MESSAGE-ID zhr_generic.

CLASS cl_gui_alv_grid DEFINITION LOAD.

* Constantes ***********************************************************
CONSTANTS:
           k_mail     TYPE char4    VALUE '0010',
           k_char_x   TYPE char1    VALUE 'X',
           k_endda    TYPE endda    VALUE '99991231',
           k_keywords TYPE keywords VALUE 'ZHR_ENVIA_REC',
           c_mailsend(8)  TYPE  c   VALUE 'MAILSEND',
           c_e            TYPE  c   VALUE 'E',
           c_prog_impressao_recibo(26) TYPE c VALUE '/1PYXXFO/YSAP_PAYSLIP_PRNT'.

TABLES: pernr.

TYPES: BEGIN OF tp_pernr,
        pernr TYPE p_pernr,
        kostl TYPE kostl,
      END OF tp_pernr.

DATA: gv_pernr    TYPE  pccet_pernr_unsorted.
DATA: gt_personid TYPE  pccet_pernr_personid.

DATA: gt_result   TYPE STANDARD TABLE OF paypt_result.
DATA: gt_pernr    TYPE STANDARD TABLE OF tp_pernr.

DATA: gv_ano TYPE pnppabrj,
      gv_mes TYPE pnppabrp.

DATA gv_exit.

DATA: BEGIN OF gt_final OCCURS 0,
          pernr              TYPE p_pernr,
          mes                TYPE fcmnr,
          ano                TYPE gjahr,
          entry_date         TYPE so_entdate,
          entry_time         TYPE so_enttime,
          estado             TYPE zhr_recibo_log-estado,
          estado_icon        TYPE icon_d,
          mes_correspondente TYPE t247-ltx,
          log                TYPE bapi_msg,
          cname              TYPE pad_cname,
          mail               TYPE comm_id_long,
          kostl              TYPE kostl,
          send               TYPE zhr_recibo_log-send,
          objectid           TYPE so_obj_id,
          box TYPE c,
      END   OF gt_final.

DATA: it_exclude    TYPE slis_t_extab.
DATA: wa_exclude    LIKE LINE OF it_exclude.
DATA gt_p0105 LIKE pa0105 OCCURS 0 WITH HEADER LINE.

TYPES: BEGIN OF tp_itab_alv,
          num_empregado TYPE p_pernr,
          ano TYPE gjahr,
          objno TYPE so_obj_no,
          mes TYPE fcmnr,
          entry_date TYPE so_entdate,
          entry_time TYPE so_enttime,
          counter TYPE so_sta_cnt,
          estado TYPE zhr_recibo_log-estado,
          estado_icon TYPE icon_d,
          mes_correspondente TYPE t247-ltx,
          log TYPE bapi_msg,
          sel(1),
      END OF tp_itab_alv.

TABLES: sscrfields.

* Tipos ****************************************************************
TYPE-POOLS: slis, icon.

TYPES: BEGIN OF ty_color,
         color_line TYPE char4,       " Line color
         color_cell TYPE lvc_t_scol,  " Cell color
       END OF ty_color.

* Constantes ***********************************************************
CONSTANTS: k_true     TYPE char1  VALUE 'X'.

*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
DATA:
      gr_salv_table TYPE REF TO cl_salv_table,
      gr_functions  TYPE REF TO cl_salv_functions,
      gr_display    TYPE REF TO cl_salv_display_settings,
      gr_columns    TYPE REF TO cl_salv_columns_table,
      gr_column     TYPE REF TO cl_salv_column_table,
      lr_layout     TYPE REF TO cl_salv_layout,
      gr_events     TYPE REF TO cl_salv_events_table,
      gr_selections TYPE REF TO cl_salv_selections.

DATA: gs_key     TYPE salv_s_layout_key.
DATA: lt_rows TYPE lvc_t_row,
      la_rows TYPE LINE OF lvc_t_row.

DATA: lo_header  TYPE REF TO cl_salv_form_layout_grid,
      lo_h_label TYPE REF TO cl_salv_form_label.

*--------------------------------------------------------------------*
*  CLASS lcl_handle_events DEFINITION
*--------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
*      Método para o botão de cancelamento em massa e selecção
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
ENDCLASS. "lcl_handle_events DEFINITION
*--------------------------------------------------------------------*
*  CLASS lcl_handle_events IMPLEMENTATION
*--------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.

    CASE e_salv_function.
      WHEN 'MAILSEND'.
        lt_rows = gr_selections->get_selected_rows( ).
        CALL METHOD cl_gui_cfw=>flush.
        PERFORM preparar_mail.
        gr_salv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ENDCASE.

  ENDMETHOD.                    "on_user_command
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK blc0 WITH FRAME.
PARAMETERS: p_erro NO-DISPLAY.
*** AS CHECKBOX .
SELECTION-SCREEN END OF BLOCK blc0.

SELECTION-SCREEN BEGIN OF BLOCK blc1 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(50) text-901 .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blc1 .

*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
* Start-of-selection ***************************************************
START-OF-SELECTION.

  PERFORM check_bloqueio USING 'X'.

  IF gv_exit IS NOT INITIAL.
    wa_exclude-fcode = c_mailsend.
    APPEND wa_exclude TO it_exclude.
  ELSE.
    REFRESH it_exclude.
  ENDIF.

  CASE 'X'.
    WHEN pnptimr1 OR pnptimr2 OR pnptimr3 OR pnptimr4 OR pnptimr5.
      gv_ano = sy-datum+0(4).
      gv_mes = sy-datum+4(2).
    WHEN pnptimr6.
      CASE 'X'.
        WHEN pnptimr9.
          gv_ano = pnpdispj.
          gv_mes = pnpdispp.
        WHEN pnptimra.
          gv_ano = pnppabrj.
          gv_mes = pnppabrp.
        WHEN OTHERS.
          IF pnpendda IS INITIAL.
            gv_ano = pnpbegda+0(4).
            gv_mes = pnpbegda+4(2).
          ELSE.
            gv_ano = pnpendda+0(4).
            gv_mes = pnpendda+4(2).
          ENDIF.
      ENDCASE.
  ENDCASE.

GET pernr.

  REFRESH:  gv_pernr,
            gt_personid,
            gt_result.
  CLEAR gv_pernr.
  APPEND pernr-pernr TO gv_pernr.

**************************************************************************
* Esta função é chamada no programa de impressão e tem como objectivo
*   verificar se é possível ou não a impressão do recibo de ordenado
*   do respectivo empregado.
  CALL FUNCTION 'HR_PERSONEE_GETPERSON_LIST'
    EXPORTING
      it_pernr    = gv_pernr
    IMPORTING
      et_personid = gt_personid.


  CHECK gt_personid IS NOT INITIAL.

*** verifica se tem processamento no mes/ano indicado
  CALL FUNCTION 'HR_GET_PAYROLL_RESULTS'
    EXPORTING
      pernr                         = pernr-pernr
      pabrj                         = gv_ano
      pabrp                         = gv_mes
      arch_too                      = ''
    TABLES
      result_tab                    = gt_result
    EXCEPTIONS
      no_results                    = 1
      error_in_currency_conversion  = 2
      t500l_entry_not_found         = 3
      period_mismatch_error         = 4
      t549q_entry_not_found         = 5
      internal_error                = 6
      wrong_structure_of_result_tab = 7
      OTHERS                        = 8.

  IF gt_result IS NOT INITIAL.
    APPEND pernr-pernr TO gt_pernr.
  ENDIF.

END-OF-SELECTION.

  IF NOT gt_pernr[] IS INITIAL.
    PERFORM check_dados.
    PERFORM get_dados_todos.
    IF gt_final[] IS INITIAL.
      MESSAGE i016(zhr_generic) WITH text-011.
    ELSE.
      PERFORM write_alv.
    ENDIF.
  ELSE.
    MESSAGE i016(zhr_generic) WITH text-011.
  ENDIF.

  PERFORM check_bloqueio USING ''.
*&---------------------------------------------------------------------*
*&      Form  CHECK_BLOQUEIO
*&---------------------------------------------------------------------*
FORM check_bloqueio USING pv_lock TYPE any.

  DATA: itab_enq LIKE seqg3 OCCURS 0 WITH HEADER LINE,
        lv_gname TYPE eqegraname VALUE 'KEYWORD'.
*        lv_string TYPE string.

  IF pv_lock IS NOT INITIAL.

*   Verifica se já existe bloqueio
    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        gclient               = sy-mandt
        gname                 = lv_gname
        guname                = ''
      TABLES
        enq                   = itab_enq
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      LOOP AT itab_enq WHERE gtarg(10) EQ k_keywords.
        MESSAGE i001 WITH text-023  itab_enq-guname text-028.
        gv_exit = k_char_x.
        RETURN.
      ENDLOOP.
    ENDIF.

*   Ainda não existe bloqueio, vai criar o bloqueio
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = k_char_x
        keyword_       = k_keywords
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

  ELSE.
*   Desbloquear
    CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword = k_char_x
        keyword_     = k_keywords.
  ENDIF.

ENDFORM.                    " CHECK_BLOQUEIO
*&---------------------------------------------------------------------*
*&      Form  CHECK_DADOS
*&---------------------------------------------------------------------*
FORM check_dados .

* Registro mestre HR infotipo 0105 (comunicações)
  SELECT * FROM pa0105 INTO TABLE gt_p0105
      FOR ALL ENTRIES IN gt_pernr
       WHERE pernr EQ gt_pernr-pernr
         AND subty EQ k_mail
         AND endda EQ k_endda.

ENDFORM.                    " CHECK_DADOS
*&---------------------------------------------------------------------*
*&      Form  get_dados_todos
*&---------------------------------------------------------------------*
*       Adaptação do form get_dados, para obter também os dados que vao
* ser visualizados no log
*----------------------------------------------------------------------*
FORM get_dados_todos.

  TYPES: BEGIN OF t_sost,
            objtp LIKE sost-objtp,
            objyr LIKE sost-objyr,
            objno LIKE sost-objno,
            counter LIKE sost-counter,
            msgno LIKE sost-msgno,
            msgty LIKE sost-msgty,
            msgid LIKE sost-msgid,
            msgv1 LIKE sost-msgv1,
            msgv2 LIKE sost-msgv2,
            msgv3 LIKE sost-msgv3,
            msgv4 LIKE sost-msgv4,
    END OF t_sost.

  DATA: lt_sost  TYPE TABLE OF t_sost,
        wa_sost  TYPE t_sost,
        wa_pernr TYPE tp_pernr.

  DATA: BEGIN OF s_pa0001,
          pernr LIKE pa0001-pernr,
          kostl LIKE pa0001-kostl,
        END OF s_pa0001.
  DATA: BEGIN OF s_pa0002,
          pernr LIKE pa0002-pernr,
          cname LIKE pa0002-cname,
        END OF s_pa0002.

  DATA: lt_itab   LIKE gt_final OCCURS 0,
        lt_pa0001 LIKE s_pa0001 OCCURS 0 WITH HEADER LINE,
        lt_pa0002 LIKE s_pa0002 OCCURS 0 WITH HEADER LINE.

  DATA: wa_itab     LIKE  gt_final,
        wa_itab_aux LIKE  gt_final,
        lv_nome     TYPE  t247-ltx,
        lv_exist    TYPE  flag.

  REFRESH gt_final.

  CHECK gt_pernr[] IS NOT INITIAL.

* Tabela de Dados Mestre HCM
  SELECT * FROM zhr_recibo_log INTO CORRESPONDING FIELDS OF TABLE lt_itab
      FOR ALL ENTRIES IN gt_pernr
       WHERE pernr = gt_pernr-pernr
         AND mes   = gv_mes
         AND ano   = gv_ano.

  SORT lt_itab BY pernr      ASCENDING
                  entry_date DESCENDING
                  entry_time DESCENDING.

  SORT gt_p0105 BY pernr.

* Valida que existe dados na lt_itab
  IF NOT lt_itab[] IS INITIAL.
*   SAPoffice: tabela log status
    SELECT * FROM sost
             INTO CORRESPONDING FIELDS OF TABLE lt_sost
        FOR ALL ENTRIES IN lt_itab
         WHERE objtp EQ lt_itab-objectid+0(3)
           AND objyr EQ lt_itab-objectid+3(2)
           AND objno EQ lt_itab-objectid+5(12).

    SORT lt_sost BY objtp   ASCENDING
                    objyr   ASCENDING
                    objno   ASCENDING
                    counter DESCENDING.
  ENDIF.

* Registro mestre HR infotipo 0002 (Dados pessoais)
  SELECT * FROM pa0002 INTO CORRESPONDING FIELDS OF TABLE lt_pa0002
      FOR ALL ENTRIES IN gt_pernr
       WHERE pernr = gt_pernr-pernr AND
             begda LE sy-datum AND
             endda GE sy-datum.
  SORT lt_pa0002 BY pernr.

  SELECT * FROM pa0001 INTO CORRESPONDING FIELDS OF TABLE lt_pa0001
      FOR ALL ENTRIES IN gt_pernr
       WHERE pernr = gt_pernr-pernr AND
             begda LE sy-datum AND
             endda GE sy-datum.
  SORT lt_pa0001 BY pernr.

  LOOP AT gt_pernr INTO wa_pernr.
    wa_itab-pernr = wa_pernr-pernr.
    READ TABLE gt_p0105 WITH KEY pernr = wa_itab-pernr
            BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_itab-mail  = gt_p0105-usrid_long.
    ENDIF.

    READ TABLE lt_pa0002 WITH KEY pernr = wa_itab-pernr
                           BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_itab-cname = lt_pa0002-cname.
    ENDIF.

    READ TABLE lt_pa0001 WITH KEY pernr = wa_itab-pernr
                           BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_itab-kostl = lt_pa0001-kostl.
    ENDIF.

    CALL FUNCTION 'ISP_GET_MONTH_NAME'
      EXPORTING
        language     = 'P'
        month_number = gv_mes
      IMPORTING
        longtext     = lv_nome.

    wa_itab-mes                = gv_mes.
    wa_itab-ano                = gv_ano.
    wa_itab-mes_correspondente = lv_nome.

****
    CLEAR lv_exist.
    LOOP AT lt_itab INTO wa_itab_aux
      WHERE pernr = wa_itab-pernr.

      lv_exist = k_true.

      IF wa_itab_aux-estado EQ ''.
        wa_itab-estado_icon = '@5D@'.
        wa_itab-log         = text-022.
        wa_itab-estado      = wa_itab_aux-estado.

        APPEND wa_itab TO gt_final.

        CLEAR: wa_itab-estado_icon,
               wa_itab-log,
               wa_itab-estado.
      ELSEIF wa_itab_aux-estado = c_e.
        wa_itab-estado_icon = '@5C@'.
        wa_itab-log         = text-029.
        wa_itab-estado      = wa_itab_aux-estado.

        wa_itab-entry_date = wa_itab_aux-entry_date.
        wa_itab-entry_time = wa_itab_aux-entry_time.

        IF ( p_erro = 'X' AND wa_sost-msgty NE 'S' AND wa_itab-mail NE '' ) OR p_erro IS INITIAL.
          APPEND wa_itab TO gt_final.
        ENDIF.
        CLEAR: wa_itab-estado_icon,
               wa_itab-log,
               wa_itab-estado,
               wa_itab-entry_date,
               wa_itab-entry_time.
      ELSE.
        wa_itab-estado_icon = '@5B@'.
        wa_itab-estado      = wa_itab_aux-estado.

        READ TABLE lt_sost INTO wa_sost
                       WITH KEY objtp = wa_itab_aux-objectid+0(3)
                                objyr = wa_itab_aux-objectid+3(2)
                                objno = wa_itab_aux-objectid+5(12)
                       BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF wa_sost-msgty EQ 'S'.
            wa_itab-estado_icon = '@5B@'.
          ELSEIF wa_sost-msgty = 'I'.
            wa_itab-estado_icon = '@5B@'.
          ELSE.
            wa_itab-estado_icon = '@5C@'.
          ENDIF.
*
          MESSAGE ID wa_sost-msgid TYPE wa_sost-msgty NUMBER wa_sost-msgno
            INTO wa_itab-log
            WITH wa_sost-msgv1 wa_sost-msgv2 wa_sost-msgv3 wa_sost-msgv4.
        ENDIF.

        wa_itab-entry_date = wa_itab_aux-entry_date.
        wa_itab-entry_time = wa_itab_aux-entry_time.

*** se pisco de erro, so mostra PERNR com erro no envio.
        IF ( p_erro = 'X' AND wa_sost-msgty NE 'S' AND wa_itab-mail NE '' ) OR p_erro IS INITIAL.
          APPEND wa_itab TO gt_final.
        ENDIF.

        CLEAR: wa_itab-estado_icon,
               wa_itab-log,
               wa_itab-estado,
               wa_itab-entry_date,
               wa_itab-entry_time.
      ENDIF.
    ENDLOOP.

    IF lv_exist IS INITIAL.
      wa_itab-estado_icon = '@5D@'.
      wa_itab-log         = text-022.
      IF ( p_erro = 'X' AND wa_sost-msgty NE 'S' AND wa_itab-mail NE '' ) OR p_erro IS INITIAL.
        APPEND wa_itab TO gt_final.
      ENDIF.
    ENDIF.

    CLEAR: gt_p0105, lt_pa0002.
    CLEAR: wa_itab, wa_sost, wa_itab_aux.
  ENDLOOP.

  SORT gt_final BY kostl pernr entry_date DESCENDING
                                       entry_time DESCENDING.

*** ficar apenas com os registos mais recentes
  DELETE ADJACENT DUPLICATES FROM gt_final COMPARING pernr.

ENDFORM.                    "get_dados_todos
*&---------------------------------------------------------------------*
*&      Form  WRITE_ALV
*&---------------------------------------------------------------------*
FORM write_alv.

  DATA: event_handler TYPE REF TO lcl_handle_events.
  DATA: lv_count LIKE sy-tabix.
  DESCRIBE TABLE gt_final LINES lv_count.

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = gr_salv_table
                               CHANGING t_table = gt_final[] ).

      gr_salv_table->set_screen_status(
      pfstatus = 'ZSTANDARD'
      report = sy-repid ).

      gr_functions = gr_salv_table->get_functions( ).
      gr_functions->set_all( abap_true ).


      gr_display = gr_salv_table->get_display_settings( ).
      gr_display->set_striped_pattern( cl_salv_display_settings=>true ).

      gr_columns = gr_salv_table->get_columns( ).

      gr_columns->set_optimize( 'X' ).

*     Mudar descrição de colunas
      gr_column ?= gr_columns->get_column( 'PERNR' ).
      gr_column->set_long_text( text-013 ).
      gr_column->set_medium_text( text-013 ).
*      gr_column->set_short_text( text-106 ).

      gr_column ?= gr_columns->get_column( 'CNAME' ).
      gr_column->set_long_text( text-014 ).
      gr_column->set_medium_text( text-014 ).
*      gr_column->set_short_text( text-107 ).

      gr_column ?= gr_columns->get_column( 'MAIL' ).
      gr_column->set_long_text( text-015 ).
      gr_column->set_medium_text( text-015 ).
*      gr_column->set_short_text( text-108 ).

      gr_column ?= gr_columns->get_column( 'KOSTL' ).
      gr_column->set_long_text( text-031 ).
      gr_column->set_medium_text( text-031 ).
*      gr_column->set_short_text( text-109 ).

      gr_column ?= gr_columns->get_column( 'MES_CORRESPONDENTE' ).
      gr_column->set_long_text( text-016 ).
      gr_column->set_medium_text( text-016 ).
*      gr_column->set_short_text( text-110 ).

      gr_column ?= gr_columns->get_column( 'ANO' ).
      gr_column->set_long_text( text-017 ).
      gr_column->set_medium_text( text-017 ).
      gr_column->set_short_text( text-111 ).

      gr_column ?= gr_columns->get_column( 'ESTADO_ICON' ).
      gr_column->set_long_text( text-018 ).
      gr_column->set_medium_text( text-018 ).
*      gr_column->set_short_text( text-112 ).

      gr_column ?= gr_columns->get_column( 'LOG' ).
      gr_column->set_long_text( text-019 ).
      gr_column->set_medium_text( text-019 ).
*      gr_column->set_short_text( text-113 ).

      gr_column ?= gr_columns->get_column( 'ENTRY_DATE' ).
      gr_column->set_long_text( text-020 ).
      gr_column->set_medium_text( text-020 ).
*      gr_column->set_short_text( text-020 ).

      gr_column ?= gr_columns->get_column( 'ENTRY_TIME' ).
      gr_column->set_long_text( text-021 ).
      gr_column->set_medium_text( text-021 ).
*      gr_column->set_short_text( text-021 ).

      gr_column ?= gr_columns->get_column( 'OBJECTID' ).
      gr_column->set_visible( space ).

      gr_column ?= gr_columns->get_column( 'SEND' ).
      gr_column->set_visible( space ).

      gr_column ?= gr_columns->get_column( 'BOX' ).
      gr_column->set_visible( space ).

      gr_selections = gr_salv_table->get_selections( ).
      gr_selections->set_selection_mode( 2 ).

      gr_events = gr_salv_table->get_event( ).
      CREATE OBJECT event_handler.
      SET HANDLER event_handler->on_user_command FOR gr_events.

*   Header object
      CREATE OBJECT lo_header.

      lo_h_label = lo_header->create_label( row = 1  column = 1 ).
      lo_h_label->set_text(  text-003 ).
      lo_h_label = lo_header->create_label( row = 2  column = 1 ).
      lo_h_label->set_text( 'Data' ).
      lo_h_label = lo_header->create_label( row = 2  column = 2 ).
      lo_h_label->set_text(  sy-datum ).

      lo_h_label = lo_header->create_label( row = 3 column = 1 ).
      lo_h_label->set_text( 'Nr recibos:' ).
      lo_h_label = lo_header->create_label( row = 3 column = 2 ).
      lo_h_label->set_text( lv_count ).

*     Set the top of list using the header for Online.
      gr_salv_table->set_top_of_list( lo_header ).

*     Set the top of list using the header for Print.
      gr_salv_table->set_top_of_list_print( lo_header ).

      lr_layout = gr_salv_table->get_layout( ).
      gs_key-report = sy-repid.
      lr_layout->set_key( gs_key ).  "Pass Program Name
      lr_layout->set_save_restriction( cl_salv_layout=>restrict_none ).
      lr_layout->set_default( abap_true ).

*     Apresentar dados
      gr_salv_table->display( ).

    CATCH cx_salv_msg.
      WRITE: / 'Exception CX_SALV_MSG'.
    CATCH cx_salv_not_found.
      WRITE: / 'Exception CX_SALV_NOT_FOUND'.
    CATCH cx_salv_data_error.
      WRITE: / 'Exception CX_SALV_DATA_ERROR'.
    CATCH cx_salv_existing.
      WRITE: / 'Exception CX_SALV_EXISTING'.
  ENDTRY.
ENDFORM.                    " WRITE_ALV
*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
* -> Processamento comandos de utilizador, no ALV
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm     LIKE sy-ucomm
                        ls_selfield TYPE slis_selfield.

  FIELD-SYMBOLS <fs_final> LIKE gt_final.

  CASE r_ucomm.
    WHEN 'MAILSEND'.
      PERFORM preparar_mail.
    WHEN 'REFRESH'.
      PERFORM check_dados.
      PERFORM get_dados_todos.
      IF gt_final[] IS INITIAL.
        MESSAGE i016(zhr_generic) WITH text-011.
        LEAVE TO SCREEN 0000.
      ELSE.
        ls_selfield-refresh = 'X'.
      ENDIF.
  ENDCASE.

  LOOP AT gt_final ASSIGNING <fs_final> WHERE box EQ 'X'.
    CLEAR <fs_final>-box.
  ENDLOOP.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  PREPARAR_MAIL
*&---------------------------------------------------------------------*
FORM preparar_mail.

  DATA:
        lv_index     TYPE sy-tabix,
        lv_uzeit     TYPE syuzeit,
        lv_cont      TYPE i,
        lv_cont_char TYPE char10,
        lv_progress  TYPE string,                           "char20,
        lv_answer    TYPE c,
        lv_datum     TYPE sy-datum,
        lv_string    TYPE string,
        lv_total     TYPE  string.

  FIELD-SYMBOLS <fs_final> LIKE gt_final.
  DATA lt_final_aux LIKE gt_final OCCURS 0 WITH HEADER LINE.
  DATA: lt_rsparams  TYPE rsparams       OCCURS 0 WITH HEADER LINE.
  DATA wa_ti004 TYPE zhr_recibo_log.

* Caso não sejam seleccionados registos, dar msg
  IF lt_rows[] IS INITIAL.
    MESSAGE text-030 TYPE 'I'.
    RETURN.
  ENDIF.

*** remover as linhas nao selecionadas
  REFRESH lt_final_aux.
  LOOP AT lt_rows INTO la_rows.
    READ TABLE gt_final ASSIGNING <fs_final> INDEX la_rows-index.
    APPEND <fs_final> TO lt_final_aux.
  ENDLOOP.
  gt_final[] = lt_final_aux[].

***
  DESCRIBE TABLE lt_final_aux LINES lv_cont.

  lv_total = lv_cont.
  CLEAR lv_cont_char.
  CONDENSE lv_total NO-GAPS.

  PERFORM popup_to_confirm USING lv_answer.

  DATA: print_parameters TYPE pri_params,
        valid_flag(1) TYPE c.

*** check se algum user é para imprimir
  lv_index = 1.
  READ TABLE gt_final WITH KEY mail = ''.
  IF sy-subrc = 4.
    lv_index = 0.
  ENDIF.

*** se é para imprimir mostra janela de opcoes da printer
  IF lv_index = 1.
    CALL FUNCTION 'GET_PRINT_PARAMETERS'
*  EXPORTING
*    archive_mode           = '3'
      IMPORTING
        out_parameters         = print_parameters
*    out_archive_parameters = archi_parameters
        valid                  = valid_flag
      EXCEPTIONS
        invalid_print_params   = 2
        OTHERS                 = 4.
*    print_parameters-primm = ' '.
  ENDIF.

  IF lv_answer EQ '1'.
    GET TIME.
    lv_uzeit = sy-uzeit.
    lv_datum = sy-datum.
    LOOP AT gt_final ASSIGNING <fs_final> .
*      CLEAR lt_hcm_ti004.

      IF <fs_final>-mail IS INITIAL.
        IF valid_flag = 'X'.
          CHECK NOT print_parameters-pdest IS INITIAL.
          SUBMIT (c_prog_impressao_recibo)
                  TO SAP-SPOOL
                  SPOOL PARAMETERS print_parameters
                  WITHOUT SPOOL DYNPRO
                  WITH pnptimed      = '2'
                  WITH pnpxabkr      = pnpxabkr
                  WITH pnppabrp      = gv_mes
                  WITH pnppabrj      = gv_ano
                  WITH pnppernr-low  = <fs_final>-pernr
                  WITH p_nostat      = 'X'
                  WITH p_popup       = ' '
                  AND RETURN.
        ENDIF.
      ELSE.
        wa_ti004-mandt      = sy-mandt.
        wa_ti004-pernr      = <fs_final>-pernr.
        wa_ti004-entry_date = lv_datum. "sy-datum.
        wa_ti004-mes        = gv_mes.
        wa_ti004-ano        = gv_ano.
        wa_ti004-entry_time = lv_uzeit.
        wa_ti004-send       = 'X'. "'S'.
        wa_ti004-userid     = sy-uname.
        <fs_final>-estado_icon = '@5B@'.
        <fs_final>-log         = text-001.

        MODIFY zhr_recibo_log FROM wa_ti004.
        IF sy-subrc <> 0.
          ROLLBACK WORK.
          CONCATENATE text-024 wa_ti004-pernr
            INTO lv_string SEPARATED BY space.
          MESSAGE lv_string TYPE 'I'.
          CONTINUE.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.

        PERFORM insert_rsparams TABLES: lt_rsparams USING 'P_PERNR' 'S' <fs_final>-pernr,
                                        lt_rsparams USING 'P_FOLHA' 'P' pnpxabkr,
                                        lt_rsparams USING 'P_ANO'   'P' gv_ano,
                                        lt_rsparams USING 'P_MES'   'P' gv_mes,
                                        lt_rsparams USING 'P_DATA'  'P' lv_datum, "sy-datum,
                                        lt_rsparams USING 'P_HORA'  'P' lv_uzeit.

        SUBMIT zhr_envia_rec_sub WITH SELECTION-TABLE lt_rsparams
                                AND RETURN.

        lv_cont_char = lv_cont_char + 1.
        CONDENSE lv_cont_char NO-GAPS.

        CONCATENATE text-025 lv_cont_char text-026 lv_total
          text-027 INTO lv_progress SEPARATED BY space.

        PERFORM sapgui_progress_indicator USING lv_progress.
        SUBTRACT 1 FROM lv_cont.

        REFRESH lt_rsparams. CLEAR lt_rsparams.
      ENDIF.
    ENDLOOP.
  ENDIF.
  COMMIT WORK AND WAIT.

ENDFORM.                    " PREPARAR_MAIL
*&---------------------------------------------------------------------*
*&      Form  read_rsparams
*&---------------------------------------------------------------------*
FORM insert_rsparams  TABLES   tp_rsparams STRUCTURE rsparams
                      USING    p_selname TYPE any
                               p_kind TYPE any
                               p_low TYPE any.

  DATA wa_rsparams TYPE rsparams.

  wa_rsparams-selname = p_selname.
  wa_rsparams-kind    = p_kind.
  wa_rsparams-sign    = 'I'.
  wa_rsparams-option  = 'EQ'.
  wa_rsparams-low     = p_low.

  APPEND wa_rsparams TO tp_rsparams.

ENDFORM.                    " read_rsparams
*&---------------------------------------------------------------------*
*&      Form  sapgui_progress_indicator
*&---------------------------------------------------------------------*
FORM sapgui_progress_indicator  USING    p_text TYPE any.

  CHECK sy-batch IS INITIAL.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = p_text.

ENDFORM.                    " sapgui_progress_indicator

*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM popup_to_confirm  USING    pv_answer TYPE any.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = text-007
      text_question         = text-008
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      display_cancel_button = ''
    IMPORTING
      answer                = pv_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

ENDFORM.                    " POPUP_TO_CONFIRM

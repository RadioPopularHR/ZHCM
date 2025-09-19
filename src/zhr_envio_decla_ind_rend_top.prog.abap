*&---------------------------------------------------------------------*
*&  Include           ZHR_ENVIO_DECLA_IND_REND_TOP
*&---------------------------------------------------------------------*

INFOTYPES: 0000, 0001, 0002, 0105, 0331.

TABLES: pernr.   " Estrutura PERNR para a Base de Dados Lógica

TYPES: BEGIN OF ty_pernr,
         pernr TYPE p_pernr,
       END OF ty_pernr,

       BEGIN OF ty_pernr_actv,
         pernr TYPE p0000-pernr,
         stat2 TYPE p0000-stat2,
       END OF ty_pernr_actv,

       BEGIN OF ty_pa0000,
         pernr TYPE  p0000-pernr,
         subty TYPE  p0000-subty,
         objps TYPE  p0000-objps,
         sprps TYPE  p0000-sprps,
         endda TYPE  p0000-endda,
         begda TYPE  p0000-begda,
         seqnr TYPE  p0000-seqnr,
         stat2 TYPE  p0000-stat2,
       END OF ty_pa0000,

       BEGIN OF ty_pa0001,
         pernr TYPE  p0001-pernr,
         subty TYPE  p0001-subty,
         objps TYPE  p0001-objps,
         sprps TYPE  p0001-sprps,
         endda TYPE  p0001-endda,
         begda TYPE  p0001-begda,
         seqnr TYPE  p0001-seqnr,
         bukrs TYPE  p0001-bukrs,
         werks TYPE  p0001-werks,
         persg TYPE  p0001-persg,
         persk TYPE  p0001-persk,
         btrtl TYPE  p0001-btrtl,
       END OF ty_pa0001,

       BEGIN OF ty_pa0002,
         pernr TYPE  p0002-pernr,
         subty TYPE  p0002-subty,
         objps TYPE  p0002-objps,
         sprps TYPE  p0002-sprps,
         endda TYPE  p0002-endda,
         begda TYPE  p0002-begda,
         seqnr TYPE  p0002-seqnr,
         cname TYPE  p0002-cname,
       END OF ty_pa0002,

       BEGIN OF ty_final_alv,
         icon     TYPE icon_d, " icon
         pernr    TYPE p_pernr,                          " 0001
         cname    TYPE pad_cname,
         subarea  TYPE char30,                     " 0002
         bukrs    TYPE bukrs,                            " 0001
         year_sel TYPE gjahr,
         stat2    TYPE stat2,                             " 0000
*         preview TYPE icon_d,
         preview  TYPE icon_int,
         email    TYPE comm_id_long,
       END OF ty_final_alv,

       BEGIN OF ty_log_alv,
         pernr TYPE p_pernr,
         log   TYPE bapi_msg,

       END OF ty_log_alv.

*       ty_send_data type table of zhr_env_dir.

DATA: gt_pernr         TYPE TABLE OF ty_pernr,
      gt_final_alv     TYPE TABLE OF ty_final_alv,
      gt_log_alv       TYPE TABLE OF ty_log_alv,
      wa_pernr         TYPE ty_pernr,
      wa_final_alv     TYPE ty_final_alv,
      wa_log_alv       TYPE ty_log_alv,
      gt_pa0002        TYPE STANDARD TABLE OF ty_pa0002,
      wa_pa0002        TYPE ty_pa0002,
      gt_pa0001        TYPE STANDARD TABLE OF ty_pa0001,
      wa_pa0001        TYPE ty_pa0001,
      gt_pa0000        TYPE STANDARD TABLE OF ty_pa0000,
      wa_pa0000        TYPE ty_pa0000,
      gv_year          TYPE pnppabrj,
      gv_pdf           TYPE fpcontent,
      gv_dir           TYPE string,
      gv_cancel_pdf    TYPE flag,
      gv_fich          TYPE string,
      gv_ini_marco     TYPE p0001-begda,
      gv_fim_marco     TYPE p0001-endda,
      gv_uzeit         TYPE sy-uzeit,
      gv_datum         TYPE sy-datum,
      gt_icon          TYPE TABLE OF icon,
      wa_icon          TYPE icon,
      lt_data          TYPE STANDARD TABLE OF x255,
      lv_url           TYPE char255,
      gv_iniano        TYPE p0001-begda,
      gv_fimano        TYPE p0001-endda,
      gv_content       TYPE xstring,
      btrtl_aux        TYPE btext,
      lt_pernr_activos TYPE TABLE OF ty_pernr_actv,
      ls_pernr_activos TYPE ty_pernr_actv,
*MRC@ROFF - 26.01.2012 - Inicio
      wa_gt_final_alv  TYPE ty_final_alv.
*MRC@ROFF - 26.01.2012 - Fim

DATA: gr_salv_table TYPE REF TO cl_salv_table, "Declaração da ALV Grid
      gr_functions  TYPE REF TO cl_salv_functions_list, "Funções da ALV
      gr_display    TYPE REF TO cl_salv_display_settings, "Display Settings
      gr_columns    TYPE REF TO cl_salv_columns_table,  "Atributos colunas
      gr_column     TYPE REF TO cl_salv_column_table,   "Coluna Individual
      gr_selections TYPE REF TO cl_salv_selections, "Sel Multiplas linhas
      gr_events     TYPE REF TO cl_salv_events_table, "Capturar eventos
      gr_log_salv   TYPE REF TO cl_salv_table, "ALV de Log de Erros
      gr_container  TYPE REF TO cl_gui_container, "Container
      gr_dock_cont  TYPE REF TO cl_gui_docking_container. "Docking Container

CLASS lcl_handle_events DEFINITION DEFERRED.

DATA: event_handler TYPE REF TO lcl_handle_events.

* Aplicação de LOG's
DATA:
****************************************************************
  gc_object     TYPE balobj_d  VALUE 'ZHR_DIR',
  gc_subobj     TYPE balsubobj VALUE 'ZHR_ENVIO_DIR',
****************************************************************
  gc_logcl1     LIKE balmi-probclass VALUE '1',
  gc_logcl2     LIKE balmi-probclass VALUE '2',
  gc_logcl3     LIKE balmi-probclass VALUE '3',
  gc_logcl4     LIKE balmi-probclass VALUE '4',
  gc_msgty_head LIKE syst-msgty      VALUE 'H',
  gc_msgty_succ LIKE syst-msgty      VALUE 'S',
  gc_msgty_info LIKE syst-msgty      VALUE 'I',
  gc_msgty_warn LIKE syst-msgty      VALUE 'W',
  gc_msgty_err  LIKE syst-msgty      VALUE 'E',
  gc_msgty_aben LIKE syst-msgty      VALUE 'A',
  gt_tab_msg    TYPE STANDARD TABLE OF balmi.

* Ecrã para Subject e Body do Email
DATA: gv_mail_subject         TYPE so_obj_des.
DATA: gt_mail_body TYPE soli_tab,
      wa_mail_body TYPE soli,
      gv_text_body TYPE string.
DATA: gv_cancel_mail          TYPE flag.
DATA: gr_text_container   TYPE REF TO cl_gui_custom_container,
      gc_tex_control_name TYPE scrfname VALUE 'TEXT_CONTAINER',
      gr_text_editor      TYPE REF TO cl_gui_textedit.
DATA: lo_docking_container TYPE REF TO cl_gui_docking_container,
      g_html_control       TYPE REF TO cl_gui_html_viewer.

** >>> INI ROFF SAM HR MN/EMP 7000050142 07.02.2018
*DATA: gt_attach_bin TYPE TABLE OF soli,
*      gs_ctrl       TYPE zhr_ctrl.
** <<< END ROFF SAM HR MN/EMP 7000050142 07.02.2018




* >>> INI ROFF SAM EMP/SM HR 7000089707 21.02.2020
* Comentado
*PARAMETERS pa_year TYPE pnppabrj OBLIGATORY DEFAULT sy-datum.

SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE TEXT-018.
PARAMETERS pa_year TYPE pnppabrj OBLIGATORY DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK 1.

SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE TEXT-019.
PARAMETERS: subcat TYPE pptp_incsbcat AS CHECKBOX DEFAULT ''.
SELECTION-SCREEN END OF BLOCK 2.

* <<< END ROFF SAM EMP/SM HR 7000089707 21.02.2020

*&---------------------------------------------------------------------*
*& Report  ZHR_ENVIA_REC_SUB
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zhr_envia_rec_sub.
TABLES: pa0000.

TYPES: BEGIN OF tp_ctrl,
        sender        TYPE xubname,
        subject       TYPE so_obj_des,
        spool         TYPE rspoid,
        attach_as_msg TYPE c,
        attach_type   TYPE so_obj_tp,
        subrc         LIKE sy-subrc,
       END OF tp_ctrl.

* Constantes ***********************************************************
CONSTANTS: k_send     TYPE char1     VALUE 'X',"'S',
           k_timed    TYPE char1     VALUE '2',
           k_nostat   TYPE char1     VALUE 'X',
           k_endda    TYPE endda     VALUE '99991231',
           c_e        TYPE c         VALUE 'E',
           k_type_pdf             TYPE so_obj_tp VALUE 'PDF',
           k_type_raw             TYPE so_obj_tp VALUE 'RAW'.

* Variaveis ************************************************************
DATA: gt_rsporq TYPE STANDARD TABLE OF rsporq.
DATA: wa_ti004     TYPE zhr_recibo_log.
DATA: lt_ti004 TYPE zhr_recibo_log OCCURS 0 WITH HEADER LINE,
      gt_p0105 TYPE pa0105         OCCURS 0 WITH HEADER LINE.

DATA: wa_mail LIKE zhr_email_recb.

* Ecrã *****************************************************************
SELECT-OPTIONS: p_pernr FOR pa0000-pernr.

PARAMETER: p_folha  TYPE pnpxabkr,
           p_ano    TYPE pnppabrj,
           p_mes    TYPE pnppabrp,
           p_data   TYPE sy-datum,
           p_hora   TYPE sy-uzeit.

* Start of Selection ***************************************************
START-OF-SELECTION.

  REFRESH: gt_p0105, lt_ti004.

  DATA lv_object_id TYPE so_obj_id.

  SELECT * FROM zhr_recibo_log
           INTO TABLE lt_ti004
          WHERE pernr   IN p_pernr
            AND mes           EQ p_mes
            AND ano           EQ p_ano
            AND entry_date    = p_data
            AND entry_time    = p_hora
            AND send          EQ k_send.

  CHECK NOT lt_ti004[] IS INITIAL.

* Registro mestre HR infotipo 0105 (comunicações)
  SELECT * FROM pa0105 INTO TABLE gt_p0105
      FOR ALL ENTRIES IN lt_ti004
       WHERE pernr EQ lt_ti004-pernr
         AND subty = '0010'
         AND endda EQ k_endda.

  LOOP AT lt_ti004.
    CLEAR gt_p0105.
    READ TABLE gt_p0105 WITH KEY pernr = lt_ti004-pernr BINARY SEARCH.
    CHECK NOT gt_p0105-usrid_long IS INITIAL.
    SUBMIT /1pyxxfo/ysap_payslip_prnt
                                          WITH pnptimed      = k_timed
                                          WITH pnpxabkr      = p_folha
                                          WITH pnppabrp      = p_mes
                                          WITH pnppabrj      = p_ano
                                          WITH pnppernr-low  = lt_ti004-pernr
                                          WITH p_nostat      = k_nostat
                                          WITH p_popup       = ' '
                                    AND RETURN.
    CLEAR wa_mail.
    SELECT SINGLE * FROM zhr_email_recb INTO wa_mail.
    CLEAR wa_ti004.
    SELECT SINGLE * FROM zhr_recibo_log
                    INTO wa_ti004
                   WHERE pernr EQ lt_ti004-pernr
                     AND mes           EQ p_mes
                     AND ano           EQ p_ano
                     AND entry_date    = p_data
                     AND entry_time    = p_hora.
    IF wa_ti004-spoolid IS NOT INITIAL.
      PERFORM process_email USING wa_ti004-pernr
                                  wa_ti004-spoolid
                                  lv_object_id
                                  gt_p0105-usrid_long
                                  wa_mail-userid.
      PERFORM delete_spool USING wa_ti004-spoolid.
    ENDIF.
    PERFORM actualiza_tab USING wa_ti004-pernr
                                wa_ti004-spoolid
                                lv_object_id.
    CLEAR: lv_object_id.
    REFRESH gt_rsporq.

  ENDLOOP.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  PROCESS_EMAIL
*&---------------------------------------------------------------------*
FORM process_email  USING    pv_pernr TYPE any
                             pv_rqident TYPE any
                             pv_object_id TYPE any
                             pv_email TYPE any
                             pv_user TYPE any.

  DATA: lv_email           LIKE somlreci1-receiver,
        lv_subject         LIKE sodocchgi1-obj_descr,
        lv_attachment_desc TYPE so_obj_des,
        lv_data            TYPE char8,
        lv_rspoid          TYPE rspoid,
        lv_pernr_ch        TYPE char8.
  DATA: lt_mess_bod        LIKE solisti1  OCCURS 0 WITH HEADER LINE,
        lt_receivers       LIKE somlreci1 OCCURS 0 WITH HEADER LINE.
  DATA: it_attach_bin	     TYPE TABLE OF soli.
  DATA: it_attach_raw      TYPE TABLE OF  solisti1.
  DATA: ls_ctrl            TYPE tp_ctrl.

  lv_email = pv_email.
  lv_subject = text-001. "Recibo de Vencimento de
  CONCATENATE p_mes '.' p_ano INTO lv_data.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = pv_pernr
    IMPORTING
      output = lv_pernr_ch.

  CONCATENATE lv_pernr_ch '-' lv_subject lv_data
         INTO lv_subject
    SEPARATED BY space.

  lv_attachment_desc = lv_subject.

*******
  lt_mess_bod =
  'Enviamos em anexo o seu recibo de vencimentos correspondente ao período de &.&.'.
  REPLACE FIRST OCCURRENCE OF '&' IN lt_mess_bod WITH p_mes.
  REPLACE FIRST OCCURRENCE OF '&' IN lt_mess_bod WITH p_ano.
  APPEND lt_mess_bod.

  CLEAR lt_mess_bod.
  APPEND lt_mess_bod.
  APPEND lt_mess_bod.

  lt_mess_bod = 'Melhores Cumprimentos,'.
  APPEND lt_mess_bod.

  lt_mess_bod = 'Direcção de Recursos Humanos'.
  APPEND lt_mess_bod.

  lt_receivers-receiver = lv_email.
  lt_receivers-rec_type = 'U'.
  APPEND lt_receivers.

  lv_rspoid = pv_rqident.

  ls_ctrl-sender        = pv_user.
  ls_ctrl-subject       = lv_subject.
  ls_ctrl-spool         = lv_rspoid.
  ls_ctrl-attach_as_msg = ''.
  ls_ctrl-attach_type   = 'PDF'.

  PERFORM valida_user_sap USING    pv_user
                          CHANGING sy-subrc.
  IF NOT sy-subrc IS INITIAL.
    EXIT.
  ENDIF.

  IF NOT lv_rspoid IS INITIAL.

    PERFORM get_abapspool TABLES   it_attach_bin
                          CHANGING ls_ctrl.

    IF ls_ctrl-subrc NE 0.
      EXIT.
    ENDIF.
  ENDIF.

  PERFORM compose_and_send_mail TABLES   lt_mess_bod
                                         it_attach_bin
                                         it_attach_raw
                                         lt_receivers
                                USING    lv_attachment_desc
                                CHANGING ls_ctrl
                                         pv_object_id.

ENDFORM.                    " PROCESS_EMAIL

*&---------------------------------------------------------------------*
*&      Form  DELETE_SPOOL
*&---------------------------------------------------------------------*
FORM delete_spool  USING    pv_rqident TYPE any.

  DATA: lv_spool_nr TYPE tsp01_sp0r-rqid_char.

  lv_spool_nr = pv_rqident.
  CALL FUNCTION 'RSPO_R_RDELETE_SPOOLREQ'
    EXPORTING
      spoolid = lv_spool_nr.

ENDFORM.                    " DELETE_SPOOL

*&---------------------------------------------------------------------*
*&      Form  ACTUALIZA_TAB
*&---------------------------------------------------------------------*
FORM actualiza_tab  USING    pv_pernr TYPE any
                             pv_spool TYPE any
                             pv_object_id TYPE any.

  DATA: wa_itab  TYPE  zhr_recibo_log,
        lt_sost  TYPE  sost OCCURS 0 WITH HEADER LINE.

  IF pv_object_id IS NOT INITIAL.
    SELECT * FROM sost
             INTO TABLE lt_sost
            WHERE objtp EQ pv_object_id+0(3)
              AND objyr EQ pv_object_id+3(2)
              AND objno EQ pv_object_id+5(12).

    SORT lt_sost BY counter DESCENDING.
    READ TABLE lt_sost INDEX 1.
    IF sy-subrc EQ 0.
      wa_itab-estado = lt_sost-msgty.
    ENDIF.
  ELSE.
    wa_itab-estado = c_e.
  ENDIF.

  wa_itab-mandt         = sy-mandt.
  wa_itab-pernr      = pv_pernr.
  wa_itab-mes           = p_mes.
  wa_itab-ano           = p_ano.
  wa_itab-entry_date    = p_data.
  wa_itab-entry_time    = p_hora.
  wa_itab-spoolid       = pv_spool.
  wa_itab-objectid      = pv_object_id.
  wa_itab-send          = space. "'N'.
  wa_itab-userid        = sy-uname.

  MODIFY  zhr_recibo_log FROM wa_itab.
  COMMIT WORK AND WAIT.

ENDFORM.                    " ACTUALIZA_TAB  .

*&---------------------------------------------------------------------*
*&      Form  valida_user_sap
*&---------------------------------------------------------------------*
FORM valida_user_sap  USING    pv_sender TYPE  xubname
                      CHANGING pv_subrc TYPE sy-subrc.

  DATA: lv_splg TYPE xusplg.

  SELECT SINGLE splg INTO lv_splg
    FROM usr01
    WHERE bname = pv_sender.

  pv_subrc = sy-subrc.

ENDFORM.                    " valida_user_sap

*&---------------------------------------------------------------------*
*&      Form  GET_ABAPSPOOL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ATTACH_BIN  text
*      <--P_LS_CTRL  text
*----------------------------------------------------------------------*
FORM get_abapspool  TABLES   pt_attach_bin
                    CHANGING ps_ctrl    TYPE tp_ctrl.
*
  IF ps_ctrl-attach_type EQ k_type_pdf.
    PERFORM get_pdf_spool  TABLES   pt_attach_bin
                           CHANGING ps_ctrl.
  ELSE.
    CALL FUNCTION 'RSPO_RETURN_SPOOLJOB'
      EXPORTING
        rqident              = ps_ctrl-spool
      IMPORTING
        real_type            = ps_ctrl-attach_type
      TABLES
        buffer               = pt_attach_bin
      EXCEPTIONS
        no_such_job          = 1
        job_contains_no_data = 2
        selection_empty      = 3
        no_permission        = 4
        can_not_access       = 5
        read_error           = 6
        type_no_match        = 7
        OTHERS               = 8.

    ps_ctrl-subrc = sy-subrc.
  ENDIF.

  IF ps_ctrl-subrc <> 0.
    EXIT.
  ENDIF.

* -- Comprime os dados em format 'RAW' para binário
  IF ps_ctrl-attach_type = k_type_raw.
    PERFORM compress_table  TABLES pt_attach_bin.
  ENDIF.

ENDFORM.                    " get_abapspool

*&---------------------------------------------------------------------*
*&      Form  compose_and_send_mail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_MESSAGE     text
*      -->PT_ATTACH_BIN  text
*      -->PT_ATTACH_RAW  text
*      -->PT_RECEIVERS   text
*      -->PV_ATTAC_DESC  text
*      -->PS_CTRL        text
*      -->NEW_OBJECT_ID  text
*----------------------------------------------------------------------*
FORM compose_and_send_mail  TABLES   pt_message     STRUCTURE solisti1
                                     pt_attach_bin  STRUCTURE soli
                                     pt_attach_raw  STRUCTURE solisti1
                                     pt_receivers   STRUCTURE somlreci1
                            USING    pv_attac_desc TYPE so_obj_des
                            CHANGING ps_ctrl       TYPE tp_ctrl
                                     new_object_id TYPE sofolenti1-object_id.

  DATA: lt_docdata    LIKE sodocchgi1,
        lt_objpack    LIKE sopcklsti1  OCCURS  1 WITH HEADER LINE,
        lt_objhead    LIKE solisti1    OCCURS  1 WITH HEADER LINE.

  DATA: lv_lines_msg     TYPE i,
        lv_lines_attach  TYPE i,
        lv_uname         LIKE sy-uname.

  lv_uname = sy-uname.
  sy-uname = ps_ctrl-sender.

  IF sy-sysid NE 'PRD'.
    CONCATENATE sy-sysid '-' ps_ctrl-subject
           INTO ps_ctrl-subject SEPARATED BY space.
  ENDIF.

* -- Create Message Body
*
* -- -- Title, Description and Expiration Date
  lt_docdata-obj_name   = 'MAIL_INTERNET'.
  lt_docdata-obj_descr  = ps_ctrl-subject.
  lt_docdata-obj_expdat = lt_docdata-expiry_dat = sy-datum + 180.
  lt_docdata-obj_langu  = 'P'.
  lt_docdata-priority   = '1'.

* -- -- Main Text

* -- -- Write Packing List (Main)

  IF ps_ctrl-attach_as_msg IS INITIAL.
    DESCRIBE TABLE pt_message LINES lv_lines_msg.
    READ     TABLE pt_message INDEX lv_lines_msg.
    lt_docdata-doc_size =
                    ( lv_lines_msg - 1 ) * 255 + STRLEN( pt_message ).
    CLEAR lt_objpack-transf_bin.
    lt_objpack-head_start = 1.
    lt_objpack-head_num   = 0.
    lt_objpack-body_start = 1.
    lt_objpack-body_num   = lv_lines_msg.
    lt_objpack-doc_type   = k_type_raw.
    APPEND lt_objpack.
  ELSE.
    REFRESH pt_message.
  ENDIF.

* -- Create Message Attachment
* -- -- Write Packing List (Attachment)

  CASE ps_ctrl-attach_type.
    WHEN k_type_raw.
      APPEND LINES OF pt_attach_raw TO pt_message.
      DESCRIBE TABLE pt_attach_raw LINES lv_lines_attach.
      READ     TABLE pt_attach_raw INDEX lv_lines_attach.
      lt_objpack-doc_size =
               ( lv_lines_attach - 1 ) * 255 + STRLEN( pt_attach_raw ).
      CLEAR lt_objpack-transf_bin.
      lt_objpack-body_start = lv_lines_msg + 1.
      lt_objpack-body_num   = lv_lines_attach.

    WHEN OTHERS.
      DESCRIBE TABLE pt_attach_bin LINES lv_lines_attach.
      READ     TABLE pt_attach_bin INDEX lv_lines_attach.
      lt_objpack-doc_size =
               ( lv_lines_attach - 1 ) * 255 + STRLEN( pt_attach_bin ).
      lt_objpack-transf_bin = 'X'.
      lt_objpack-body_start = 1.
      lt_objpack-body_num   = lv_lines_attach.
  ENDCASE.

  lt_objpack-head_start = 1.
  lt_objpack-head_num   = 0.
  lt_objpack-doc_type   = ps_ctrl-attach_type.
  lt_objpack-obj_name   = 'ATTACHMENT'.

  IF pv_attac_desc IS INITIAL.
    lt_objpack-obj_descr  = 'Attached Document'.
  ELSE.
    lt_objpack-obj_descr  = pv_attac_desc.
  ENDIF.

  IF NOT ps_ctrl-spool IS INITIAL         OR
     NOT ps_ctrl-attach_as_msg IS INITIAL.
    APPEND lt_objpack.
  ENDIF.

  DATA: lv_sent_to_all LIKE  sonv-flag.
  lv_sent_to_all = ''.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = lt_docdata
      put_in_outbox              = 'X'
      commit_work                = 'X'     "used from rel.6.10
    IMPORTING
      sent_to_all                = lv_sent_to_all
      new_object_id              = new_object_id
    TABLES
      packing_list               = lt_objpack
      object_header              = lt_objhead
      contents_bin               = pt_attach_bin
      contents_txt               = pt_message
      receivers                  = pt_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  ps_ctrl-subrc = sy-subrc.

  sy-uname = lv_uname.

ENDFORM.                    " compose_and_send_mail
*&---------------------------------------------------------------------*
*&      Form  compress_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_ATTACH  text
*----------------------------------------------------------------------*
FORM compress_table  TABLES   pt_attach.

  CALL FUNCTION 'TABLE_COMPRESS'
    TABLES
      in     = pt_attach
      out    = pt_attach
    EXCEPTIONS
      OTHERS = 1.

ENDFORM.                    " compress_table
*&---------------------------------------------------------------------*
*&      Form  get_pdf_spool
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_ATTACH_BIN  text
*      -->PS_CTRL        text
*----------------------------------------------------------------------*
FORM get_pdf_spool  TABLES   pt_attach_bin
                    CHANGING ps_ctrl    TYPE tp_ctrl.

  DATA: BEGIN OF lt_pdf_output OCCURS 0.
          INCLUDE STRUCTURE tline.
  DATA: END OF lt_pdf_output.

  DATA: lv_buffer   TYPE string,
        lv_function TYPE char30.

  DATA wa_tsp01 TYPE tsp01.

  DATA lv_objtype TYPE rststyname.

  SELECT SINGLE * FROM tsp01
                  INTO wa_tsp01
                 WHERE rqident EQ ps_ctrl-spool.

  CALL FUNCTION 'RSTS_GET_ATTRIBUTES'
    EXPORTING
      authority     = 'SP01'
      name          = wa_tsp01-rqo1name
    IMPORTING
      objtype       = lv_objtype
    EXCEPTIONS
      fb_error      = 1
      fb_rsts_other = 2
      no_object     = 3
      no_permission = 4
      OTHERS        = 5.


  IF lv_objtype EQ 'TEXT'.
*   Spool normal
    lv_function = 'CONVERT_ABAPSPOOLJOB_2_PDF'.
  ELSE.
*   Spool de formularios
    lv_function = 'CONVERT_OTFSPOOLJOB_2_PDF'.
  ENDIF.

  CALL FUNCTION lv_function
    EXPORTING
      src_spoolid              = ps_ctrl-spool
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

  ps_ctrl-subrc = sy-subrc.
  CHECK ps_ctrl-subrc EQ 0.

  LOOP AT lt_pdf_output.
    TRANSLATE lt_pdf_output USING ' ~'.
    CONCATENATE lv_buffer lt_pdf_output INTO lv_buffer.
  ENDLOOP.

  TRANSLATE lv_buffer USING '~ '.

  DO.
    pt_attach_bin = lv_buffer.
    APPEND pt_attach_bin.
    SHIFT lv_buffer LEFT BY 255 PLACES.
    IF lv_buffer IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " GET_PDF_SPOOL

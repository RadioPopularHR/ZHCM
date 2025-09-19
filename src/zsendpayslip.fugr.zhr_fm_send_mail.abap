FUNCTION zhr_fm_send_mail.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_SENDER) TYPE  XUBNAME
*"     REFERENCE(IV_SUBJECT) TYPE  SO_OBJ_DES
*"     REFERENCE(IV_SPOOL) TYPE  RSPOID
*"     REFERENCE(IV_ATTACH_AS_MSG) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(IV_ATTACH_TYPE) TYPE  SO_OBJ_TP DEFAULT 'RAW'
*"     REFERENCE(IV_REPID) TYPE  REPID DEFAULT SY-REPID
*"     REFERENCE(IV_ATTACHMENT_DESC) TYPE  SO_OBJ_DES OPTIONAL
*"     REFERENCE(IV_CTRL) TYPE  ZHR_CTRL OPTIONAL
*"  EXPORTING
*"     VALUE(NEW_OBJECT_ID) TYPE  SOFOLENTI1-OBJECT_ID
*"  TABLES
*"      IT_MESSAGE STRUCTURE  SOLISTI1 OPTIONAL
*"      IT_ATTACH_BIN STRUCTURE  SOLI OPTIONAL
*"      IT_ATTACH_RAW STRUCTURE  SOLISTI1 OPTIONAL
*"      IT_RECEIVERS STRUCTURE  SOMLRECI1 OPTIONAL
*"  EXCEPTIONS
*"      TOO_MANY_RECEIVERS
*"      DOCUMENT_NOT_SENT
*"      DOCUMENT_TYPE_NOT_EXIST
*"      OPERATION_NO_AUTHORIZATION
*"      PARAMETER_ERROR
*"      X_ERROR
*"      ENQUEUE_ERROR
*"      SAP_USER_ERROR
*"      NO_SUCH_JOB
*"      JOB_CONTAINS_NO_DATA
*"      SELECTION_EMPTY
*"      NO_PERMISSION
*"      CAN_NOT_ACCESS
*"      READ_ERROR
*"      TYPE_NO_MATCH
*"      EMPTY_TABLES
*"      ATTACH_TABLE_EMPTY
*"      RECEIVERS_TABLE_EMPTY
*"      REC_TYPE_EMPTY
*"----------------------------------------------------------------------
*
  DATA: ls_ctrl         TYPE tp_ctrl.
*        gv_obj_id type SOFOLENTI1-OBJECT_ID.

* -- Valida se as tabelas necessárias para a composição do mail estão
* -- vazias e também o Nr de ordem de spool
  IF it_message[]     IS INITIAL AND
     it_attach_bin[]  IS INITIAL AND
     it_attach_raw[]  IS INITIAL AND
     iv_spool         IS INITIAL.
    RAISE empty_tables.
  ENDIF.

* -- Valida se a tabela de destinatários está preenchida
  IF it_receivers[] IS INITIAL.
    RAISE receivers_table_empty.
  ENDIF.

* -- Valida se as tabelas de anexos contêm dados para a composição do
* -- mail.
  IF iv_attach_as_msg = 'X' AND
     it_attach_bin[] IS INITIAL AND
     it_attach_raw[] IS INITIAL AND
     iv_spool IS INITIAL.
    RAISE attach_table_empty.
  ENDIF.

* -- Valida se o tipo de destinatário está preenchido
  LOOP AT it_receivers.
    IF it_receivers-rec_type IS INITIAL.
      RAISE rec_type_empty.
    ENDIF.
  ENDLOOP.

* -- Actualiza campos de controlo
  ls_ctrl-sender        = iv_sender.
  ls_ctrl-subject       = iv_subject.
  ls_ctrl-spool         = iv_spool.
  ls_ctrl-attach_as_msg = iv_attach_as_msg.
  ls_ctrl-attach_type   = iv_attach_type.


* -- Valida se o emissor é um utilizador válido em SAP
  PERFORM valida_user_sap USING    iv_sender
                          CHANGING sy-subrc.

  IF NOT sy-subrc IS INITIAL.
    RAISE sap_user_error.
  ENDIF.



* -- Extrai a ordem de spool
  IF NOT iv_spool IS INITIAL.
** >>> INI ROFF SAM HR MN/EMP 7000050142 07.02.2018
*    IF it_attach_bin[] IS NOT INITIAL.
*      ls_ctrl               = iv_ctrl.
*      ls_ctrl-sender        = iv_sender.
*      ls_ctrl-subject       = iv_subject.
*      ls_ctrl-spool         = iv_spool.
*      ls_ctrl-attach_as_msg = iv_attach_as_msg.
*      ls_ctrl-attach_type   = iv_attach_type.
*
*      CASE ls_ctrl-subrc.
*        WHEN k_no_such_job.
*          RAISE no_such_job.
*        WHEN k_job_contains_no_data.
*          RAISE job_contains_no_data.
*        WHEN k_selection_empty.
*          RAISE selection_empty.
*        WHEN k_no_permission.
*          RAISE no_permission.
*        WHEN k_can_not_access.
*          RAISE can_not_access.
*        WHEN k_read_error.
*          RAISE read_error.
*        WHEN k_type_no_match.
*          RAISE type_no_match.
*      ENDCASE.
*
*    ELSE.
** <<< END ROFF SAM HR MN/EMP 7000050142 07.02.2018
      PERFORM get_abapspool TABLES   it_attach_bin
                            CHANGING ls_ctrl.

      CASE ls_ctrl-subrc.
        WHEN k_no_such_job.
          RAISE no_such_job.
        WHEN k_job_contains_no_data.
          RAISE job_contains_no_data.
        WHEN k_selection_empty.
          RAISE selection_empty.
        WHEN k_no_permission.
          RAISE no_permission.
        WHEN k_can_not_access.
          RAISE can_not_access.
        WHEN k_read_error.
          RAISE read_error.
        WHEN k_type_no_match.
          RAISE type_no_match.
      ENDCASE.
** >>> INI ROFF SAM HR MN/EMP 7000050142 07.02.2018
*    ENDIF.
** <<< END ROFF SAM HR MN/EMP 7000050142 07.02.2018
  ENDIF.


* -- Composição do mail
  PERFORM compose_and_send_mail TABLES   it_message
                                         it_attach_bin
                                         it_attach_raw
                                         it_receivers
                                USING    iv_attachment_desc
                                CHANGING ls_ctrl
                                         new_object_id.

  CASE sy-subrc.
    WHEN k_too_many_receivers.
      RAISE too_many_receivers.
    WHEN k_document_not_sent.
      RAISE document_not_sent.
    WHEN k_document_type_not_exist.
      RAISE document_type_not_exist.
    WHEN k_operation_no_authorization.
      RAISE operation_no_authorization.
    WHEN k_parameter_error.
      RAISE parameter_error.
    WHEN k_x_error.
      RAISE x_error.
    WHEN k_enqueue_error.
      RAISE enqueue_error.
  ENDCASE.
ENDFUNCTION.

*&---------------------------------------------------------------------*
*&      Form  COMPOSE_AND_SEND_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MESSAGE  text
*      -->P_IT_ATTACH_BIN  text
*      -->P_IT_ATTACH_RAW  text
*      -->P_IT_RECEIVERS  text
*      -->P_IV_ATTACHMENT_DESC  text
*      <--P_LS_CTRL  text
*      <--P_NEW_OBJECT_ID  text
*----------------------------------------------------------------------*
FORM compose_and_send_mail  TABLES   pt_message     STRUCTURE solisti1
                                     pt_attach_bin  STRUCTURE soli
                                     pt_attach_raw  STRUCTURE solisti1
                                     pt_receivers
                            USING    pv_attac_desc
                            CHANGING ps_ctrl       TYPE tp_ctrl
                                     new_object_id TYPE sofolenti1-object_id.


  DATA: lt_docdata    LIKE sodocchgi1,
        lt_objpack    LIKE sopcklsti1  OCCURS  1 WITH HEADER LINE,
        lt_objhead    LIKE solisti1    OCCURS  1 WITH HEADER LINE,
        lt_attach     LIKE solisti1    OCCURS 10 WITH HEADER LINE.

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

  DATA: sa TYPE soextreci1-receiver.

  sa = sy-uname.
*  Send Message
*  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
*      EXPORTING
*        document_data              = lt_docdata
*        put_in_outbox              = 'X'
*        sender_address             = sa
*        sender_address_type        = 'INT'
*        commit_work                = 'X'
*      IMPORTING
*        new_object_id              = new_object_id
*      TABLES
*        packing_list               = lt_objpack
*        object_header              = lt_objhead
*        contents_bin               = pt_attach_bin
*        contents_txt               = pt_message
*        receivers                  = pt_receivers
*      EXCEPTIONS
*        too_many_receivers         = 1
*        document_not_sent          = 2
*        document_type_not_exist    = 3
*        operation_no_authorization = 4
*        parameter_error            = 5
*        x_error                    = 6
*        enqueue_error              = 7
*        OTHERS                     = 8.

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
*&      Form  compress_table
*&---------------------------------------------------------------------*
FORM compress_table  TABLES   pt_attach.

  CALL FUNCTION 'TABLE_COMPRESS'
    TABLES
      in     = pt_attach
      out    = pt_attach
    EXCEPTIONS
      OTHERS = 1.

ENDFORM.                    " compress_table

*&---------------------------------------------------------------------*
*&      Form  valida_user_sap
*&---------------------------------------------------------------------*
FORM valida_user_sap  USING    pv_sender
                      CHANGING pv_subrc.

  DATA: lv_splg TYPE xusplg.

  SELECT SINGLE splg INTO lv_splg
    FROM usr01
    WHERE bname = pv_sender.

  pv_subrc = sy-subrc.

ENDFORM.                    " valida_user_sap

*&---------------------------------------------------------------------*
*&      Form  GET_PDF_SPOOL
*&---------------------------------------------------------------------*
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

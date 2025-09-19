*&---------------------------------------------------------------------*
*&  Include           ZHR_ENVIO_DECLA_IND_REND_LOG
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  EXIBE_LOGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VISIVEL    text
*----------------------------------------------------------------------*
FORM exibe_logs USING hide.

* Objecto e SubObjecto p/ Log Aplicação
  DATA: lv_extnum LIKE balhdr-extnumber.

  CALL FUNCTION 'APPL_LOG_DISPLAY'
    EXPORTING
      object                    = gc_object
      subobject                 = gc_subobj
      external_number           = lv_extnum
      date_to                   = sy-datum
      suppress_selection_dialog = hide
    IMPORTING
      number_of_protocols       = sy-dbcnt
    EXCEPTIONS
      no_authority              = 1
      OTHERS                    = 2.

  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

ENDFORM.                    " EXIBE_LOGS
*&---------------------------------------------------------------------*
*&      Form  APP_INIT_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GC_OBJECT  text
*      -->P_GC_SUBOBJ  text
*----------------------------------------------------------------------*
FORM app_init_log  USING    object TYPE balobj_d
                        subobj TYPE balsubobj.

  REFRESH gt_tab_msg.

* Inicializa o log de aplicação
  PERFORM app_log_inicializa USING object
                                   subobj.

  PERFORM app_log_cabecalho USING sy-datum
                                  object
                                  subobj.

ENDFORM.                    " APP_INIT_LOG

*&---------------------------------------------------------------------*
*&      Form  APP_LOG_INICIALIZA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OBJECT  text
*      -->P_SUBOBJ  text
*----------------------------------------------------------------------*
FORM app_log_inicializa  USING    object TYPE balobj_d
                               subobj TYPE balsubobj.

  CALL FUNCTION 'APPL_LOG_INIT'
    EXPORTING
      object              = object
      subobject           = subobj
      log_handle          = ' '
    EXCEPTIONS
      object_not_found    = 1
      subobject_not_found = 2
      OTHERS              = 3.

  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

  CALL FUNCTION 'APPL_LOG_SET_OBJECT'
    EXPORTING
      object              = object
      subobject           = subobj
      log_handle          = ' '
      no_log              = ' '
    EXCEPTIONS
      object_not_found    = 1
      subobject_not_found = 2
      OTHERS              = 3.

  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

ENDFORM.                    " APP_LOG_INICIALIZA
*&---------------------------------------------------------------------*
*&      Form  APP_LOG_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DATUM  text
*      -->P_OBJECT  text
*      -->P_SUBOBJ  text
*----------------------------------------------------------------------*
FORM app_log_cabecalho  USING    i_numext
                              object TYPE balobj_d
                              subobj TYPE balsubobj.

  DATA: ls_header LIKE balhdri.

  ls_header-object    = object.
  ls_header-subobject = subobj.
  ls_header-extnumber = i_numext.
  ls_header-aldate    = sy-datlo.
  ls_header-altime    = sy-uzeit.
  ls_header-aluser    = sy-uname.
  ls_header-altcode   = sy-tcode.
  ls_header-alprog    = sy-cprog.

  CALL FUNCTION 'APPL_LOG_WRITE_HEADER'
    EXPORTING
      header              = ls_header
    EXCEPTIONS
      object_not_found    = 1
      subobject_not_found = 2
      error               = 3
      OTHERS              = 4.

  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.


ENDFORM.                    " APP_LOG_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  APP_END_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GC_OBJECT  text
*      -->P_GC_SUBOBJ  text
*----------------------------------------------------------------------*
FORM app_end_log  USING  object TYPE balobj_d
                         subobj TYPE balsubobj.

  PERFORM app_log_write_messages USING object
                                       subobj.

  PERFORM app_log_save_bd USING object
                                subobj.

ENDFORM.                    " APP_END_LOG
*&---------------------------------------------------------------------*
*&      Form  APP_LOG_WRITE_MESSAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OBJECT  text
*      -->P_SUBOBJ  text
*----------------------------------------------------------------------*
FORM app_log_write_messages  USING    object TYPE balobj_d
                                  subobj TYPE balsubobj.

  CALL FUNCTION 'APPL_LOG_WRITE_MESSAGES'
    EXPORTING
      object              = object
      subobject           = subobj
      update_or_insert    = 'I'
    TABLES
      messages            = gt_tab_msg
    EXCEPTIONS
      object_not_found    = 1
      subobject_not_found = 2
      OTHERS              = 3.

  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

  REFRESH gt_tab_msg.

ENDFORM.                    " APP_LOG_WRITE_MESSAGES
*&---------------------------------------------------------------------*
*&      Form  APP_LOG_SAVE_BD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OBJECT  text
*      -->P_SUBOBJ  text
*----------------------------------------------------------------------*
FORM app_log_save_bd  USING    object TYPE balobj_d
                            subobj TYPE balsubobj.

  DATA: lt_balnri LIKE balnri OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'APPL_LOG_WRITE_DB'
    EXPORTING
      object                = object
      subobject             = subobj
    TABLES
      object_with_lognumber = lt_balnri
    EXCEPTIONS
      object_not_found      = 1
      subobject_not_found   = 2
      internal_error        = 3
      OTHERS                = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " APP_LOG_SAVE_BD

*&---------------------------------------------------------------------*
*&      Form  APP_LOG_NEW_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0434   text
*      -->P_0435   text
*      -->P_0436   text
*      -->P_<FS_FINAL_ALV>_PERNR  text
*      -->P_0438   text
*      -->P_0439   text
*      -->P_0440   text
*----------------------------------------------------------------------*
FORM app_log_new_message  USING     i_msgty
                                    i_msgid
                                    i_msgno
                                    i_msgv1
                                    i_msgv2
                                    i_msgv3
                                    i_msgv4.

  DATA: ls_tab_msg TYPE balmi.

  ls_tab_msg-msgty     = i_msgty.
  ls_tab_msg-msgid     = i_msgid.
  ls_tab_msg-msgno     = i_msgno.

  ls_tab_msg-msgv1     = i_msgv1.
  CONDENSE ls_tab_msg-msgv1.

  ls_tab_msg-msgv2     = i_msgv2.
  CONDENSE ls_tab_msg-msgv2.

  ls_tab_msg-msgv3     = i_msgv3.
  CONDENSE ls_tab_msg-msgv3.

  ls_tab_msg-msgv4     = i_msgv4.
  CONDENSE ls_tab_msg-msgv4.

  CASE i_msgty.
    WHEN gc_msgty_head.
      ls_tab_msg-probclass = gc_logcl2.
    WHEN gc_msgty_succ.
      ls_tab_msg-probclass = gc_logcl4.
    WHEN gc_msgty_info.
      ls_tab_msg-probclass = gc_logcl3.
    WHEN gc_msgty_warn.
      ls_tab_msg-probclass = gc_logcl2.
    WHEN gc_msgty_err.
      ls_tab_msg-probclass = gc_logcl1.
    WHEN gc_msgty_aben.
      ls_tab_msg-probclass = gc_logcl1.
  ENDCASE.

  ls_tab_msg-detlevel = '1'.

  APPEND ls_tab_msg TO gt_tab_msg.
ENDFORM.                    " APP_LOG_NEW_MESSAGE

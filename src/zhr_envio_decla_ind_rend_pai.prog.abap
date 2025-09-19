*&---------------------------------------------------------------------*
*&  Include           ZHR_ENVIO_DECLA_IND_REND_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA ok_code LIKE sy-ucomm.

  MOVE sy-ucomm TO ok_code.
  CASE ok_code.
    WHEN 'EXIT'.
      CALL METHOD lo_docking_container->free.
*      CALL METHOD g_html_control->free.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  gv_cancel_mail = 'X'.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT_DOWNLOAD_PDF  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_download_pdf INPUT.
  gv_cancel_pdf = 'X'.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " EXIT_DOWNLOAD_PDF  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  DATA: ok_code2 TYPE sy-ucomm.

  CASE ok_code2.
    WHEN 'CANCEL'.
      gv_cancel_pdf = 'X'.
      LEAVE TO SCREEN 0.
    WHEN 'DPDF'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_DIR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_dir INPUT.

*  CLEAR gv_dir.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = 'Selecionar Directório'
      initial_folder  = ''
    CHANGING
      selected_folder = gv_dir
    EXCEPTIONS
      cntl_error      = 1
      error_no_gui    = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    CLEAR gv_dir.
  ENDIF.

ENDMODULE.                 " SEARCH_DIR  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  DATA: lv_stop TYPE flag.

  CASE ok_code2.
    WHEN 'CANCEL'.
      gv_cancel_mail = 'X'.
      CALL METHOD gr_text_editor->free.
      CALL METHOD gr_text_container->free.
      LEAVE TO SCREEN 0.
    WHEN 'EMAIL'.
      CLEAR: lv_stop.
      PERFORM check_mail_data CHANGING lv_stop.
      CHECK lv_stop IS INITIAL.
      CALL METHOD gr_text_editor->free.
      CALL METHOD gr_text_container->free.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT

*&---------------------------------------------------------------------*
*&  Include           ZHR_ENVIO_DECLA_IND_REND_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PDF_PREVIEW'.
*  SET TITLEBAR 'xxx'.

  CREATE OBJECT lo_docking_container
    EXPORTING
      repid     = sy-repid
      dynnr     = '100'"SY-DYNNR
      side      = lo_docking_container->dock_at_left
      extension = 1200.

  CREATE OBJECT g_html_control
    EXPORTING
      parent = lo_docking_container.

*   Convert xstring to binary table to pass to the LOAD_DATA method
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = gv_content
      TABLES
        binary_tab = lt_data.


* Load the HTML
  CALL METHOD g_html_control->load_data(
     EXPORTING
       type         = 'application'
       subtype      = 'pdf'
     IMPORTING
       assigned_url         = lv_url
     CHANGING
       data_table           = lt_data
     EXCEPTIONS
       dp_invalid_parameter = 1
       dp_error_general     = 2
       cntl_error           = 3
       OTHERS               = 4 ).

* Show it
  CALL METHOD g_html_control->show_url( url = lv_url
    in_place = 'X' ).

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'MAIL_STATUS'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_0200 OUTPUT.
  CONSTANTS: c_line_length TYPE i VALUE 254.

  CREATE OBJECT gr_text_container
    EXPORTING
      container_name              = gc_tex_control_name
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  CHECK gr_text_container IS NOT INITIAL.

  CREATE OBJECT gr_text_editor
    EXPORTING
      parent                     = gr_text_container
      wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
      wordwrap_position          = c_line_length
      wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

  CALL METHOD gr_text_editor->set_toolbar_mode
    EXPORTING
      toolbar_mode = cl_gui_textedit=>false.

  CALL METHOD gr_text_editor->set_statusbar_mode
    EXPORTING
      statusbar_mode = cl_gui_textedit=>false.

    CALL METHOD gr_text_editor->set_text_as_r3table
      exporting
      table = gt_mail_body.


  IF sy-subrc eq 0.

  ENDIF.
ENDMODULE.                 " INIT_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'MAIL_STATUS'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_0300 OUTPUT.

ENDMODULE.                 " INIT_0300  OUTPUT

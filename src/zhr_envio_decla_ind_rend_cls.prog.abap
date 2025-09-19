*&---------------------------------------------------------------------*
*&  Include           ZHR_ENVIO_DECLA_IND_REND_CLS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,
*     hotspot
      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.

ENDCLASS.                    "lcl_handle_events DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.
    CASE e_salv_function.
      WHEN 'MAILSEND'.
        PERFORM write_mail.
      WHEN 'DOWNPDF'.
        PERFORM download_pdf.
      WHEN 'LOGERRO'.
        PERFORM get_error_log.
    ENDCASE.
  ENDMETHOD.                    "on_user_command

* Handle Hotspot Click
  METHOD on_link_click.
    PERFORM on_link_click USING row column.
  ENDMETHOD.                    "on_link_click

ENDCLASS.                    "lcl_handle_events IMPLEMENTATION

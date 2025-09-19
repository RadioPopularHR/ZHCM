*&---------------------------------------------------------------------*
*&  Include           ZHR_WORKVIVO_INTEGRACAO_TOP
*&---------------------------------------------------------------------*

TABLES: pernr.
*INFOTYPES: 0000, 0001, 0002, 0016, 0302, 9010.
NODES: person, peras.

TYPES: BEGIN OF ty_json,
         pernr TYPE pernr_d,
         line  TYPE string,
       END OF ty_json.

DATA: gv_bearer   TYPE tvarvc-low,
      gs_workvivo TYPE zshr_workvivo_integracao,
      gs_p9010    TYPE p9010,
      gt_workvivo TYPE TABLE OF zshr_workvivo_integracao,
      gt_json     TYPE TABLE OF ty_json.

*----------------------------------------------------------------------*
*       CLASS lcl_events DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_link_click FOR EVENT link_click
                  OF cl_salv_events_table
        IMPORTING row column.
ENDCLASS.                    "lcl_events DEFINITION
DATA: gr_event_handler TYPE REF TO lcl_events.
*----------------------------------------------------------------------*
*       CLASS lcl_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_events IMPLEMENTATION.
  METHOD on_link_click.
    READ TABLE gt_workvivo ASSIGNING FIELD-SYMBOL(<fs_workvivo>) INDEX row.
    IF <fs_workvivo> IS ASSIGNED.
      IF column EQ 'JSON'.
        READ TABLE gt_json INTO DATA(ls_json) WITH KEY pernr = <fs_workvivo>-pernr.
        IF sy-subrc IS INITIAL.
          CALL TRANSFORMATION sjson2html SOURCE XML ls_json-line
                                 RESULT XML DATA(html).
          cl_abap_browser=>show_html( html_string = cl_abap_codepage=>convert_from( html ) ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "on_link_click

ENDCLASS.                    "lcl_events IMPLEMENTATION

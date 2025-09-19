*&---------------------------------------------------------------------*
*&  Include           ZHR_WFM_INTEGRACAO_SCR
*&---------------------------------------------------------------------*
TABLES: pa0000.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
SELECT-OPTIONS: s_pernr FOR pa0000-pernr.
PARAMETERS: p_datum  TYPE datum DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.
PARAMETERS: p_employ TYPE char1 RADIOBUTTON GROUP gr1 DEFAULT 'X',
            p_cont_e TYPE char1 RADIOBUTTON GROUP gr1,
            p_contin TYPE char1 RADIOBUTTON GROUP gr1,
            p_abse_r TYPE char1 RADIOBUTTON GROUP gr1,
            p_abse_o TYPE char1 RADIOBUTTON GROUP gr1,
*            p_sche_o TYPE char1 AS CHECKBOX DEFAULT abap_true,
            p_wage_o TYPE char1 RADIOBUTTON GROUP gr1.
*            p_data_o TYPE char1 RADIOBUTTON GROUP gr1.
SELECTION-SCREEN END OF BLOCK b02.

*IF sy-sysid NE 'DEV'.
SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-b03.
PARAMETERS: p_test TYPE char1 AS CHECKBOX DEFAULT abap_false.
SELECTION-SCREEN END OF BLOCK b03.
*ENDIF.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

*    IF  screen-name EQ 'P_TEST'.
*    IF  screen-name EQ '%BB03013_BLOCK_1000' OR screen-name EQ 'P_TEST'.
    IF  screen-name EQ '%BB03012_BLOCK_1000' OR screen-name EQ 'P_TEST'.
      IF sy-sysid EQ 'PRD'.

        screen-active = '0'. "---FIELD will be disabaled...

        screen-invisible = '1'. "---FIELD will be invisible...
        MODIFY SCREEN .
      ENDIF.


    ENDIF.
  ENDLOOP.

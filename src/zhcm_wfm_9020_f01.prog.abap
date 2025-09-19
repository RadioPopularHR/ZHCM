*&---------------------------------------------------------------------*
*&  Include           ZHCM_WFM_9020_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
FORM process_data.

  DATA: lv_date      TYPE datum,
        lv_first_day TYPE datum,
        lv_last_day  TYPE datum,
        lv_data      TYPE REF TO data,
        ls_return    TYPE bapireturn1,
        ls_key       TYPE bapipakey.

  FIELD-SYMBOLS: <fs_record> TYPE any.


*  lv_date = sy-datum(6) && '01'.
  lv_date = p_datum.


  SELECT * FROM ztwfm_hcm_subtyp
    INTO TABLE @DATA(lt_subtype)."

  SELECT * FROM pa0000
    INTO TABLE @DATA(lt_pa0000)
    WHERE stat2 = '3'
      AND begda LE @lv_date
      AND endda GE @lv_date.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_date
    IMPORTING
      last_day_of_month = lv_last_day
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

  lv_first_day = lv_date(6) && '01'.

  IF sy-subrc EQ 0.

    READ TABLE lt_subtype INTO DATA(ls_subtype) WITH KEY subtype = 9020.
    IF sy-subrc EQ 0.

      LOOP AT lt_pa0000 INTO DATA(ls_pa0000).

        CLEAR: ls_return, ls_key.

        SELECT SINGLE * FROM pa0015
          INTO @DATA(ls_pa0015)
          WHERE pernr = @ls_pa0000-pernr
            AND subty = '9020'
            AND begda BETWEEN @lv_first_day AND @lv_last_day.

        IF sy-subrc NE 0.

          DATA(lv_struct) = 'P' && ls_subtype-infotype.
          CREATE DATA lv_data TYPE (lv_struct).
          ASSIGN lv_data->* TO <fs_record>.

          ASSIGN COMPONENT 'PERNR' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_pernr_9020>).
          ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_subty_9020>).
          ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_begda_9020>).
          ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_endda_9020>).
          ASSIGN COMPONENT 'LGART' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_lgart_9020>).
          ASSIGN COMPONENT 'ANZHL' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_anzhl_9020>).
          ASSIGN COMPONENT 'ZEINH' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_zeinh_9020>).
          ASSIGN COMPONENT 'AEDTM' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_aedtm_9020>).
          ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_waers_9020>).

          <fs_pernr_9020> = ls_pa0000-pernr.
*        <fs_infty_9020> = ls_subtype-infotype.
          <fs_subty_9020> = <fs_lgart_9020> = '9020'.
          <fs_begda_9020> = <fs_endda_9020> = lv_date.
          <fs_anzhl_9020> = 22.
          <fs_zeinh_9020> = '010'.
          <fs_waers_9020> = 'EUR'.
          <fs_aedtm_9020> = sy-datum.

          zcl_hcm_ws_wfm=>infotype_operation(
            EXPORTING
              iv_infotype        = ls_subtype-infotype                 " Infotipo
              iv_employee_number = <fs_pernr_9020>                 " Nº pessoal
              iv_subty           = <fs_subty_9020>               " Subinfotipo
              iv_begda           = <fs_begda_9020>                 " Início da validade
              iv_endda           = <fs_endda_9020>                 " Fim da validade
              iv_operation       = 'INS'                " Operação em infotipos
              iv_commit          = abap_false
              is_record          = <fs_record>
            IMPORTING
              es_return          = ls_return                 " Parâmetro de retorno
              es_key             = ls_key                 " Chave para dados mestre HR
          ).

          UNASSIGN <fs_record>.

        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

*  COMMIT WORK.


ENDFORM.

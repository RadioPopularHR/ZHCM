*----------------------------------------------------------------------*
***INCLUDE ZHCM_WFM_WAGES_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_PERNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pernr .
  DATA: lv_last_day    TYPE datum.

  DATA(lv_first_day) = CONV begda( sy-datum(4) && sy-datum+4(2) && '01' ).

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_first_day
    IMPORTING
      last_day_of_month = lv_last_day
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

  SELECT pernr
    FROM pa0041
    INTO TABLE @gt_pernr_old
    WHERE pernr NOT IN (
                          SELECT pernr
                            FROM pa0015
                            WHERE subty = '9020'
                              AND begda >= @lv_first_day
                              AND endda <= @lv_last_day
                        )
      AND begda <= @sy-datum
      AND endda >= @sy-datum
      AND dat01 < @lv_first_day.

  SELECT pernr
    FROM pa0041
    INTO TABLE gt_pernr_new
    WHERE begda <= sy-datum
      AND endda >= sy-datum
      AND dat01 >= lv_first_day.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REGISTER_DAYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM register_days .
  TYPES: BEGIN OF ty_s_sched,
           pernr TYPE pernr_d,
           anzhl TYPE anzhl,
         END OF ty_s_sched.

  DATA: lt_total_sched     TYPE TABLE OF ztwfm_hcm_sched,
        lt_total_sched_new TYPE TABLE OF ztwfm_hcm_sched,
        lt_sched_old       TYPE TABLE OF ty_s_sched,

        ls_return          TYPE bapireturn1,
        ls_key             TYPE bapipakey,

        lv_last_day        TYPE datum,
        lv_endda           TYPE endda,
        lv_total           TYPE p LENGTH 3 DECIMALS 1,
        lv_data            TYPE REF TO data.

  SELECT *
    FROM ztwfm_hcm_subtyp
    INTO TABLE @DATA(lt_subtype)
    WHERE subtype = '9053'
       OR subtype = '9020'.

  CHECK sy-subrc = 0.

  DATA(lv_first_day) = CONV begda( sy-datum - 30 ).
  lv_first_day = CONV begda( lv_first_day(4) && lv_first_day+4(2) && '01' ).

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_first_day
    IMPORTING
      last_day_of_month = lv_last_day
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

  DATA(lv_begda) = CONV begda( sy-datum(4) && sy-datum+4(2) && '01' ).

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_begda
    IMPORTING
      last_day_of_month = lv_endda
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

  IF lines( gt_pernr_new ) > 0.
    SELECT pernr,
           begda,
           anzhl
      FROM ztwfm_hcm_sched
      INTO TABLE @DATA(lt_sched_new)
      FOR ALL ENTRIES IN @gt_pernr_new
      WHERE pernr = @gt_pernr_new-table_line
        AND begda >= @lv_begda
        AND endda <= @lv_endda.

    SORT lt_sched_new BY pernr ASCENDING begda ASCENDING.
  ENDIF.

  LOOP AT lt_sched_new INTO DATA(ls_sched_new).
    AT NEW begda.
      IF ls_sched_new-anzhl >= 5.
        lv_total = lv_total + 1.
      ELSE.
        lv_total = lv_total + ( ls_sched_new-anzhl / 8 ).
      ENDIF.
    ENDAT.

    AT END OF pernr.
      APPEND INITIAL LINE TO lt_total_sched_new ASSIGNING FIELD-SYMBOL(<fs_total_sched_new>).
      <fs_total_sched_new>-pernr = ls_sched_new-pernr.
      <fs_total_sched_new>-anzhl = lv_total.

      CLEAR lv_total.
    ENDAT.
  ENDLOOP.

  IF lines( gt_pernr_old ) > 0.
    SELECT pernr,
           begda,
           anzhl
      FROM ztwfm_hcm_sched
      INTO TABLE @DATA(lt_sched)
      FOR ALL ENTRIES IN @gt_pernr_old
      WHERE pernr = @gt_pernr_old-table_line
        AND begda >= @lv_first_day
        AND endda <= @lv_last_day.

    READ TABLE lt_subtype INTO DATA(ls_subtype) WITH KEY subtype = '9020'.
    IF sy-subrc = 0.
      DATA(lv_table) = 'PA' && ls_subtype-infotype.

      SELECT pernr,
             anzhl
        FROM (lv_table)
        INTO TABLE @lt_sched_old
        FOR ALL ENTRIES IN @gt_pernr_old
        WHERE pernr = @gt_pernr_old-table_line
          AND subty = '9020'
          AND begda >= @lv_first_day
          AND endda <= @lv_last_day.
    ENDIF.

    SORT lt_sched BY pernr ASCENDING begda ASCENDING.
  ENDIF.

  LOOP AT lt_sched INTO DATA(ls_sched).
    AT NEW begda.
      IF ls_sched-anzhl >= 5.
        lv_total = lv_total + 1.
      ELSE.
        lv_total = lv_total + ( ls_sched-anzhl / 8 ).
      ENDIF.
    ENDAT.

    AT END OF pernr.
      APPEND INITIAL LINE TO lt_total_sched ASSIGNING FIELD-SYMBOL(<fs_total_sched>).
      <fs_total_sched>-pernr = ls_sched-pernr.
      <fs_total_sched>-anzhl = lv_total.

      CLEAR lv_total.
    ENDAT.
  ENDLOOP.

  LOOP AT gt_pernr_old INTO DATA(ls_pernr).
    READ TABLE lt_subtype INTO ls_subtype WITH KEY subtype = '9020'.
    IF sy-subrc = 0.
      DATA(lv_struct) = 'P' && ls_subtype-infotype.
      CREATE DATA lv_data TYPE (lv_struct).
      ASSIGN lv_data->* TO FIELD-SYMBOL(<fs_record>).

      ASSIGN COMPONENT 'PERNR' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_pernr>).
      ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_subty>).
      ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_begda>).
      ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_endda>).
      ASSIGN COMPONENT 'LGART' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_lgart>).
      ASSIGN COMPONENT 'ANZHL' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_anzhl>).
      ASSIGN COMPONENT 'ZEINH' OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_zeinh>).

      <fs_pernr> = ls_pernr.
      <fs_subty> = <fs_lgart> = '9020'.
      <fs_begda> = lv_begda.
      <fs_endda> = lv_endda.
      <fs_anzhl> = 22.
      <fs_zeinh> = '010'.

      zcl_hcm_ws_wfm=>infotype_operation(
        EXPORTING
          iv_infotype        = ls_subtype-infotype                 " Infotipo
          iv_employee_number = <fs_pernr>                 " Nº pessoal
          iv_subty           = <fs_subty>                 " Subinfotipo
          iv_begda           = <fs_begda>                 " Início da validade
          iv_endda           = <fs_endda>                 " Fim da validade
          iv_operation       = 'INS'                 " Operação em infotipos
          iv_commit          = abap_false
          is_record          = <fs_record>
        IMPORTING
          es_return          = ls_return                 " Parâmetro de retorno
          es_key             = ls_key                 " Chave para dados mestre HR
      ).
    ENDIF.

    READ TABLE lt_subtype INTO ls_subtype WITH KEY subtype = '9053'.
    IF sy-subrc = 0.
      lv_struct = 'P' && ls_subtype-infotype.
      CREATE DATA lv_data TYPE (lv_struct).
      ASSIGN lv_data->* TO <fs_record>.

      ASSIGN COMPONENT 'PERNR' OF STRUCTURE <fs_record> TO <fs_pernr>.
      ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <fs_record> TO <fs_subty>.
      ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <fs_record> TO <fs_begda>.
      ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <fs_record> TO <fs_endda>.
      ASSIGN COMPONENT 'LGART' OF STRUCTURE <fs_record> TO <fs_lgart>.
      ASSIGN COMPONENT 'ANZHL' OF STRUCTURE <fs_record> TO <fs_anzhl>.
      ASSIGN COMPONENT 'ZEINH' OF STRUCTURE <fs_record> TO <fs_zeinh>.

      READ TABLE lt_sched_old INTO DATA(ls_sched_old) WITH KEY pernr = ls_pernr.
      CHECK sy-subrc = 0.

      READ TABLE lt_total_sched INTO DATA(ls_total_sched) WITH KEY pernr = ls_pernr.
      CHECK sy-subrc = 0.

      <fs_pernr> = ls_pernr.
      <fs_subty> = <fs_lgart> = '9053'.
      <fs_begda> = lv_begda.
      <fs_endda> = lv_endda.
      <fs_anzhl> = ls_total_sched-anzhl - ls_sched_old-anzhl.
      <fs_zeinh> = '010'.

      zcl_hcm_ws_wfm=>infotype_operation(
        EXPORTING
          iv_infotype        = ls_subtype-infotype                 " Infotipo
          iv_employee_number = <fs_pernr>                 " Nº pessoal
          iv_subty           = <fs_subty>                 " Subinfotipo
          iv_begda           = <fs_begda>                 " Início da validade
          iv_endda           = <fs_endda>                 " Fim da validade
          iv_operation       = 'INS'                 " Operação em infotipos
          iv_commit          = abap_false
          is_record          = <fs_record>
        IMPORTING
          es_return          = ls_return                 " Parâmetro de retorno
          es_key             = ls_key                 " Chave para dados mestre HR
      ).
    ENDIF.
  ENDLOOP.

  LOOP AT gt_pernr_new INTO ls_pernr.
    READ TABLE lt_subtype INTO ls_subtype WITH KEY subtype = '9020'.
    IF sy-subrc = 0.
      lv_struct = 'P' && ls_subtype-infotype.
      CREATE DATA lv_data TYPE (lv_struct).
      ASSIGN lv_data->* TO <fs_record>.

      ASSIGN COMPONENT 'PERNR' OF STRUCTURE <fs_record> TO <fs_pernr>.
      ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <fs_record> TO <fs_subty>.
      ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <fs_record> TO <fs_begda>.
      ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <fs_record> TO <fs_endda>.
      ASSIGN COMPONENT 'LGART' OF STRUCTURE <fs_record> TO <fs_lgart>.
      ASSIGN COMPONENT 'ANZHL' OF STRUCTURE <fs_record> TO <fs_anzhl>.
      ASSIGN COMPONENT 'ZEINH' OF STRUCTURE <fs_record> TO <fs_zeinh>.

      READ TABLE lt_total_sched_new INTO DATA(ls_total_sched_new) WITH KEY pernr = ls_pernr.
      CHECK sy-subrc = 0.

      <fs_pernr> = ls_pernr.
      <fs_subty> = <fs_lgart> = '9020'.
      <fs_begda> = lv_begda.
      <fs_endda> = lv_endda.
      <fs_anzhl> = ls_total_sched_new-anzhl.
      <fs_zeinh> = '010'.

      zcl_hcm_ws_wfm=>infotype_operation(
        EXPORTING
          iv_infotype        = ls_subtype-infotype                 " Infotipo
          iv_employee_number = <fs_pernr>                 " Nº pessoal
          iv_subty           = <fs_subty>                 " Subinfotipo
          iv_begda           = <fs_begda>                 " Início da validade
          iv_endda           = <fs_endda>                 " Fim da validade
          iv_operation       = 'INS'                 " Operação em infotipos
          iv_commit          = abap_false
          is_record          = <fs_record>
        IMPORTING
          es_return          = ls_return                 " Parâmetro de retorno
          es_key             = ls_key                 " Chave para dados mestre HR
      ).
    ENDIF.
  ENDLOOP.
ENDFORM.

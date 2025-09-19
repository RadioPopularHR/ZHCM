*&---------------------------------------------------------------------*
*&  Include           ZHR_WFM_INTEGRACAO_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data .

  IF p_employ IS NOT INITIAL.
    PERFORM post_employee.
  ENDIF.

  IF p_cont_e IS NOT INITIAL.
    PERFORM post_contingent_e.
  ENDIF.

  IF p_contin IS NOT INITIAL.
    PERFORM post_contingent.
  ENDIF.

  IF p_abse_r IS NOT INITIAL.
    PERFORM post_absence_r.
  ENDIF.

  IF p_abse_o IS NOT INITIAL.

    PERFORM valida_area_processamento USING gv_state gv_state_text.
    IF gv_state EQ '01'.
      MESSAGE gv_state_text TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    PERFORM get_absences_o.
  ENDIF.

*  IF p_sche_o IS NOT INITIAL.
*    PERFORM get_schedule_o.
*  ENDIF.

  IF p_wage_o IS NOT INITIAL.

    PERFORM valida_area_processamento USING gv_state gv_state_text.
    IF gv_state EQ '01'.
      MESSAGE gv_state_text TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    PERFORM get_wages_o.
  ENDIF.

*  IF p_data_o IS NOT INITIAL.
*
*    PERFORM valida_area_processamento USING gv_state gv_state_text.
*    IF gv_state EQ '01'.
*      MESSAGE gv_state_text TYPE 'S' DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
*
*    PERFORM get_data_o.
*  ENDIF.
ENDFORM.

FORM valida_area_processamento USING gv_state
                                     gv_state_text.

* Validar se existem mais além da 'RP'
  CALL FUNCTION 'PA03_PCR_READ'
    EXPORTING
      f_abkrs               = 'RP'
      error_no_accounting   = 'X'
    IMPORTING
      f_state               = gv_state
      f_state_text          = gv_state_text
    EXCEPTIONS
      abkrs_no_accounting   = 1
      pcr_does_not_exist    = 2
      abkrs_does_not_exist  = 3
      period_does_not_exist = 4
      no_pcr_authority      = 5
      OTHERS                = 6.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  POST_EMPLOYEE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_employee .
  DATA: ls_employee_i TYPE zhcm_s_i_employee,
        ls_output     TYPE zhcm_s_wfm_output,

        lv_times      TYPE i,
        lv_count      TYPE i,

        lt_pa0008     TYPE TABLE OF pa0008.

* >>> INI Inetum SAM EMP/SS HR 7000189109 02.01.2024
  DATA: lv_hire_date TYPE begda,
        lt_p0000     TYPE TABLE OF p0000,
        lt_p0001     TYPE TABLE OF p0001.
* >>> INI Inetum SAM EMP/SS HR 7000189109 02.01.2024


  "Inbound
  "Employee integration selection
  SELECT p1~pernr,
         p2~cname,
         p0~begda,
         p0~endda,
         p0~massn,
         p1~persg,
         p331~finum,
         p1~bukrs,
         p1~btrtl,
         p1~stell,
         p7~wostd,
         p7~begda AS begda_p7,
         p7~endda AS endda_p7,
         p50~begda AS begda_p50,
         p50~endda AS endda_p50,
         p50~zausw
    FROM pa0000 AS p0
    JOIN pa0001 AS p1 ON p1~pernr = p0~pernr
                     AND p1~begda <= @p_datum
                     AND p1~endda >= @p_datum
    JOIN pa0002 AS p2 ON p2~pernr = p0~pernr
                     AND p2~begda <= @p_datum
                     AND p2~endda >= @p_datum
    JOIN pa0331 AS p331 ON p331~pernr = p0~pernr
                       AND p331~begda <= @p_datum
                       AND p331~endda >= @p_datum
    LEFT OUTER JOIN pa0007 AS p7 ON p7~pernr = p0~pernr
                                AND p7~begda <= @p_datum
                                AND p7~endda >= @p_datum
    LEFT OUTER JOIN pa0050 AS p50 ON p50~pernr = p0~pernr
                                 AND p50~begda <= @p_datum
                                 AND p50~endda >= @p_datum
    INTO TABLE @DATA(lt_employee)
    WHERE p0~pernr IN @s_pernr
      AND p0~begda = @p_datum
*      AND p0~endda >= @p_datum
*APP - 22.05.2023
      AND ( p0~stat2 = @gc_active
      OR    p0~stat2 = @gc_inactive )
*      AND ( p0~massn = @gc_stat_z1
*       OR   p0~massn = @gc_stat_z4 )
      AND ( p0~massn = @gc_stat_z1
       OR   p0~massn = @gc_stat_z3
       OR   p0~massn = @gc_stat_z4
       OR   p0~massn = @gc_stat_zz )
*APP - 22.05.2023
    ORDER BY p0~pernr.

  lv_times = lines( lt_employee ) / gc_max_entries.
  lv_count = 0.

  IF lv_times = 0.
    lv_times = 1.
  ENDIF.

  DO lv_times TIMES.
    LOOP AT lt_employee INTO DATA(ls_employee).
      IF lv_count = gc_max_entries.
        lv_count = 0.
        EXIT.
      ENDIF.

      CHECK ls_employee-stell IS NOT INITIAL.

      APPEND INITIAL LINE TO ls_employee_i-employees ASSIGNING FIELD-SYMBOL(<fs_employees>).
      <fs_employees>-company_id = ls_employee-bukrs.
      <fs_employees>-section_id = ls_employee-btrtl.
      <fs_employees>-role_id = ls_employee-stell.

      <fs_employees>-employee_info-employee_id = ls_employee-pernr.
      <fs_employees>-employee_info-name = ls_employee-cname.
      <fs_employees>-employee_info-admission_date = ls_employee-begda(4) && '-' && ls_employee-begda+4(2) && '-' && ls_employee-begda+6(2).

* >>> INI Inetum SAM EMP/SS HR 7000189109 02.01.2024
      IF ls_employee-massn EQ gc_stat_z4.
* Quando é uma saída a data de admissºao tem de ser lida de outra medida que não a Z4
        REFRESH: lt_p0000, lt_p0001.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_p0000
                 FROM pa0000
                 WHERE pernr EQ ls_employee-pernr
                   AND sprps EQ space.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_p0001
                 FROM pa0001
                 WHERE pernr EQ ls_employee-pernr
                   AND sprps EQ space.

        CALL FUNCTION 'HR_PT_HIRE_FIRE'
          IMPORTING
*           FIRE_DATE            =
            hire_date            = lv_hire_date
          TABLES
            pp0000               = lt_p0000
            pp0001               = lt_p0001
          EXCEPTIONS
            entry_date_not_found = 1
            feature_error        = 2
            OTHERS               = 3.
        IF sy-subrc <> 0.
          CLEAR: <fs_employees>-employee_info-admission_date.
        ELSE.
          <fs_employees>-employee_info-admission_date = lv_hire_date(4) && '-' && lv_hire_date+4(2) && '-' && lv_hire_date+6(2).
        ENDIF.
      ENDIF.
* <<< END Inetum SAM EMP/SS HR 7000189109 02.01.2024

* APP - 29.03.2023
      <fs_employees>-employee_info-has_time_management = abap_true.
* APP - 29.03.2023

* APP - 07.11.2023
      IF ls_employee-massn = gc_stat_z4.
* >>> INI Inetum SAM EMP/SS HR 7000189109 01.03.2024
* Comentado
*        <fs_employees>-employee_info-leave_date = ls_employee-begda(4) && '-' && ls_employee-begda+4(2) && '-' && ls_employee-begda+6(2).

        DATA: lv_data TYPE begda.
        lv_data = ls_employee-begda - 1.
        <fs_employees>-employee_info-leave_date = lv_data(4) && '-' && lv_data+4(2) && '-' && lv_data+6(2).
* <<< END Inetum SAM EMP/SS HR 7000189109 01.03.2024
      ENDIF.
* APP - 07.11.2023

*      IF ls_employee-endda(4) <> '9999'.
*        <fs_employees>-employee_info-leave_date = ls_employee-endda(4) && '-' && ls_employee-endda+4(2) && '-' && ls_employee-endda+6(2).
*      ENDIF.

      <fs_employees>-employee_info-employee_status = ls_employee-persg.
      <fs_employees>-employee_info-nif = ls_employee-finum.

      IF ls_employee-wostd > 0.
        <fs_employees>-weekly_workload-weekly_workload_value = ls_employee-wostd.
        REPLACE '.' IN <fs_employees>-weekly_workload-weekly_workload_value WITH ','.
      ENDIF.

      <fs_employees>-weekly_workload-weekly_workload_start_date = ls_employee-begda_p7(4) && '-' && ls_employee-begda_p7+4(2) && '-' && ls_employee-begda_p7+6(2).

      IF <fs_employees>-weekly_workload-weekly_workload_end_date IS NOT INITIAL.
        IF <fs_employees>-weekly_workload-weekly_workload_end_date(4) <> '9999'.
          <fs_employees>-weekly_workload-weekly_workload_end_date = ls_employee-endda_p7(4) && '-' && ls_employee-endda_p7+4(2) && '-' && ls_employee-endda_p7+6(2).
        ELSE.
          <fs_employees>-weekly_workload-weekly_workload_end_date = '2100' && '-' && ls_employee-endda_p7+4(2) && '-' && ls_employee-endda_p7+6(2).
        ENDIF.
      ENDIF.

* APP - 19.06.2023
      "IHT
      SELECT * FROM pa0008
        INTO TABLE lt_pa0008
        WHERE pernr = ls_employee-pernr
          AND begda LE p_datum
          AND endda GE p_datum
          AND ( lga01 = '1010'        "Transformar em dinâmico LGAxx
          OR    lga02 = '1010'          OR    lga03 = '1010'          OR    lga04 = '1010'          OR    lga05 = '1010'          OR    lga06 = '1010'          OR    lga07 = '1010'
          OR    lga08 = '1010'          OR    lga09 = '1010'          OR    lga10 = '1010'          "OR    lga11 = '1010'          OR    lga12 = '1010'          OR    lga13 = '1010'
*          OR    lga14 = '1010'          OR    lga15 = '1010'          OR    lga16 = '1010'          OR    lga17 = '1010'          OR    lga18 = '1010'          OR    lga19 = '1010'
*          OR    lga20 = '1010'          OR    lga21 = '1010'          OR    lga22 = '1010'          OR    lga23 = '1010'          OR    lga24 = '1010'          OR    lga25 = '1010'
*          OR    lga26 = '1010'          OR    lga27 = '1010'          OR    lga28 = '1010'          OR    lga29 = '1010'          OR    lga30 = '1010'          OR    lga31 = '1010'
*          OR    lga32 = '1010'          OR    lga33 = '1010'          OR    lga34 = '1010'          OR    lga35 = '1010'          OR    lga36 = '1010'          OR    lga37 = '1010'
*          OR    lga38 = '1010'          OR    lga39 = '1010'          OR    lga40 = '1010'
          ).

      IF sy-subrc EQ 0.
        READ TABLE lt_pa0008 INTO DATA(ls_pa0008) INDEX 1.

        IF sy-subrc EQ 0.
          "HasIHT -> Validar na pa0008 se existe algum LGA'XX com o valor 1010
          "-> Para a data de dia/admissão ?
          <fs_employees>-iht-has_iht = 'X'.
          <fs_employees>-time_management-night_work-night_work_allowance = 'X'.

          IF ls_pa0008-endda IS NOT INITIAL.
            "IHTEndDate   - Data de Fim do IHT
            IF ls_pa0008-endda(4) <> '9999'.
              <fs_employees>-iht-iht_end_date   = ls_pa0008-endda(4) && '-' && ls_pa0008-endda+4(2) && '-' && ls_pa0008-endda+6(2).
            ELSE.
              <fs_employees>-iht-iht_end_date   = '2049' && '-' && ls_pa0008-endda+4(2) && '-' && ls_pa0008-endda+6(2).
            ENDIF.

          ENDIF.

          IF ls_pa0008-begda IS NOT INITIAL.
            "IHTStartDate - Data de Inicio do IHT
            IF ls_pa0008-begda(4) <> '9999'.
              <fs_employees>-iht-iht_start_date = ls_pa0008-begda(4) && '-' && ls_pa0008-begda+4(2) && '-' && ls_pa0008-begda+6(2).
            ELSE.
              <fs_employees>-iht-iht_start_date = '2049' && '-' && ls_pa0008-begda+4(2) && '-' && ls_pa0008-begda+6(2).
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.

* APP - 19.06.2023

      IF ls_employee-begda_p50 IS NOT INITIAL.
        <fs_employees>-time_management-info_start_date = ls_employee-begda_p50(4) && '-' && ls_employee-begda_p50+4(2) && '-' && ls_employee-begda_p50+6(2).
      ENDIF.

      IF ls_employee-endda_p50 IS NOT INITIAL.
        IF ls_employee-endda_p50(4) <> '9999'.
          <fs_employees>-time_management-info_end_date = ls_employee-endda_p50(4) && '-' && ls_employee-endda_p50+4(2) && '-' && ls_employee-endda_p50+6(2).
        ELSE.
          <fs_employees>-time_management-info_end_date = '2100' && '-' && ls_employee-endda_p50+4(2) && '-' && ls_employee-endda_p50+6(2).
        ENDIF.
      ENDIF.

*<- APP - 22.05.2023
      IF ls_employee-zausw IS NOT INITIAL.
*        <fs_employees>-time_management-card_number = ls_employee-zausw.
        <fs_employees>-time_management-card_number = ls_employee-zausw.

      ELSE.
        <fs_employees>-time_management-card_number = ls_employee-pernr.
      ENDIF.
*>- APP - 22.05.2023

      ls_employee_i-flow_control-extraction_date = sy-datum(4) && '-' && sy-datum+4(2) && '-' && sy-datum+6(2)..
      ls_employee_i-flow_control-flow_type = 'D'.

      DELETE lt_employee INDEX 1.

      lv_count = lv_count + 1.
    ENDLOOP.

    IF p_test IS NOT INITIAL.
      ls_employee_i-test = abap_true.
    ELSE.
      ls_employee_i-test = abap_false.
    ENDIF.

    CALL FUNCTION 'ZFM_HCM_WFM_POST_EMPLOYEE'
      EXPORTING
        input  = ls_employee_i
      IMPORTING
        output = ls_output.

    CLEAR: ls_employee_i, ls_output.

  ENDDO.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POST_CONTINGENT_E
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_contingent_e .
  DATA: ls_contingent_e_i TYPE zhcm_s_i_contingent_e,
        ls_output         TYPE zhcm_s_wfm_output,

        lv_times          TYPE i,
        lv_count          TYPE i.

  "pa2007 acrescentar

  "Employee contingents integration
  SELECT p2007~pernr,
         p2007~begda,
         p2007~endda,
         p2007~desta,
         p2007~deend,
         p2007~ktart,
         'P' AS type,
         p2007~seqnr,
         p2007~anzhl,
         p2007~kverb
    FROM pa0000 AS p0
    JOIN pa2007 AS p2007 ON p2007~pernr = p0~pernr
                        AND p2007~begda <= @p_datum
                        AND p2007~endda >= @p_datum
    INTO TABLE @DATA(lt_contingent_e)
    WHERE p0~pernr IN @s_pernr
      AND p0~begda <= @p_datum
      AND p0~endda >= @p_datum
* >>> INI Inetum SAM EMP/SS HR 7000189109 04.01.2024
* Comentado
*      AND ( p0~massn = @gc_stat_z1
*       OR   p0~massn = @gc_stat_z4 )

* Tem de se seleccionar os contingentes criados na data de selecção
* que sejam demitidos, mas que sejam também independentes da medida
* que tenham nessa data, têm é de ser activos
      AND ( ( p0~massn EQ @gc_stat_z4 )
         OR ( p0~massn NE @gc_stat_z4 AND p0~stat2 EQ '3' ) )
* <<< END Inetum SAM EMP/SS HR 7000189109 04.01.2024
    ORDER BY p0~pernr.

* >>> INI Inetum SAM EMP/SS HR 7000191756 07.03.2024
* Comentado, porque vai ler a tabela ZTWFM_HCM_CONTG alimentada somente quando
* há alterações no IT2006, para só serem enviadas as alterações ao direito em
* cada contingente (badi ZHR_HRPAD00INFTY método IN_UPDATE)
*  SELECT p2006~pernr,
*         p2006~begda,
*         p2006~endda,
*         p2006~desta,
*         p2006~deend,
*         p2006~ktart,
*         'A' AS type,
*         p2006~seqnr,
*         p2006~anzhl,
*         p2006~kverb
*    FROM pa0000 AS p0
*    JOIN pa2006 AS p2006 ON p2006~pernr = p0~pernr
*                        AND p2006~begda <= @p_datum
*                        AND p2006~endda >= @p_datum
*    APPENDING TABLE @lt_contingent_e
*    WHERE p0~pernr IN @s_pernr
*      AND p0~begda <= @p_datum
*      AND p0~endda >= @p_datum
** >>> INI Inetum SAM EMP/SS HR 7000189109 04.01.2024
** Comentado
**      AND ( p0~massn = @gc_stat_z1
**       OR   p0~massn = @gc_stat_z4 )
*
** Tem de se seleccionar os contingentes criados na data de selecção
** que sejam demitidos, mas que sejam também independentes da medida
** que tenham nessa data, têm é de ser activos
*      AND ( ( p0~massn EQ @gc_stat_z4 )
*         OR ( p0~massn NE @gc_stat_z4 AND p0~stat2 EQ '3' ) )
** <<< END Inetum SAM EMP/SS HR 7000189109 04.01.2024
*    ORDER BY p0~pernr.

  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE lt_contingent_e
           FROM ztwfm_hcm_contg
           WHERE pernr   IN s_pernr
             AND aedtm   EQ p_datum
             AND enviado EQ '00000000'.

  DATA: lt_contg TYPE TABLE OF ztwfm_hcm_contg.
  REFRESH: lt_contg.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_contg
           FROM ztwfm_hcm_contg
           WHERE pernr   IN s_pernr
             AND aedtm   EQ p_datum
             AND enviado EQ '00000000'.
* <<< END Inetum SAM EMP/SS HR 7000191756 07.03.2024

* >>> INI Inetum SAM EMP/SS HR 7000189109 04.01.2024
  DELETE lt_contingent_e WHERE ktart EQ '49'
                            OR ktart EQ '90'
                            OR ktart EQ '91'
*                            OR ktart EQ '96'   " DEL Inetum SAM EMP/SS HR 7000191756 07.03.2024
                            OR ktart EQ '97'
                            OR ktart EQ '99'.
* <<< END Inetum SAM EMP/SS HR 7000189109 04.01.2024

  lv_times = lines( lt_contingent_e ) / gc_max_entries.
  lv_count = 0.

  IF lv_times = 0.
    lv_times = 1.
  ENDIF.

  DO lv_times TIMES.
    LOOP AT lt_contingent_e INTO DATA(ls_contingent_e).
      IF lv_count = gc_max_entries.
        lv_count = 0.
        EXIT.
      ENDIF.

      APPEND INITIAL LINE TO ls_contingent_e_i-contingent_employees ASSIGNING FIELD-SYMBOL(<fs_contingent_employees>).
      <fs_contingent_employees>-contingent_id = ls_contingent_e-ktart.
      <fs_contingent_employees>-employee_id = ls_contingent_e-pernr.
      <fs_contingent_employees>-contingent_start_date = ls_contingent_e-begda(4) && '-' && ls_contingent_e-begda+4(2) && '-' && ls_contingent_e-begda+6(2).

      IF ls_contingent_e-endda(4) <> '9999'.
        <fs_contingent_employees>-contingent_end_date = ls_contingent_e-endda(4) && '-' && ls_contingent_e-endda+4(2) && '-' && ls_contingent_e-endda+6(2).
      ELSE.
        <fs_contingent_employees>-contingent_end_date = '2100' && '-' && ls_contingent_e-endda+4(2) && '-' && ls_contingent_e-endda+6(2).
      ENDIF.

      <fs_contingent_employees>-deduction_start_date = ls_contingent_e-desta(4) && '-' && ls_contingent_e-desta+4(2) && '-' && ls_contingent_e-desta+6(2).

      IF ls_contingent_e-deend(4) <> '9999'.
        <fs_contingent_employees>-deduction_end_date = ls_contingent_e-deend(4) && '-' && ls_contingent_e-deend+4(2) && '-' && ls_contingent_e-deend+6(2).
      ELSE.
        <fs_contingent_employees>-deduction_end_date = '2100' && '-' && ls_contingent_e-deend+4(2) && '-' && ls_contingent_e-deend+6(2).
      ENDIF.

      <fs_contingent_employees>-contingent_type = ls_contingent_e-type.
      <fs_contingent_employees>-sequence = ls_contingent_e-seqnr.
      <fs_contingent_employees>-quantity_value = ls_contingent_e-anzhl.
      <fs_contingent_employees>-deduction_value = ls_contingent_e-kverb.

      ls_contingent_e_i-flow_control-extraction_date = sy-datum(4) && '-' && sy-datum+4(2) && '-' && sy-datum+6(2).
      ls_contingent_e_i-flow_control-flow_type = 'D'.

      DELETE lt_contingent_e INDEX 1.

      lv_count = lv_count + 1.
    ENDLOOP.

    IF p_test IS NOT INITIAL.
      ls_contingent_e_i-test = abap_true.
    ELSE.
      ls_contingent_e_i-test = abap_false.
    ENDIF.

    CALL FUNCTION 'ZFM_HCM_WFM_POST_CONTINGENT_E'
      EXPORTING
        input  = ls_contingent_e_i
      IMPORTING
        output = ls_output.

* >>> INI Inetum SAM EMP/SS HR 7000191756 07.03.2024
    DATA: ls_contg TYPE ztwfm_hcm_contg.

    IF ls_output-status_code EQ 'S'.
      LOOP AT lt_contg INTO ls_contg.
        ls_contg-enviado = sy-datum.
        MODIFY ztwfm_hcm_contg FROM ls_contg.
      ENDLOOP.
    ENDIF.
* <<< END Inetum SAM EMP/SS HR 7000191756 07.03.2024

    CLEAR: ls_contingent_e_i, ls_output.
  ENDDO.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POST_CONTINGENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_contingent .
  DATA: lt_contingent   TYPE TABLE OF ty_s_contingent,

        ls_contingent_i TYPE zhcm_s_i_contingent,
        ls_output       TYPE zhcm_s_wfm_output,

        lv_times        TYPE i,
        lv_count        TYPE i.

  "Contingent integration
  "Absences
  SELECT a~ktart,
         'A' AS type,
         b~ktext,
         a~begda,
         a~endda,
         a~zeinh,
         a~qtneg
    FROM t556a AS a
    JOIN t556b AS b ON a~mopgk = b~mopgk
                   AND a~mozko = b~mozko
                   AND a~ktart = b~ktart
    INTO CORRESPONDING FIELDS OF TABLE @lt_contingent
    WHERE a~mopgk = '2'
      AND a~mozko = '19'
      AND a~endda >= @p_datum
      AND a~begda <= @p_datum
      AND b~sprsl = @sy-langu.

  "Presences
  SELECT p~ktart,
         'P' AS type,
         q~ktext,
         p~begda,
         p~endda,
         p~zeinh,
         p~qtneg
    FROM t556p AS p
    JOIN t556q AS q ON p~mopgk = q~mopgk
                   AND p~mozko = q~mozko
                   AND p~ktart = q~ktart
    APPENDING TABLE @lt_contingent
    WHERE p~mopgk = '2'
      AND p~mozko = '19'
      AND p~endda >= @p_datum
      AND p~begda <= @p_datum
      AND q~sprsl = @sy-langu.

* >>> INI Inetum SAM EMP/SS HR 7000189109 04.01.2024
  DELETE lt_contingent WHERE ktart EQ '49'
                          OR ktart EQ '90'
                          OR ktart EQ '91'
                          OR ktart EQ '96'
                          OR ktart EQ '97'
                          OR ktart EQ '99'.
* <<< END Inetum SAM EMP/SS HR 7000189109 04.01.2024

  lv_times = lines( lt_contingent ) / gc_max_entries.
  lv_count = 0.

  IF lv_times = 0.
    lv_times = 1.
  ENDIF.

  DO lv_times TIMES.
    LOOP AT lt_contingent INTO DATA(ls_contingent).
      IF lv_count = gc_max_entries.
        lv_count = 0.
        EXIT.
      ENDIF.

      APPEND INITIAL LINE TO ls_contingent_i-contingents ASSIGNING FIELD-SYMBOL(<fs_contingent_i>).
      <fs_contingent_i>-contingent_id = ls_contingent-ktart.
      <fs_contingent_i>-contingent_type = ls_contingent-type.
      <fs_contingent_i>-contingent_description = ls_contingent-ktext.
      <fs_contingent_i>-contingent_start_date = ls_contingent-begda(4) && '-' && ls_contingent-begda+4(2) && '-' && ls_contingent-begda+6(2).

      IF ls_contingent-endda(4) <> '9999'.
        <fs_contingent_i>-contingent_end_date = ls_contingent-endda(4) && '-' && ls_contingent-endda+4(2) && '-' && ls_contingent-endda+6(2).
      ELSE.
        <fs_contingent_i>-contingent_end_date = '2100' && '-' && ls_contingent-endda+4(2) && '-' && ls_contingent-endda+6(2).
      ENDIF.

      <fs_contingent_i>-contingent_unit = ls_contingent-zeinh.

      IF ls_contingent-qtneg < 0.
        ls_contingent-qtneg = ls_contingent-qtneg * -1.
      ENDIF.

      <fs_contingent_i>-allow_negative_deduction = ls_contingent-qtneg.
      REPLACE ALL OCCURRENCES OF '.' IN <fs_contingent_i>-allow_negative_deduction WITH ','.

      ls_contingent_i-flow_control-extraction_date = sy-datum(4) && '-' && sy-datum+4(2) && '-' && sy-datum+6(2).
      ls_contingent_i-flow_control-flow_type = 'D'.

      DELETE lt_contingent INDEX 1.

      lv_count = lv_count + 1.
    ENDLOOP.

    IF p_test IS NOT INITIAL.
      ls_contingent_i-test = abap_true.
    ELSE.
      ls_contingent_i-test = abap_false.
    ENDIF.

    CALL FUNCTION 'ZFM_HCM_WFM_POST_CONTINGENT'
      EXPORTING
        input  = ls_contingent_i
      IMPORTING
        output = ls_output.

    CLEAR: ls_contingent_i, ls_output.
  ENDDO.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POST_ABSENCE_R
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_absence_r .
  DATA: ls_absence_r_i TYPE zhcm_s_i_absence_r,
        ls_output      TYPE zhcm_s_wfm_output,

        lv_times       TYPE i,
        lv_count       TYPE i,
        lv_mintg       TYPE string,
        lv_maxtg       TYPE string.

  SELECT DISTINCT s~subty,
           t~atext,
           s~begda,
           s~endda,
           s~art01,
           s~mintg,
           s~maxtg,
           s~munit,
           c~ref01
      FROM t554s AS s
      JOIN t554t AS t ON t~moabw = s~moabw
                     AND t~awart = s~subty
      JOIN t554c AS c ON c~molga = s~moabw
                     AND c~klbew = s~klbew
      INTO TABLE @DATA(lt_absence_r)
      WHERE s~moabw = '19'
        AND s~begda <= @p_datum
        AND s~endda >= @p_datum
        AND t~sprsl = @sy-langu.

  lv_times = lines( lt_absence_r ) / gc_max_entries.
  lv_count = 0.

  IF lv_times = 0.
    lv_times = 1.
  ENDIF.

  DO lv_times TIMES.
    LOOP AT lt_absence_r INTO DATA(ls_absence_r).
      IF lv_count = gc_max_entries.
        lv_count = 0.
        EXIT.
      ENDIF.

      APPEND INITIAL LINE TO ls_absence_r_i-absence_reasons ASSIGNING FIELD-SYMBOL(<fs_absence_r_i>).
      lv_mintg = ls_absence_r-mintg.
      lv_maxtg = ls_absence_r-maxtg.

      <fs_absence_r_i>-absence_reason_id = ls_absence_r-subty.
      <fs_absence_r_i>-absence_reason_desc = ls_absence_r-atext.
      <fs_absence_r_i>-absence_reason_start_date = ls_absence_r-begda(4) && '-' && ls_absence_r-begda+4(2) && '-' && ls_absence_r-begda+6(2).

      IF ls_absence_r-endda(4) <> '9999'.
        <fs_absence_r_i>-absence_reason_end_date = ls_absence_r-endda(4) && '-' && ls_absence_r-endda+4(2) && '-' && ls_absence_r-endda+6(2).
      ELSE.
        <fs_absence_r_i>-absence_reason_end_date = '2100' && '-' && ls_absence_r-endda+4(2) && '-' && ls_absence_r-endda+6(2).
      ENDIF.

      <fs_absence_r_i>-absence_reason_type = ls_absence_r-art01.
      <fs_absence_r_i>-absence_reason_min = lv_mintg.
      <fs_absence_r_i>-absence_reason_max = lv_maxtg.
      <fs_absence_r_i>-absence_reason_unit = ls_absence_r-munit.
      <fs_absence_r_i>-payable = ls_absence_r-ref01.

      ls_absence_r_i-flow_control-extraction_date = sy-datum(4) && '-' && sy-datum+4(2) && '-' && sy-datum+6(2).
      ls_absence_r_i-flow_control-flow_type = 'D'.

      DELETE lt_absence_r INDEX 1.

      lv_count = lv_count + 1.
    ENDLOOP.

    IF p_test IS NOT INITIAL.
      ls_absence_r_i-test = abap_true.
    ELSE.
      ls_absence_r_i-test = abap_false.
    ENDIF.

    CALL FUNCTION 'ZFM_HCM_WFM_POST_ABSENCE_R'
      EXPORTING
        input  = ls_absence_r_i
      IMPORTING
        output = ls_output.

    CLEAR: ls_absence_r_i, ls_output.
  ENDDO.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_ABSENCES_O
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_absences_o .
  DATA: ls_dummy  TYPE zhcm_s_i_dummy,
        ls_output TYPE zhcm_s_o_absences,
        lt_pernr  TYPE TABLE OF pernr,
        lv_pernr  TYPE pernr.

  IF p_test IS NOT INITIAL.
    ls_dummy-test = abap_true.
  ELSE.
    ls_dummy-test = abap_false.
  ENDIF.

*Passar nº de pessoal caso exista -> Alterar 'ZFM_HCM_WFM_GET_ABSENCES_O'
  CALL FUNCTION 'ZFM_HCM_WFM_GET_ABSENCES_O'
    EXPORTING
      input  = ls_dummy
    IMPORTING
      output = ls_output.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_SCHEDULE_O
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_schedule_o .
  DATA: ls_dummy  TYPE zhcm_s_i_dummy,
        ls_output TYPE zhcm_s_o_schedule.

  IF p_test IS NOT INITIAL.
    ls_dummy-test = abap_true.
  ELSE.
    ls_dummy-test = abap_false.
  ENDIF.

  CALL FUNCTION 'ZFM_HCM_WFM_GET_SCHEDULE_O'
    EXPORTING
      input  = ls_dummy
    IMPORTING
      output = ls_output.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_WAGES_O
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_wages_o .
  DATA: ls_dummy  TYPE zhcm_s_i_dummy,
        ls_output TYPE zhcm_s_o_wages.

  IF p_test IS NOT INITIAL.
    ls_dummy-test = abap_true.
  ELSE.
    ls_dummy-test = abap_false.
  ENDIF.

  CALL FUNCTION 'ZFM_HCM_WFM_GET_WAGES_O'
    EXPORTING
      input  = ls_dummy
    IMPORTING
      output = ls_output.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_O
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_o .
  DATA: ls_dummy  TYPE zhcm_s_i_dummy,
        ls_output TYPE zhcm_s_o_data.

  IF p_test IS NOT INITIAL.
    ls_dummy-test = abap_true.
  ELSE.
    ls_dummy-test = abap_false.
  ENDIF.

  CALL FUNCTION 'ZFM_HCM_WFM_GET_DATA_O'
    EXPORTING
      input  = ls_dummy
    IMPORTING
      output = ls_output.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email .
  DATA: lo_data      TYPE REF TO data,
        lo_root      TYPE REF TO cx_root,

        lt_main_text TYPE soli_tab,
        lt_att_head  TYPE soli_tab,
        lt_fieldcat  TYPE lvc_t_fcat,

        lv_subject   TYPE so_obj_des,
        lv_subject2  TYPE so_obj_des,
        lv_xml       TYPE xstring,
        lv_xls_size  TYPE sood-objlen.

  IF gv_state EQ '01'.
    EXIT.
  ENDIF.

  SELECT *
    FROM ztwfm_hcm_integ
    INTO TABLE @DATA(lt_integ)
    WHERE integrated = @abap_false.

  CHECK sy-subrc = 0.

  SELECT low
    FROM tvarvc
    INTO TABLE @DATA(lt_recipients)
    WHERE name = 'WFM-EMAIL'.

  CHECK sy-subrc = 0.

  TRY.
      GET REFERENCE OF lt_integ INTO lo_data.

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'ZTWFM_HCM_INTEG'
        CHANGING
          ct_fieldcat            = lt_fieldcat
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.

      DELETE lt_fieldcat WHERE fieldname = 'INTEGRATED'.

      DATA(lo_result_data) = cl_salv_ex_util=>factory_result_data_table(
         EXPORTING
           r_data                 = lo_data
           t_fieldcatalog         = lt_fieldcat
       ).

      CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
        EXPORTING
          xml_version   = cl_salv_bs_a_xml_base=>get_version( )
          r_result_data = lo_result_data
          xml_type      = if_salv_bs_xml=>c_type_xlsx
        IMPORTING
          xml           = lv_xml.

      lv_xls_size = xstrlen( lv_xml ).

      DATA(lt_xls_content) = cl_document_bcs=>xstring_to_solix( ip_xstring = lv_xml ).
      DATA(lo_send_request) = cl_bcs=>create_persistent( ).

      DATA(lv_datum) = sy-datum+6(2) && '-' && sy-datum+4(2) && '-' && sy-datum(4).

      CONCATENATE 'WFM - Registos não integrados' lv_datum INTO lv_subject SEPARATED BY space.

      DATA(lv_filename) = 'WFM_' && lv_datum && '.xlsx'.
      lv_subject2 = lv_filename.

      APPEND INITIAL LINE TO lt_main_text ASSIGNING FIELD-SYMBOL(<fs_main_text>).
      <fs_main_text>-line = 'Segue em anexo os registos não integrados de WFM.'.

      APPEND INITIAL LINE TO lt_main_text ASSIGNING <fs_main_text>.
      <fs_main_text>-line = ''.

      APPEND INITIAL LINE TO lt_main_text ASSIGNING <fs_main_text>.
      <fs_main_text>-line = 'Melhores cumprimentos.'.

      DATA(lo_document) = cl_document_bcs=>create_document(
            EXPORTING
              i_type          = 'RAW'
              i_subject       = lv_subject
              i_text          = lt_main_text
          ).

      APPEND INITIAL LINE TO lt_att_head ASSIGNING FIELD-SYMBOL(<fs_att_head>).
      <fs_att_head>-line = lv_filename.

      lo_document->add_attachment(
        EXPORTING
          i_attachment_type     = 'BIN'
          i_attachment_subject  = lv_subject2
          i_attachment_size     = lv_xls_size
          i_att_content_hex     = lt_xls_content
          i_attachment_header   = lt_att_head
      ).

      lo_send_request->set_document( i_document = lo_document ).

      LOOP AT lt_recipients INTO DATA(ls_recipients).
        DATA(lv_address) = CONV adr6-smtp_addr( ls_recipients-low ).
        DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address(
                             i_address_string = lv_address ).
        IF sy-subrc EQ 0.
          lo_send_request->add_recipient(
            EXPORTING
              i_recipient     = lo_recipient ).
        ENDIF.
      ENDLOOP.

      lo_send_request->send( ).
      COMMIT WORK AND WAIT.

    CATCH cx_root INTO lo_root .
      DATA(lv_msg) = lo_root->get_text( ).
      MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
      ROLLBACK WORK.
  ENDTRY.
ENDFORM.

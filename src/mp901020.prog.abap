*----------------------------------------------------------------------*
*                                                                      *
*       Output-modules for infotype 9010                               *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       MODULE  P9010 OUTPUT                                           *
*----------------------------------------------------------------------*
*       Default values, Texts                                          *
*----------------------------------------------------------------------*
MODULE p9010 OUTPUT.
  IF psyst-nselc EQ yes.
* read text fields etc.; do this whenever the screen is show for the
*  first time:
*   PERFORM RExxxx.

* >>> INI Inetum SAM EMP/SS HR 7000218401 27.01.2025
    DATA: ls_pa9010 TYPE pa9010.

    IF p9010-email IS INITIAL
   AND psyst-ioper EQ 'INS'.   " só na criação de um novo registo
* Deve gerar um novo email na criação de um registo novo, mas só se
* a mesma pessoa ainda não tive um email associado no IT9010
      CLEAR: ls_pa9010.
      SELECT * INTO ls_pa9010
               FROM pa9010
               WHERE pernr EQ p9010-pernr
                 AND sprps EQ space
                 AND email NE space.
      ENDSELECT.
      IF sy-subrc NE 0.
        PERFORM: f_get_new_mail.
      ELSE.
        p9010-email = ls_pa9010-email.
      ENDIF.

    ENDIF.
* <<< END Inetum SAM EMP/SS HR 7000218401 27.01.2025

    PERFORM: f_descricao_equipa,
             f_descricao_subarea,
             f_descricao_centro_cst,
             f_descricao_funcao.

    IF psyst-iinit = yes AND psyst-ioper = insert.
* generate default values; do this the very first time on insert only:
*     PERFORM GET_DEFAULT.
    ENDIF.
  ENDIF.


ENDMODULE.
*----------------------------------------------------------------------*
*       MODULE  P9010L OUTPUT                                          *
*----------------------------------------------------------------------*
*       read texts for listscreen
*----------------------------------------------------------------------*
MODULE p9010l OUTPUT.


ENDMODULE.
* >>> INI Inetum SAM EMP/SS HR 7000218401 27.01.2025
*&---------------------------------------------------------------------*
*&      Form  F_GET_NEW_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_new_mail.

* Template com 5 ou mais nomes - Template 0
* Fórmulas:
* F1  - N(ini)  + N(end)
* F2  - N(ini)1 + N(end)
* F3  - N(ini)  + N(end)1
* F4  - N(ini)  + N(n)1 + N(end)
* F5  - N(ini)  + N(n)1 + N(n+1)  + N(end)
* F6  - N(ini)  + N(n)  + N(n+1)1 + N(end)
* F7  - N(ini)  + N(n)1 + N(n+1)1 + N(end)
* F8  - N(ini)  + N(n)1 + N(n+1)  + N(n+n)  + N(end)
* F9  - N(ini)  + N(n)  + N(n+1)1 + N(n+n)  + N(end)
* F10 - N(ini)  + N(n)  + N(n+1)  + N(n+n)1 + N(end)

* Template com 4 nomes - Template 1
* Fórmulas:
* F1  - N(ini)  + N(end)
* F2  - N(ini)1 + N(end)
* F3  - N(ini)  + N(end)1
* F4  - N(ini)  + N(n)1 + N(end)
* F5  - N(ini)  + N(n)1 + N(n+1)  + N(end)
* F6  - N(ini)  + N(n)  + N(n+1)1 + N(end)
* F7  - N(ini)  + N(n)1 + N(n+1)1 + N(end)

* Template com 3 nomes - Template 2
* Fórmulas:
* F1  - N(ini)  + N(end)
* F2  - N(ini)1 + N(end)
* F3  - N(ini)  + N(end)1
* F4  - N(ini)  + N(n)1 + N(end)

* Template com 2 nomes - Template 3
* Fórmulas:
* F1 - N(ini)  + N(end)
* F2 - N(ini)1 + N(end)
* F3 - N(ini)  + N(end)1

  DATA: lv_name     TYPE string,
        lv_n_names  TYPE i,
        lv_n_names2 TYPE i,
        lv_time     TYPE i,
        lv_length   TYPE i,
        lv_offset   TYPE i,
        lv_new_name TYPE string,
        lv_db_name  TYPE string,
        lv_x(100).

  CONSTANTS: cv_radio TYPE string VALUE '@radiopopular.pt'.

  CLEAR: lv_name, lv_n_names.
  SELECT cname INTO lv_name
               FROM pa0002
               WHERE pernr EQ p9010-pernr
                 AND sprps EQ space
                 AND begda LE sy-datum
                 AND endda GE sy-datum.
  ENDSELECT.
  IF lv_name IS NOT INITIAL.
    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = lv_name
      IMPORTING
        outtext           = lv_name
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
    ENDIF.

    TRANSLATE lv_name TO UPPER CASE.
    SPLIT lv_name AT space INTO TABLE DATA(itab).

    LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs_itab>).
      IF <fs_itab> = 'SÓ' OR <fs_itab> = 'SÁ'
      OR <fs_itab> = 'SO' OR <fs_itab> = 'SA'.
      ELSE.
        lv_length = strlen( <fs_itab> ).
        IF lv_length LT 3.
          DELETE itab INDEX sy-tabix.
        ELSE.
          lv_x = <fs_itab>.
          DO lv_length TIMES.
            lv_offset = sy-index - 1.
            IF lv_x+lv_offset(1) CO sy-abcde.
            ELSE.
              DELETE itab INDEX sy-tabix.
            ENDIF.
          ENDDO.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE itab LINES lv_n_names2.
    lv_n_names = lv_n_names2.
    IF lv_n_names2 GE 5.
      lv_n_names2 = 9.
    ENDIF.
    ADD 1 TO lv_n_names2.

    CLEAR: lv_time, p9010-email.
    DO lv_n_names2 TIMES.
*      ADD 1 TO lv_time.
      PERFORM get_formula USING lv_n_names2
                          CHANGING lv_time.
      CASE lv_time.
        WHEN 1.
* F1 - N(ini) + N(end)
          CLEAR: lv_new_name.
          READ TABLE itab INTO DATA(ls_itab) INDEX 1.
          lv_new_name = ls_itab.
          READ TABLE itab INTO ls_itab INDEX lv_n_names.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          TRANSLATE lv_new_name TO LOWER CASE.
          CONCATENATE lv_new_name cv_radio INTO lv_new_name.

          SELECT SINGLE email INTO lv_db_name
                              FROM pa9010
                              WHERE email EQ lv_new_name.
          IF sy-subrc EQ 0.
            CONTINUE.
          ELSE.
            p9010-email = lv_new_name.
            EXIT.
          ENDIF.

        WHEN 2.
* F2 - N(ini)1 + N(end)
          CLEAR: lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 1.
          CONCATENATE lv_new_name ls_itab(1) INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX lv_n_names.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          TRANSLATE lv_new_name TO LOWER CASE.
          CONCATENATE lv_new_name cv_radio INTO lv_new_name.

          SELECT SINGLE email INTO lv_db_name
                              FROM pa9010
                              WHERE email EQ lv_new_name.
          IF sy-subrc EQ 0.
            CONTINUE.
          ELSE.
            p9010-email = lv_new_name.
            EXIT.
          ENDIF.

        WHEN 3.
* F3 - N(ini) + N(end)1
          CLEAR: lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 1.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX lv_n_names.
          CONCATENATE lv_new_name ls_itab(1) INTO lv_new_name.
          TRANSLATE lv_new_name TO LOWER CASE.
          CONCATENATE lv_new_name cv_radio INTO lv_new_name.

          SELECT SINGLE email INTO lv_db_name
                              FROM pa9010
                              WHERE email EQ lv_new_name.
          IF sy-subrc EQ 0.
            CONTINUE.
          ELSE.
            p9010-email = lv_new_name.
            EXIT.
          ENDIF.

        WHEN 4.
* F4 - N(ini)  + N(n)1 + N(end)
          CLEAR: lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 1.
          lv_new_name = ls_itab.
          READ TABLE itab INTO ls_itab INDEX 2.
          CONCATENATE lv_new_name ls_itab(1) INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX lv_n_names.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          TRANSLATE lv_new_name TO LOWER CASE.
          CONCATENATE lv_new_name cv_radio INTO lv_new_name.

          SELECT SINGLE email INTO lv_db_name
                              FROM pa9010
                              WHERE email EQ lv_new_name.
          IF sy-subrc EQ 0.
            CONTINUE.
          ELSE.
            p9010-email = lv_new_name.
            EXIT.
          ENDIF.

        WHEN 5.
* F5 - N(ini) + N(n)1 + N(n+1) + N(end)
          CLEAR: lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 1.
          lv_new_name = ls_itab.
          READ TABLE itab INTO ls_itab INDEX 2.
          CONCATENATE lv_new_name ls_itab(1) INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 3.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX lv_n_names.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          TRANSLATE lv_new_name TO LOWER CASE.
          CONCATENATE lv_new_name cv_radio INTO lv_new_name.

          SELECT SINGLE email INTO lv_db_name
                              FROM pa9010
                              WHERE email EQ lv_new_name.
          IF sy-subrc EQ 0.
            CONTINUE.
          ELSE.
            p9010-email = lv_new_name.
            EXIT.
          ENDIF.

        WHEN 6.
* F6 - N(ini) + N(n) + N(n+1)1 + N(end)
          CLEAR: lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 1.
          lv_new_name = ls_itab.
          READ TABLE itab INTO ls_itab INDEX 2.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 3.
          CONCATENATE lv_new_name ls_itab(1) INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX lv_n_names.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          TRANSLATE lv_new_name TO LOWER CASE.
          CONCATENATE lv_new_name cv_radio INTO lv_new_name.

          SELECT SINGLE email INTO lv_db_name
                              FROM pa9010
                              WHERE email EQ lv_new_name.
          IF sy-subrc EQ 0.
            CONTINUE.
          ELSE.
            p9010-email = lv_new_name.
            EXIT.
          ENDIF.

        WHEN 7.
* F7 - N(ini) + N(n)1 + N(n+1)1 + N(end)
          CLEAR: lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 1.
          lv_new_name = ls_itab.
          READ TABLE itab INTO ls_itab INDEX 2.
          CONCATENATE lv_new_name ls_itab(1) INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 3.
          CONCATENATE lv_new_name ls_itab(1) INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX lv_n_names.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          TRANSLATE lv_new_name TO LOWER CASE.
          CONCATENATE lv_new_name cv_radio INTO lv_new_name.

          SELECT SINGLE email INTO lv_db_name
                              FROM pa9010
                              WHERE email EQ lv_new_name.
          IF sy-subrc EQ 0.
            CONTINUE.
          ELSE.
            p9010-email = lv_new_name.
            EXIT.
          ENDIF.

        WHEN 8.
* F8 - N(ini) + N(n)1 + N(n+1) + N(n+n) + N(end)
          CLEAR: lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 1.
          lv_new_name = ls_itab.
          READ TABLE itab INTO ls_itab INDEX 2.
          CONCATENATE lv_new_name ls_itab(1) INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 3.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 4.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX lv_n_names.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          TRANSLATE lv_new_name TO LOWER CASE.
          CONCATENATE lv_new_name cv_radio INTO lv_new_name.

          SELECT SINGLE email INTO lv_db_name
                              FROM pa9010
                              WHERE email EQ lv_new_name.
          IF sy-subrc EQ 0.
            CONTINUE.
          ELSE.
            p9010-email = lv_new_name.
            EXIT.
          ENDIF.

        WHEN 9.
* F9 - N(ini) + N(n) + N(n+1)1 + N(n+n) + N(end)
          CLEAR: lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 1.
          lv_new_name = ls_itab.
          READ TABLE itab INTO ls_itab INDEX 2.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 3.
*          CONCATENATE lv_new_name ls_itab '1' INTO lv_new_name.
          CONCATENATE lv_new_name ls_itab(1) INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 4.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX lv_n_names.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          TRANSLATE lv_new_name TO LOWER CASE.
          CONCATENATE lv_new_name cv_radio INTO lv_new_name.

          SELECT SINGLE email INTO lv_db_name
                              FROM pa9010
                              WHERE email EQ lv_new_name.
          IF sy-subrc EQ 0.
            CONTINUE.
          ELSE.
            p9010-email = lv_new_name.
            EXIT.
          ENDIF.

        WHEN 10.
* F10 - N(ini) + N(n) + N(n+1) + N(n+n)1 + N(end)
          CLEAR: lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 1.
          lv_new_name = ls_itab.
          READ TABLE itab INTO ls_itab INDEX 2.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 3.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX 4.
          CONCATENATE lv_new_name ls_itab(1) INTO lv_new_name.
          READ TABLE itab INTO ls_itab INDEX lv_n_names.
          CONCATENATE lv_new_name ls_itab INTO lv_new_name.
          TRANSLATE lv_new_name TO LOWER CASE.
          CONCATENATE lv_new_name cv_radio INTO lv_new_name.

          SELECT SINGLE email INTO lv_db_name
                              FROM pa9010
                              WHERE email EQ lv_new_name.
          IF sy-subrc EQ 0.
            CLEAR: p9010-email.
            EXIT.
          ELSE.
            p9010-email = lv_new_name.
            EXIT.
          ENDIF.

        WHEN OTHERS.
          EXIT.

      ENDCASE.
    ENDDO.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_FORMULA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_N_NAMES2  text
*      <--P_LV_TIME  text
*----------------------------------------------------------------------*
FORM get_formula  USING    p_n_names TYPE i
                  CHANGING p_time    TYPE i.

* Template com 5 ou mais nomes - Template 0
* F1 / F4 / F5 / F6 / F7 / F8 / F9 / F10 / F2 / F3
*
* Template com 4 nomes - Template 1
* F1 / F4 / F5 / F6 / F7 / F2 / F3
*
* Template com 3 nomes - Template 2
* F1 / F4 / F2 / F3
*
* Template com 2 nomes - Template 3
* F1 / F2 / F3

  IF p_time IS INITIAL.
    p_time = 1.

  ELSE.
    IF p_n_names GE 5.     " Template com 5 ou mais nomes - Template 0
* Template com 5 ou mais nomes - Template 0
* F1 / F4 / F5 / F6 / F7 / F8 / F9 / F10 / F2 / F3
      IF p_time = 1.
        p_time = 4.
      ELSEIF p_time = 4.
        p_time = 5.
      ELSEIF p_time = 5.
        p_time = 6.
      ELSEIF p_time = 6.
        p_time = 7.
      ELSEIF p_time = 7.
        p_time = 8.
      ELSEIF p_time = 8.
        p_time = 9.
      ELSEIF p_time = 9.
        p_time = 10.
      ELSEIF p_time GE 10.
        p_time = 2.
      ELSEIF p_time = 3.
        p_time = 99.
      ENDIF.

    ELSEIF p_n_names EQ 4. "  Template com 4 nomes - Template 1
* Template com 4 nomes - Template 1
* F1 / F4 / F5 / F6 / F7 / F2 / F3
      IF p_time = 1.
        p_time = 4.
      ELSEIF p_time = 4.
        p_time = 5.
      ELSEIF p_time = 5.
        p_time = 6.
      ELSEIF p_time = 6.
        p_time = 7.
      ELSEIF p_time = 7.
        p_time = 2.
      ELSEIF p_time = 2.
        p_time = 3.
      ELSEIF p_time = 3.
        p_time = 99.
      ENDIF.

    ELSEIF p_n_names EQ 3. "  Template com 3 nomes - Template 2
* Template com 3 nomes - Template 2
* F1 / F4 / F2 / F3
      IF p_time = 1.
        p_time = 4.
      ELSEIF p_time = 4.
        p_time = 2.
      ELSEIF p_time = 2.
        p_time = 3.
      ELSEIF p_time = 3.
        p_time = 99.
      ENDIF.

    ELSEIF p_n_names EQ 2. "  Template com 2 nomes - Template 3
* Template com 2 nomes - Template 3
* F1 / F2 / F3
      IF p_time = 1.
        p_time = 2.
      ELSEIF p_time = 2.
        p_time = 3.
      ELSEIF p_time = 3.
        p_time = 99.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
* <<< END Inetum SAM EMP/SS HR 7000218401 27.01.2025

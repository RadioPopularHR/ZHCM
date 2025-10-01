*----------------------------------------------------------------------*
***INCLUDE LZHR_WORKVIVOF01.
*----------------------------------------------------------------------*
FORM update_titulotrab.

  DATA: lv_index LIKE sy-tabix.
  DATA: BEGIN OF lt_total.
          INCLUDE STRUCTURE zhr_titulotrab_v.
          INCLUDE  STRUCTURE vimtbflags.
        DATA  END OF lt_total.
  DATA: ls_record TYPE zhr_titulotrab_v.

  LOOP AT total INTO lt_total.
    IF lt_total-vim_action = aendern OR
       lt_total-vim_action = neuer_eintrag.
      MOVE-CORRESPONDING lt_total TO ls_record.
      ls_record-usr_alteracao = sy-uname.
      ls_record-tm_alteracao = sy-uzeit.
      ls_record-dt_alteracao = sy-datum.
      READ TABLE extract WITH KEY lt_total.
      IF sy-subrc EQ 0.
        lv_index = sy-tabix.
      ELSE.
        CLEAR lv_index.
      ENDIF.
      MOVE-CORRESPONDING ls_record TO lt_total.
      MODIFY total FROM lt_total.
      CHECK lv_index GT 0.
      MODIFY extract INDEX lv_index FROM lt_total.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM update_subarea.

  DATA: lv_index LIKE sy-tabix.
  DATA: BEGIN OF lt_total.
          INCLUDE STRUCTURE zhr_subarea.
          INCLUDE  STRUCTURE vimtbflags.
        DATA  END OF lt_total.
  DATA: ls_record TYPE zhr_subarea.

  LOOP AT total INTO lt_total.
    IF lt_total-vim_action = aendern OR
       lt_total-vim_action = neuer_eintrag.
      MOVE-CORRESPONDING lt_total TO ls_record.
      ls_record-usr_alteracao = sy-uname.
      ls_record-tm_alteracao = sy-uzeit.
      ls_record-dt_alteracao = sy-datum.
      READ TABLE extract WITH KEY lt_total.
      IF sy-subrc EQ 0.
        lv_index = sy-tabix.
      ELSE.
        CLEAR lv_index.
      ENDIF.
      MOVE-CORRESPONDING ls_record TO lt_total.
      MODIFY total FROM lt_total.
      CHECK lv_index GT 0.
      MODIFY extract INDEX lv_index FROM lt_total.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM update_des_equipa.

  DATA: lv_index LIKE sy-tabix.
  DATA: BEGIN OF lt_total.
          INCLUDE STRUCTURE zhr_des_equipa_v.
          INCLUDE  STRUCTURE vimtbflags.
        DATA  END OF lt_total.
  DATA: ls_record TYPE zhr_des_equipa_v.

  LOOP AT total INTO lt_total.
    IF lt_total-vim_action = aendern OR
       lt_total-vim_action = neuer_eintrag.
      MOVE-CORRESPONDING lt_total TO ls_record.
      ls_record-usr_alteracao = sy-uname.
      ls_record-tm_alteracao = sy-uzeit.
      ls_record-dt_alteracao = sy-datum.
      READ TABLE extract WITH KEY lt_total.
      IF sy-subrc EQ 0.
        lv_index = sy-tabix.
      ELSE.
        CLEAR lv_index.
      ENDIF.
      MOVE-CORRESPONDING ls_record TO lt_total.
      MODIFY total FROM lt_total.
      CHECK lv_index GT 0.
      MODIFY extract INDEX lv_index FROM lt_total.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM update_centro_custo.

  DATA: lv_index LIKE sy-tabix.
  DATA: BEGIN OF lt_total.
          INCLUDE STRUCTURE zhr_centro_custo.
          INCLUDE  STRUCTURE vimtbflags.
        DATA  END OF lt_total.
  DATA: ls_record TYPE zhr_centro_custo.

  LOOP AT total INTO lt_total.
    IF lt_total-vim_action = aendern OR
       lt_total-vim_action = neuer_eintrag.
      MOVE-CORRESPONDING lt_total TO ls_record.
      ls_record-usr_alteracao = sy-uname.
      ls_record-tm_alteracao = sy-uzeit.
      ls_record-dt_alteracao = sy-datum.
      READ TABLE extract WITH KEY lt_total.
      IF sy-subrc EQ 0.
        lv_index = sy-tabix.
      ELSE.
        CLEAR lv_index.
      ENDIF.
      MOVE-CORRESPONDING ls_record TO lt_total.
      MODIFY total FROM lt_total.
      CHECK lv_index GT 0.
      MODIFY extract INDEX lv_index FROM lt_total.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM update_funcao.

  DATA: lv_index LIKE sy-tabix.
  DATA: BEGIN OF lt_total.
          INCLUDE STRUCTURE zhr_funcao.
          INCLUDE  STRUCTURE vimtbflags.
        DATA  END OF lt_total.
  DATA: ls_record TYPE zhr_funcao.

  LOOP AT total INTO lt_total.
    IF lt_total-vim_action = aendern OR
       lt_total-vim_action = neuer_eintrag.
      MOVE-CORRESPONDING lt_total TO ls_record.
      ls_record-usr_alteracao = sy-uname.
      ls_record-tm_alteracao = sy-uzeit.
      ls_record-dt_alteracao = sy-datum.
      READ TABLE extract WITH KEY lt_total.
      IF sy-subrc EQ 0.
        lv_index = sy-tabix.
      ELSE.
        CLEAR lv_index.
      ENDIF.
      MOVE-CORRESPONDING ls_record TO lt_total.
      MODIFY total FROM lt_total.
      CHECK lv_index GT 0.
      MODIFY extract INDEX lv_index FROM lt_total.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM update_idchefia.

  DATA: lv_index LIKE sy-tabix.
  DATA: BEGIN OF lt_total.
          INCLUDE STRUCTURE zhr_idchefia.
          INCLUDE  STRUCTURE vimtbflags.
        DATA  END OF lt_total.
  DATA: ls_record TYPE zhr_idchefia.

  LOOP AT total INTO lt_total.
    IF lt_total-vim_action = aendern OR
       lt_total-vim_action = neuer_eintrag.
      MOVE-CORRESPONDING lt_total TO ls_record.
      ls_record-usr_alteracao = sy-uname.
      ls_record-tm_alteracao = sy-uzeit.
      ls_record-dt_alteracao = sy-datum.
      READ TABLE extract WITH KEY lt_total.
      IF sy-subrc EQ 0.
        lv_index = sy-tabix.
      ELSE.
        CLEAR lv_index.
      ENDIF.
      MOVE-CORRESPONDING ls_record TO lt_total.
      MODIFY total FROM lt_total.
      CHECK lv_index GT 0.
      MODIFY extract INDEX lv_index FROM lt_total.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM update_email.

  DATA: lv_index LIKE sy-tabix.
  DATA: BEGIN OF lt_total.
          INCLUDE STRUCTURE zhr_email.
          INCLUDE  STRUCTURE vimtbflags.
        DATA  END OF lt_total.
  DATA: ls_record TYPE zhr_email.

  LOOP AT total INTO lt_total.
    IF lt_total-vim_action = aendern OR
       lt_total-vim_action = neuer_eintrag.
      MOVE-CORRESPONDING lt_total TO ls_record.
      ls_record-usr_alteracao = sy-uname.
      ls_record-tm_alteracao = sy-uzeit.
      ls_record-dt_alteracao = sy-datum.
      READ TABLE extract WITH KEY lt_total.
      IF sy-subrc EQ 0.
        lv_index = sy-tabix.
      ELSE.
        CLEAR lv_index.
      ENDIF.
      MOVE-CORRESPONDING ls_record TO lt_total.
      MODIFY total FROM lt_total.
      CHECK lv_index GT 0.
      MODIFY extract INDEX lv_index FROM lt_total.
    ENDIF.
  ENDLOOP.

ENDFORM.

MODULE zliste_update_liste.                                 "#EC CALLED
  CHECK status-action NE anzeigen.
  CHECK status-action NE transportieren.
  CHECK status-delete NE geloescht.
  IF ok_code NE 'IGN '.
*    IF x_header-ptfrkyexst NE space.
*      PERFORM consistency_prt_frky_fields
*                                   USING vim_show_consistency_alert.
*    ENDIF.
    CASE status-action.                "UFprofile
      WHEN kopieren.                   "UFprofile
        READ TABLE vim_copied_indices WITH KEY level = vim_copy_call_level
                                                     ex_ix = nextline. "#EC *
        IF sy-subrc EQ 0.              "entry alr. processed
          <xact> = neuer_eintrag.
          PERFORM update_tab.
        ELSE.
          PERFORM kopiere_eintrag USING <orig_key>.
        ENDIF.
*       Check whether the function code is not assigned
        IF function IS INITIAL.
*         Maintenance screen should behave as the function code ENTER
          function = 'KOPF'.
        ENDIF. " IF function IS INITIAL
      WHEN OTHERS.
        PERFORM update_tab.
    ENDCASE.
  ELSE.
    PERFORM set_pf_status USING 'ERRORLIS'.
  ENDIF.
  IF replace_mode EQ space.
    CLEAR ok_code.
  ENDIF.
ENDMODULE.                    "liste_update_liste

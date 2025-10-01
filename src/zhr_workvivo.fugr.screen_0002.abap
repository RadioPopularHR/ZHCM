PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zhr_des_equipa_v CURSOR nextline.
    MODULE liste_show_liste.
*   MODULE liste_deactivate.                        "ROF-20240401-203624
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD zhr_des_equipa_v-codigo .
      FIELD zhr_des_equipa_v-endda .
      FIELD zhr_des_equipa_v-begda .
      FIELD zhr_des_equipa_v-descricao .
      FIELD zhr_des_equipa_v-dt_alteracao .
      FIELD zhr_des_equipa_v-tm_alteracao .
      FIELD zhr_des_equipa_v-usr_alteracao .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    CHAIN.                                         "ROF-20240401-203624
      FIELD zhr_des_equipa_v-begda.                "ROF-20240401-203624
      FIELD zhr_des_equipa_v-endda.                "ROF-20240401-203624
      MODULE temp_delimitation ON CHAIN-REQUEST.   "ROF-20240401-203624
    ENDCHAIN.                                      "ROF-20240401-203624
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD zhr_des_equipa_v-codigo .
      FIELD zhr_des_equipa_v-endda .
*      MODULE liste_update_liste.
      MODULE zliste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.

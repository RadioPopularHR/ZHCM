PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zhr_subarea CURSOR nextline.
    MODULE liste_show_liste.
    MODULE liste_deactivate.  "ROF-20240402-125620
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD zhr_subarea-btrtl .
      FIELD zhr_subarea-endda .
      FIELD zhr_subarea-begda .
      FIELD zhr_subarea-descricao .
      FIELD zhr_subarea-dt_alteracao .
      FIELD zhr_subarea-tm_alteracao .
      FIELD zhr_subarea-usr_alteracao .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    CHAIN.        "ROF-20240402-125620
      FIELD zhr_subarea-begda.    "ROF-20240402-125620
      FIELD zhr_subarea-endda.   "ROF-20240402-125620
      MODULE temp_delimitation ON CHAIN-REQUEST.   "ROF-20240402-125620
    ENDCHAIN.   "ROF-20240402-125620
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD zhr_subarea-btrtl .
      FIELD zhr_subarea-endda .
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.

PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zhr_centro_custo CURSOR nextline.
    MODULE liste_show_liste.
    MODULE liste_deactivate.                       "ROF-20240402-125335
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD zhr_centro_custo-kostl .
      FIELD zhr_centro_custo-endda .
      FIELD zhr_centro_custo-begda .
      FIELD zhr_centro_custo-descricao .
      FIELD zhr_centro_custo-mail.
      FIELD zhr_centro_custo-dt_alteracao .
      FIELD zhr_centro_custo-tm_alteracao .
      FIELD zhr_centro_custo-usr_alteracao .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    CHAIN.                                        "ROF-20240402-125335
      FIELD zhr_centro_custo-begda.                "ROF-20240402-125335
      FIELD zhr_centro_custo-endda.                "ROF-20240402-125335
      MODULE temp_delimitation ON CHAIN-REQUEST.   "ROF-20240402-125335
    ENDCHAIN.                                     "ROF-20240402-125335
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD zhr_centro_custo-kostl .
      FIELD zhr_centro_custo-endda .
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.

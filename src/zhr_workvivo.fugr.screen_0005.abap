PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zhr_funcao CURSOR nextline.
    MODULE liste_show_liste.
    MODULE liste_deactivate.
"ROF-20240423-215138
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD zhr_funcao-plans .
      FIELD zhr_funcao-endda .
      FIELD zhr_funcao-begda .
      FIELD zhr_funcao-descricao .
      FIELD zhr_funcao-dt_alteracao .
      FIELD zhr_funcao-tm_alteracao .
      FIELD zhr_funcao-usr_alteracao .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    CHAIN.
"ROF-20240423-215138
      FIELD zhr_funcao-begda.
"ROF-20240423-215138
      FIELD zhr_funcao-endda.
"ROF-20240423-215138
      MODULE temp_delimitation ON CHAIN-REQUEST.
"ROF-20240423-215138
    ENDCHAIN.
"ROF-20240423-215138
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD zhr_funcao-plans .
      FIELD zhr_funcao-endda .
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.

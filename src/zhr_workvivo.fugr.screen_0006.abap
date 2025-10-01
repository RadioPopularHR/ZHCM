PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zhr_idchefia CURSOR nextline.
    MODULE liste_show_liste.
    MODULE liste_deactivate.
"ROF-20240411-141809
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD zhr_idchefia-idchefia .
      FIELD zhr_idchefia-endda .
      FIELD zhr_idchefia-begda .
      FIELD zhr_idchefia-dt_alteracao .
      FIELD zhr_idchefia-tm_alteracao .
      FIELD zhr_idchefia-usr_alteracao .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    CHAIN.
"ROF-20240411-141809
      FIELD zhr_idchefia-begda.
"ROF-20240411-141809
      FIELD zhr_idchefia-endda.
"ROF-20240411-141809
      MODULE temp_delimitation ON CHAIN-REQUEST.
"ROF-20240411-141809
    ENDCHAIN.
"ROF-20240411-141809
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD zhr_idchefia-idchefia .
      FIELD zhr_idchefia-endda .
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.

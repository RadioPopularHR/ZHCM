PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zhr_email CURSOR nextline.
    MODULE liste_show_liste.
    MODULE liste_deactivate.
"ROF-20240423-210517
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD zhr_email-email .
      FIELD zhr_email-endda .
      FIELD zhr_email-begda .
      FIELD zhr_email-dt_alteracao .
      FIELD zhr_email-tm_alteracao .
      FIELD zhr_email-usr_alteracao .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    CHAIN.
"ROF-20240423-210517
      FIELD zhr_email-begda.
"ROF-20240423-210517
      FIELD zhr_email-endda.
"ROF-20240423-210517
      MODULE temp_delimitation ON CHAIN-REQUEST.
"ROF-20240423-210517
    ENDCHAIN.
"ROF-20240423-210517
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD zhr_email-email .
      FIELD zhr_email-endda .
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.

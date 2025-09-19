
PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zhrdiuturnidade CURSOR nextline.
    MODULE liste_show_liste.
    MODULE liste_deactivate.
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD zhrdiuturnidade-trfar .
      FIELD zhrdiuturnidade-trfgb .
      FIELD zhrdiuturnidade-btrtl .
      FIELD zhrdiuturnidade-endda .
      FIELD zhrdiuturnidade-begda .
      FIELD zhrdiuturnidade-anos .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    CHAIN.
      FIELD zhrdiuturnidade-endda .
      FIELD zhrdiuturnidade-begda .
      MODULE temp_delimitation ON CHAIN-REQUEST.
    ENDCHAIN.

    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD zhrdiuturnidade-trfar .
      FIELD zhrdiuturnidade-trfgb .
      FIELD zhrdiuturnidade-btrtl .
      FIELD zhrdiuturnidade-endda .
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.

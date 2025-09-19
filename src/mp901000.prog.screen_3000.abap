PROCESS BEFORE OUTPUT.
*---------------------------------------------------------------------*
*       Setzt PFSTATUS und fuehrt Bildmodifikationen durch.           *
*---------------------------------------------------------------------*
  MODULE before_output.
  CALL SUBSCREEN subscreen_empl INCLUDING empl_prog empl_dynnr.
  MODULE assign_tc3000.
  MODULE variation_tc.
*---------------------------------------------------------------------*
*       Holt Infotypsaetze fuer die Anzeige.                          *
*---------------------------------------------------------------------*
  LOOP.
    MODULE pslist_tc.
*---------------------------------------------------------------------*
*       Infotypspezifischer Modul fuer Textanzeige.                   *
*---------------------------------------------------------------------*
    MODULE p9010l.
  ENDLOOP.
  CALL SUBSCREEN subscreen_header INCLUDING header_prog header_dynnr.
*
PROCESS AFTER INPUT.
*---------------------------------------------------------------------*
*       Eingabe von 'E' im OK-Code                                    *
*---------------------------------------------------------------------*
  MODULE exit AT EXIT-COMMAND.
*---------------------------------------------------------------------*
*       Verarbeitung nach der Eingabe.                                *
*---------------------------------------------------------------------*
  LOOP.
    FIELD rp50m-sele2 MODULE mark ON REQUEST.
  ENDLOOP.
*---------------------------------------------------------------------*
*       Bei der Eingabe vom Gueltigkeitsintervall oder Infosubtyp     *
*       wird ein neuer Bereich zur Anzeige ausgewaehlt.               *
*---------------------------------------------------------------------*
  CALL SUBSCREEN subscreen_empl.
  CHAIN.
    FIELD rp50m-begda.
    FIELD rp50m-endda.
    FIELD rp50m-subty.
    MODULE select_for_list ON CHAIN-REQUEST.
  ENDCHAIN.
  FIELD rp50m-pagea ON REQUEST MODULE top_of_list.
  MODULE adjust_list_tc.
*---------------------------------------------------------------------*
*       Allgemeine Funktionscodebearbeitung.                          *
*---------------------------------------------------------------------*
  MODULE post_input_checks.

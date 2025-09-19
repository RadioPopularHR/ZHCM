*----------------------------------------------------------------------*
* Vorreport zur Ausbildungshistorie
*----------------------------------------------------------------------*
REPORT zhr_rhxthist_f MESSAGE-ID pv NO STANDARD PAGE HEADING.

INCLUDE: rhscmdat.
INCLUDE: rhscmpar.
INCLUDE: rhxmacro.
INCLUDE: rhxf4get.
INCLUDE: <icon>.


* Block 1
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
PARAMETERS pchplvar LIKE pchdy-plvar NO-DISPLAY.         "Planvariante
PARAMETERS pchotype LIKE pchdy-otype NO-DISPLAY.         "Objekttyp
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS ptyp1 LIKE hrvpv-ptyp1 RADIOBUTTON GROUP xxx. "Objekttyp1
SELECTION-SCREEN COMMENT 4(22) ptxt1 FOR FIELD ptyp1.
SELECTION-SCREEN POSITION 27.
PARAMETERS ptyp2 LIKE hrvpv-ptyp2 RADIOBUTTON GROUP xxx. "Objekttyp2
SELECTION-SCREEN COMMENT 30(22) ptxt2 FOR FIELD ptyp2.
SELECTION-SCREEN POSITION 53.
PARAMETERS ptyp3 LIKE hrvpv-ptyp3 RADIOBUTTON GROUP xxx. "Objekttyp3
SELECTION-SCREEN COMMENT 56(22) ptxt3 FOR FIELD ptyp3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS ptyp4 LIKE hrvpv-ptyp4 RADIOBUTTON GROUP xxx. "Objekttyp4
SELECTION-SCREEN COMMENT 4(22) ptxt4 FOR FIELD ptyp4.
SELECTION-SCREEN POSITION 27.
PARAMETERS ptyp5 LIKE hrvpv-ptyp5 RADIOBUTTON GROUP xxx. "Objekttyp5
SELECTION-SCREEN COMMENT 30(22) ptxt5 FOR FIELD ptyp5.
SELECTION-SCREEN POSITION 53.
PARAMETERS ptyp6 LIKE hrvpv-ptyp6 RADIOBUTTON GROUP xxx. "Objekttyp6
SELECTION-SCREEN COMMENT 56(20) ptxt6 FOR FIELD ptyp6.
SELECTION-SCREEN PUSHBUTTON 76(4) morepart USER-COMMAND more.
SELECTION-SCREEN END OF LINE.

rhx-f4-objid-data.
SELECT-OPTIONS pchobjid FOR objec-realo NO INTERVALS.      "Teilnehmer
PARAMETERS pchseark LIKE pchdy-seark.  "Suchbegriff

PARAMETERS pchostat LIKE pchdy-ostat NO-DISPLAY.         "Objektstatus
PARAMETERS pchistat LIKE pchdy-istat NO-DISPLAY.          "Datenstatus

PARAMETERS pchwegid LIKE pchdy-wegid NO-DISPLAY.
PARAMETERS pchsvect LIKE pchdy-svect NO-DISPLAY.
PARAMETERS pchactiv LIKE pchdy-activ NO-DISPLAY.
PARAMETERS pchdepth LIKE pchdy-depth NO-DISPLAY.
PARAMETERS pchdymod LIKE t77s3-dymod NO-DISPLAY.       "Dynp.variation
SELECTION-SCREEN END OF BLOCK b1.

rhx-obeg-radios-define.



SELECTION-SCREEN SKIP.
PARAMETER sel_box LIKE pp00-hr_via_sel.



INITIALIZATION.
  rhx-radios-init.
  PERFORM read_t77s0_parameters.
  PERFORM read_t77s0_parameters_ext.
  IF pchplvar IS INITIAL.
    pchplvar = $plvar.
  ENDIF.
  PERFORM init_participant_otypes.
  CALL FUNCTION 'RHVM_USET_GET_REPORTING'   "YMO 28.12.99
    IMPORTING
      stdsel = sel_box
    EXCEPTIONS
      OTHERS = 0.



AT SELECTION-SCREEN OUTPUT.
  rhx-radios-pbo.
  PERFORM set_participant_type.

AT SELECTION-SCREEN ON pchobjid.
  PERFORM check_objid_sign(rhxchk00) USING pchobjid-sign.

AT SELECTION-SCREEN.
  PERFORM user_command.
  PERFORM get_participant_type.
  rhx-radios-pai.
  IF NOT pchotype IN expart_tab.       "Wenn kein externes Objekt >C8
*   Uebertragung PCHOBJID nach HPCHOBJID
    REFRESH hpchobjid.
    LOOP AT pchobjid.
      CLEAR hpchobjid.
      MOVE-CORRESPONDING pchobjid TO hpchobjid.
      APPEND hpchobjid.
    ENDLOOP.
    PERFORM check_seark(rhxchk00) TABLES hpchobjid
                                  USING  pchseark pchplvar pchotype.
*   Uebertragung HPCHOBJID nach PCHOBJID
    REFRESH pchobjid.
    LOOP AT hpchobjid.
      CLEAR pchobjid.
      MOVE-CORRESPONDING hpchobjid TO pchobjid.
      WRITE pchobjid-low TO pchobjid-low NO-ZERO.
      CONDENSE pchobjid-low NO-GAPS.
      WRITE pchobjid-high TO pchobjid-high NO-ZERO.
      CONDENSE pchobjid-high NO-GAPS.
      APPEND pchobjid.
    ENDLOOP.
  ELSE.
    PERFORM check_seark_realo(rhxchk00) TABLES pchobjid
            USING  pchseark pchplvar pchotype.
  ENDIF.
  PERFORM check_plvar(rhxchk00) USING pchplvar.
  PERFORM check_obeg_oend(rhxchk00) USING pchobeg pchoend.
  CALL FUNCTION 'RHVM_USET_SET_REPORTING'   "YMO 28.12.99
    EXPORTING
      stdsel = sel_box
    EXCEPTIONS
      OTHERS = 0.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pchobjid-low.
  h_dynnr = sy-dynnr.
  h_repid = sy-repid.
  PERFORM get_value_extended TABLES ptyp_tab
                             USING '6' h_repid h_dynnr pchotype.
  rhx-f4-objid-low pchplvar pchotype pchobjid '' ''.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pchobjid-high.
  h_dynnr = sy-dynnr.
  h_repid = sy-repid.
  PERFORM get_value_extended TABLES ptyp_tab
                             USING '6' h_repid h_dynnr pchotype.
  rhx-f4-objid-high pchplvar pchotype pchobjid '' ''.



START-OF-SELECTION.
  IF NOT pchotype IN expart_tab.
*   Uebertragung PCHOBJID nach HPCHOBJID
    REFRESH hpchobjid.
    LOOP AT pchobjid.
      CLEAR hpchobjid.
      MOVE-CORRESPONDING pchobjid TO hpchobjid.
      APPEND hpchobjid.
    ENDLOOP.
  ENDIF.

  IF NOT sel_box IS INITIAL.
    IF sy-subty O print.
*--> Begin of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021
*      SUBMIT rhthist0 TO SAP-SPOOL VIA SELECTION-SCREEN
      SUBMIT zhr_rhthist0 TO SAP-SPOOL VIA SELECTION-SCREEN
*<-- End of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021
        SPOOL PARAMETERS %_print
        ARCHIVE PARAMETERS %_archive
        WITHOUT SPOOL DYNPRO
        WITH pchplvar EQ pchplvar
        WITH pchotype EQ pchotype
        WITH pchobjid IN hpchobjid
        WITH pchsobid IN pchobjid
        WITH pchseark EQ pchseark
        WITH pchobeg  EQ pchobeg
        WITH pchoend  EQ pchoend
        WITH pchtimed EQ pchtimed
        WITH pchbegda EQ pchobeg
        WITH pchendda EQ pchoend.
    ELSE.
*--> Begin of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021
*      SUBMIT rhthist0 VIA SELECTION-SCREEN
      SUBMIT zhr_rhthist0 VIA SELECTION-SCREEN
*<-- End of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021
        WITH pchplvar EQ pchplvar
        WITH pchotype EQ pchotype
        WITH pchobjid IN hpchobjid
        WITH pchsobid IN pchobjid
        WITH pchseark EQ pchseark
        WITH pchobeg  EQ pchobeg
        WITH pchoend  EQ pchoend
        WITH pchtimed EQ pchtimed
        WITH pchbegda EQ pchobeg
        WITH pchendda EQ pchoend.
    ENDIF.
  ELSE.
    IF sy-subty O print.
*--> Begin of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021
*      SUBMIT rhthist0 TO SAP-SPOOL
      SUBMIT zhr_rhthist0 TO SAP-SPOOL
*<-- End of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021
        SPOOL PARAMETERS %_print
        ARCHIVE PARAMETERS %_archive
        WITHOUT SPOOL DYNPRO
        WITH pchplvar EQ pchplvar
        WITH pchotype EQ pchotype
        WITH pchobjid IN hpchobjid
        WITH pchsobid IN pchobjid
        WITH pchseark EQ pchseark
        WITH pchobeg  EQ pchobeg
        WITH pchoend  EQ pchoend
        WITH pchtimed EQ pchtimed
        WITH pchbegda EQ pchobeg
        WITH pchendda EQ pchoend.
    ELSE.
*--> Begin of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021
*      SUBMIT rhthist0 AND RETURN
      SUBMIT zhr_rhthist0 AND RETURN
*<-- End of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021

        WITH pchplvar EQ pchplvar
        WITH pchotype EQ pchotype
        WITH pchobjid IN hpchobjid
        WITH pchsobid IN pchobjid
        WITH pchseark EQ pchseark
        WITH pchobeg  EQ pchobeg
        WITH pchoend  EQ pchoend
        WITH pchtimed EQ pchtimed
        WITH pchbegda EQ pchobeg
        WITH pchendda EQ pchoend.
    ENDIF.
  ENDIF.



  INCLUDE rhscmfor.

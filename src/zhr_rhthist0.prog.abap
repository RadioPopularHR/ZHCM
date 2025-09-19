*----------------------------------------------------------------------*
* Ausbildungshistorie eines Teilnehmers                                *
*----------------------------------------------------------------------*
REPORT zhr_rhthist0 MESSAGE-ID pv NO STANDARD PAGE HEADING LINE-SIZE 131.

TABLES: objec, gdstr, gdset, pad25, pad34.

INFOTYPES: 1001 NAME i1001 MODE N.
INFOTYPES: 1031 NAME i1031 MODE N.


INCLUDE: rhreptop.
INCLUDE: rhodat00.
INCLUDE: rhrdat00.
INCLUDE: rhrdat10.
INCLUDE: rhauthtop.
INCLUDE: rhalvinc.

*--> Begin of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021
*DATA alv_itab LIKE hrv_rhthist0 OCCURS 0 WITH HEADER LINE.
DATA alv_itab LIKE zhr_hrv_rhthist0 OCCURS 0 WITH HEADER LINE.
DATA: gt_p0001 TYPE TABLE OF p0001,
      gt_t001p TYPE TABLE OF t001p.
*<-- End of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021

DATA: save_patyp LIKE objec-otype,
      $rkurs     LIKE plog-objid,
      $rplva     LIKE plog-plvar,
      tabix      LIKE sy-tabix,
      h_short    LIKE hrvres-short,
      h_stext    LIKE hrvres-stext,
      h_begda    LIKE hrvres-begda,
      h_endda    LIKE hrvres-endda.
* Tabelle: Ausbildungshistorie
DATA: BEGIN OF partic_training_tab OCCURS 0,
        plvar       LIKE hrsobid-plvar,
        patyp       LIKE hrvpartic-patyp,   "Teilnehmertyp
        parid       LIKE hrvpartic-parid,   "TeilnehmerID
        parsh       LIKE hrvpartic-parsh,   "Teilnehmertext
        partx       LIKE hrvpartic-partx,   "Teilnehmertext
        evtyp       LIKE hrvpartic-evtyp,   "Veranstaltungs(typ)-Typ
        eveid       LIKE hrvpartic-eveid,   "Veranstaltungs(typ)-ID
        evsht       LIKE objec-short,       "Veranstaltungs(typ)-Text
        evtxt       LIKE objec-stext,       "Veranstaltungs(typ)-Text
        evbeg       LIKE objec-begda,       "Veranstaltungsbeginn
        evend       LIKE objec-endda,       "Veranstaltungsende
*       Zusatzdaten
        evday       LIKE hrvsched-ndays,    "eff.Veranst.dauer in Tagen
        evhour      LIKE hrvsched-nhours,   "eff.Veranst.dauer in Stunden
        kkost       LIKE hrvkostl-kkost,    "Veranstaltungspreis
        kwaer       LIKE hrvkostl-kwaer,    "Währung für Teilnahmepreis
        manzl       LIKE hrvpad25-manzl,    "Anzahl Buchungen
        priox       LIKE hrvpad25-priox,    "Buchungspriorität
        seqnr       LIKE hrvpad25-seqnr,    "Seq.Nr.
        budat       LIKE hrvpad25-budat,    "Buchungsdatum
        no_adata(1).                   "Keine Zusatzdaten
DATA: END OF partic_training_tab.
DATA: BEGIN OF de_check_tab OCCURS 0,
        patyp LIKE hrvpartic-patyp,
        parid LIKE hrvpartic-parid,
        evtid LIKE hrvpartic-eveid,
        evbeg LIKE objec-begda,
        evend LIKE objec-endda.
DATA: END OF de_check_tab.
DATA: i1001_ext       LIKE hri1001 OCCURS 1 WITH HEADER LINE,
      patyp_tab       LIKE hrvprotp OCCURS 1 WITH HEADER LINE,
      multi_tab       LIKE hrvprotp OCCURS 1 WITH HEADER LINE,
      expart_tab      LIKE hrvprotp OCCURS 1 WITH HEADER LINE,
      ext_objects_tab LIKE hrsobid OCCURS 1 WITH HEADER LINE,
      int_objects_tab LIKE hrobject OCCURS 1 WITH HEADER LINE,
      partic_tab      LIKE hrsobid OCCURS 1 WITH HEADER LINE,
      event_tab       LIKE hrobject OCCURS 1 WITH HEADER LINE.
DATA: BEGIN OF participant_tab OCCURS 1,
        plvar LIKE hrsobid-plvar,
        patyp LIKE hrvpartic-patyp,
        parid LIKE hrvpartic-parid,
        parsh LIKE hrvpartic-parsh,
        partx LIKE hrvpartic-partx.
DATA: END OF participant_tab.




AT SELECTION-SCREEN.
  PERFORM check_otype.




INITIALIZATION.
  INCLUDE: rhoini00.
  INCLUDE: rhrini00.
  INCLUDE: rhrini10.
  pchplvar = $plvar.
  pchotype = $kurst.
  pchtimed = 'P'.
  PERFORM re77s0(mstt77s0) USING 'RAUM' 'RKURS'
                                 $rkurs subrc.
  PERFORM re77s0(mstt77s0) USING 'RAUM' 'RPLVA'
                                 $rplva subrc.
  PERFORM re77s0(mstt77s0) USING 'OTYPE' 'KURS'
                                 $kurs subrc.
  IF subrc NE 0.
    $kurs = 'E'.
  ENDIF.
  PERFORM re77s0(mstt77s0) USING 'OTYPE' 'KURST'
                                 $kurst subrc.
  IF subrc NE 0.
    $kurst = 'D'.
  ENDIF.
  PERFORM re77s0(mstt77s0) USING 'PPREL' 'PARTB'
                                 $partb subrc.
  IF subrc NE 0.
    $partb = 'B025'.
  ENDIF.
  PERFORM re77s0(mstt77s0) USING 'PPREL' 'KNOWA'
                                 $knowa subrc.
  IF subrc NE 0.
    $knowb = 'B034'.
  ELSE.
    PERFORM $transform_relat(sapfh5an) USING $knowa $knowb.
  ENDIF.
  CLEAR: patyp_tab[], multi_tab[], expart_tab[].
* erlaubten Teilnehmertypen
  CALL FUNCTION 'RH_PART_TYPES_GET'
    TABLES
      patyp_tab = patyp_tab
    EXCEPTIONS
      OTHERS    = 1.
  IF sy-subrc = 0. ENDIF.
* Sammelteilnehmertypen
  CALL FUNCTION 'RH_PART_TYPES_MULTI'
    TABLES
      multi_tab = multi_tab
    EXCEPTIONS
      OTHERS    = 1.
  IF sy-subrc = 0. ENDIF.
* externer Teilnehmertypen
  CALL FUNCTION 'RH_PART_TYPES_EXT'
    TABLES
      expart_tab = expart_tab
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc = 0. ENDIF.
  CLEAR: partic_training_tab[], participant_tab[], ext_objects_tab[],
         int_objects_tab[].
*--> Begin of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021
*  alv_ddic = 'HRV_RHTHIST0'.
  alv_ddic = 'ZHR_HRV_RHTHIST0'.
*<-- End of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021





GET objec.
  MOVE: objec-plvar TO participant_tab-plvar,
        objec-otype TO participant_tab-patyp,
        objec-realo TO participant_tab-parid,
        objec-short TO participant_tab-parsh,
        objec-stext TO participant_tab-partx.
  IF participant_tab-partx IS INITIAL.
    MOVE participant_tab-parsh TO participant_tab-partx.
  ENDIF.
  APPEND participant_tab.
  IF objec-otype IN expart_tab.
    MOVE: objec-plvar TO ext_objects_tab-plvar,
          objec-otype TO ext_objects_tab-otype,
          objec-realo TO ext_objects_tab-sobid.
    APPEND ext_objects_tab.
  ELSE.
    MOVE: objec-plvar TO int_objects_tab-plvar,
          objec-otype TO int_objects_tab-otype,
          objec-objid TO int_objects_tab-objid.
    APPEND int_objects_tab.
  ENDIF.


*--> Begin of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021
  DATA: lt_p0001 TYPE TABLE OF p0001,
        ls_p0001 TYPE p0001,
        lt_t001p TYPE TABLE OF t001p,
        ls_t001p TYPE t001p.
  CALL FUNCTION 'HR_READ_INFOTYPE'
    EXPORTING
      pernr           = objec-objid
      infty           = '0001'
      begda           = pchbegda
      endda           = pchendda
    TABLES
      infty_tab       = lt_p0001
    EXCEPTIONS
      infty_not_found = 1
      OTHERS          = 2.
  IF sy-subrc EQ 0.
    LOOP AT lt_p0001 INTO ls_p0001.
      APPEND ls_p0001 TO gt_p0001.
    ENDLOOP.

    SELECT * FROM t001p
      INTO TABLE lt_t001p
      FOR ALL ENTRIES IN lt_p0001
      WHERE werks = lt_p0001-werks
      AND btrtl = lt_p0001-btrtl.
    IF sy-subrc EQ 0.
      LOOP AT lt_t001p INTO ls_t001p.
        APPEND ls_t001p TO gt_t001p.
      ENDLOOP.
    ENDIF.
  ENDIF.
*<-- End of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021


END-OF-SELECTION.
  IF NOT participant_tab IS INITIAL.
    PERFORM read_1001_b025.
    PERFORM read_1001_b034.
    PERFORM alv_display.
  ELSE.
    MESSAGE s859.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  CHECK_OTYPE                                              *
*&---------------------------------------------------------------------*
*       Prüfung: Objekttyp zulässig?                                   *
*----------------------------------------------------------------------*
FORM check_otype.

  IF NOT pchotype IN patyp_tab.
    MESSAGE e456.
  ELSE.
    pchsvect = '12   '.
    save_patyp = pchotype.
  ENDIF.

ENDFORM.                               " CHECK_OTYPE

*&---------------------------------------------------------------------*
*&      Form  READ_1001_B034                                           *
*&---------------------------------------------------------------------*
*       Veranstaltungstypen die von Teilnehmer besucht wurden (B034)   *
*----------------------------------------------------------------------*
FORM read_1001_b034.

  CLEAR: i1001[], i1001_ext[], event_tab[].

  IF save_patyp IN expart_tab.         "externe Teilnehmertypen
    CALL FUNCTION 'RH_READ_INFTY_1001_EXT'
      EXPORTING
        authority      = 'DISP'
        with_stru_auth = 'X'
        istat          = pchistat
        extend         = 'X'
        subty          = $knowb
        begda          = pchbegda
        endda          = pchendda
      TABLES
        i1001          = i1001_ext
        objects        = ext_objects_tab
      EXCEPTIONS
        nothing_found  = 1
        OTHERS         = 3.
    IF sy-subrc <> 1.
      LOOP AT participant_tab.
        LOOP AT i1001_ext WHERE plvar = participant_tab-plvar AND
                                otype = participant_tab-patyp AND
                                objid = participant_tab-parid.
*-----
          READ TABLE de_check_tab WITH KEY patyp = participant_tab-patyp
                                           parid = participant_tab-parid
                                            evtid = i1001_ext-sobid
                                            evbeg = i1001_ext-begda
                                            evend = i1001_ext-endda
                                            BINARY SEARCH.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
*-----
          CLEAR: pad34, partic_training_tab.
          IF NOT rhthist0_auth_sobid IS INITIAL.
            CALL FUNCTION 'RH_STRU_AUTHORITY_CHECK'
              EXPORTING
                fcode                    = stru_authority
                plvar                    = i1001_ext-plvar
                otype                    = i1001_ext-sclas
                objid                    = i1001_ext-sobid
                with_base_ac             = base_authority
              EXCEPTIONS
                no_stru_authority        = 1
                no_stru_authority_hyper  = 2
                no_stru_authority_at_all = 3
                no_base_authority        = 4
                OTHERS                   = 5.
            IF sy-subrc >< 0.
              DELETE i1001_ext.
              CONTINUE.
            ENDIF.
          ENDIF.
          MOVE: participant_tab-plvar TO partic_training_tab-plvar,
                participant_tab-patyp TO partic_training_tab-patyp,
                participant_tab-parid TO partic_training_tab-parid,
                participant_tab-parsh TO partic_training_tab-parsh,
                participant_tab-partx TO partic_training_tab-partx,
                i1001_ext-sclas       TO partic_training_tab-evtyp,
                i1001_ext-sobid       TO partic_training_tab-eveid,
                i1001_ext-begda       TO partic_training_tab-evbeg,
                i1001_ext-endda       TO partic_training_tab-evend.
*         YMOUNI - start
*               i1001_ext-adata       TO pad34,
          CALL METHOD cl_hr_adata_type_cast=>adata_to_padnn
            EXPORTING
              adata = i1001_ext-adata
            IMPORTING
              padnn = pad34.
*         YMOUNI - end
          MOVE: i1001_ext-priox       TO partic_training_tab-priox,
                i1001_ext-seqnr       TO partic_training_tab-seqnr,
                pad34-manzl           TO partic_training_tab-manzl,
                pad34-ndays           TO partic_training_tab-evday,
                pad34-nhours          TO partic_training_tab-evhour,
                pad34-kkost           TO partic_training_tab-kkost,
                pad34-kwaer           TO partic_training_tab-kwaer.
          IF pad34 IS INITIAL.
            MOVE 'X' TO partic_training_tab-no_adata.
            IF NOT pc-otype IN multi_tab.
              MOVE 1 TO partic_training_tab-manzl.
            ENDIF.
          ENDIF.
          APPEND partic_training_tab.
          MOVE: participant_tab-plvar TO event_tab-plvar,
                i1001_ext-sclas       TO event_tab-otype,
                i1001_ext-sobid       TO event_tab-objid.
          COLLECT event_tab.
        ENDLOOP.
        IF sy-subrc = 4.
          CLEAR partic_training_tab.
          MOVE: participant_tab-plvar TO partic_training_tab-plvar,
                participant_tab-patyp TO partic_training_tab-patyp,
                participant_tab-parid TO partic_training_tab-parid,
                participant_tab-parsh TO partic_training_tab-parsh,
                participant_tab-partx TO partic_training_tab-partx.
          APPEND partic_training_tab.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSE.                                "interne Teilnehmertypen
    CALL FUNCTION 'RH_READ_INFTY_1001'
      EXPORTING
        authority      = 'DISP'
        with_stru_auth = 'X'
        istat          = pchistat
        extend         = 'X'
        subty          = $knowb
        begda          = pchbegda
        endda          = pchendda
      TABLES
        i1001          = i1001
        objects        = int_objects_tab
      EXCEPTIONS
        nothing_found  = 1
        OTHERS         = 3.
    IF sy-subrc <> 1.
      LOOP AT participant_tab.
        LOOP AT i1001 WHERE plvar = participant_tab-plvar AND
                            otype = participant_tab-patyp AND
                            objid = participant_tab-parid.
*-----
          READ TABLE de_check_tab WITH KEY patyp = participant_tab-patyp
                                           parid = participant_tab-parid
                                            evtid = i1001-sobid
                                            evbeg = i1001-begda
                                            evend = i1001-endda
                                            BINARY SEARCH.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
*-----
          CLEAR: pad34, partic_training_tab.
          IF NOT rhthist0_auth_sobid IS INITIAL.
            CALL FUNCTION 'RH_STRU_AUTHORITY_CHECK'
              EXPORTING
                fcode                    = stru_authority
                plvar                    = i1001-plvar
                otype                    = i1001-sclas
                objid                    = i1001-sobid
                with_base_ac             = base_authority
              EXCEPTIONS
                no_stru_authority        = 1
                no_stru_authority_hyper  = 2
                no_stru_authority_at_all = 3
                no_base_authority        = 4
                OTHERS                   = 5.
            IF sy-subrc >< 0.
              DELETE i1001.
              CONTINUE.
            ENDIF.
          ENDIF.
          MOVE: participant_tab-plvar TO partic_training_tab-plvar,
                participant_tab-patyp TO partic_training_tab-patyp,
                participant_tab-parid TO partic_training_tab-parid,
                participant_tab-parsh TO partic_training_tab-parsh,
                participant_tab-partx TO partic_training_tab-partx,
                i1001-sclas           TO partic_training_tab-evtyp,
                i1001-sobid           TO partic_training_tab-eveid,
                i1001-begda           TO partic_training_tab-evbeg,
                i1001-endda           TO partic_training_tab-evend.
*         YMOUNI - start
*               i1001-adata           TO pad34,
          CALL METHOD cl_hr_adata_type_cast=>adata_to_padnn
            EXPORTING
              adata = i1001-adata
            IMPORTING
              padnn = pad34.
*         YMOUNI - end
          MOVE: i1001-priox           TO partic_training_tab-priox,
                i1001-seqnr           TO partic_training_tab-seqnr,
                pad34-manzl           TO partic_training_tab-manzl,
                pad34-ndays           TO partic_training_tab-evday,
                pad34-nhours          TO partic_training_tab-evhour,
                pad34-kkost           TO partic_training_tab-kkost,
                pad34-kwaer           TO partic_training_tab-kwaer.
          IF pad34 IS INITIAL.
            MOVE 'X' TO partic_training_tab-no_adata.
            IF NOT pc-otype IN multi_tab.
              MOVE 1 TO partic_training_tab-manzl.
            ENDIF.
          ENDIF.
          APPEND partic_training_tab.
          MOVE: participant_tab-plvar TO event_tab-plvar,
                i1001-sclas           TO event_tab-otype,
                i1001-sobid           TO event_tab-objid.
          COLLECT event_tab.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDIF.

* Prüfung: Raumbelegung
  DELETE partic_training_tab WHERE plvar EQ $rplva AND
                                   eveid EQ $rkurs.
  DELETE event_tab WHERE plvar EQ $rplva AND
                         objid EQ $rkurs.

* Texte zu Veranstaltungstypen
  IF NOT event_tab IS INITIAL.
    CALL FUNCTION 'RH_TEXT_BUFFER_FILL'
      EXPORTING
        check_stru_auth = 'X'
        with_extint     = 'X'
      TABLES
        objects         = event_tab.
    LOOP AT event_tab.
      CLEAR : h_short, h_stext.
      CALL FUNCTION 'RH_READ_OBJECT'
        EXPORTING
          plvar           = event_tab-plvar
          otype           = event_tab-otype
          objid           = event_tab-objid
          istat           = pc-istat
          begda           = pc-begda
          endda           = pc-endda
          langu           = sy-langu
          ointerval       = 'X'
          store           = 'X'
          check_stru_auth = 'X'
        IMPORTING
          short           = h_short
          stext           = h_stext
        EXCEPTIONS
          not_found       = 01.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      LOOP AT partic_training_tab WHERE plvar = event_tab-plvar AND
                                        evtyp = event_tab-otype AND
                                        eveid = event_tab-objid.
        tabix = sy-tabix.
        partic_training_tab-evsht = h_short.
        IF NOT h_stext IS INITIAL.
          partic_training_tab-evtxt = h_stext.
        ELSE.
          partic_training_tab-evtxt = h_short.
        ENDIF.
        MODIFY partic_training_tab INDEX tabix.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

ENDFORM.                               " READ_1001_B034
*&---------------------------------------------------------------------*
*&      Form  READ_1001_B025                                           *
*&---------------------------------------------------------------------*
*       Buchungen der Teilnehmer (B025)                                *
*----------------------------------------------------------------------*
FORM read_1001_b025.

  DATA: schedule_tab LIKE hrvsched OCCURS 10 WITH HEADER LINE,
        kont_tab     LIKE hrvkont_n OCCURS 30 WITH HEADER LINE,
        h_kont_tab   LIKE hrvkont_n OCCURS 30 WITH HEADER LINE,
        kostl_tab    LIKE hrvkont_n OCCURS 30 WITH HEADER LINE,
        abl_days     LIKE partic_training_tab-evday,
        abl_hour     LIKE partic_training_tab-evhour.

  CLEAR: i1001[], i1001_ext[], event_tab[], h_kont_tab[].

  IF save_patyp IN expart_tab.         "externe Teilnehmertypen
    CALL FUNCTION 'RH_READ_INFTY_1001_EXT'
      EXPORTING
        authority      = 'DISP'
        with_stru_auth = 'X'
        istat          = pchistat
        extend         = 'X'
        subty          = $partb
        begda          = pchbegda
        endda          = pchendda
      TABLES
        i1001          = i1001_ext
        objects        = ext_objects_tab
      EXCEPTIONS
        nothing_found  = 1
        OTHERS         = 3.
    IF sy-subrc <> 1.
      LOOP AT participant_tab.
        LOOP AT i1001_ext WHERE plvar = participant_tab-plvar AND
                                otype = participant_tab-patyp AND
                                objid = participant_tab-parid.
          IF NOT rhthist0_auth_sobid IS INITIAL.
            CALL FUNCTION 'RH_STRU_AUTHORITY_CHECK'
              EXPORTING
                fcode                    = stru_authority
                plvar                    = i1001_ext-plvar
                otype                    = i1001_ext-sclas
                objid                    = i1001_ext-sobid
                with_base_ac             = base_authority
              EXCEPTIONS
                no_stru_authority        = 1
                no_stru_authority_hyper  = 2
                no_stru_authority_at_all = 3
                no_base_authority        = 4
                OTHERS                   = 5.
            IF sy-subrc >< 0.
              DELETE i1001_ext.
              CONTINUE.
            ENDIF.
          ENDIF.
*         YMOUNI - start
*         pad25 = i1001_ext-adata.
          CALL METHOD cl_hr_adata_type_cast=>adata_to_padnn
            EXPORTING
              adata = i1001_ext-adata
            IMPORTING
              padnn = pad25.
*         YMOUNI - end
          partic_training_tab-plvar = participant_tab-plvar.
          partic_training_tab-patyp = participant_tab-patyp.
          partic_training_tab-parid = participant_tab-parid.
          partic_training_tab-parsh = participant_tab-parsh.
          partic_training_tab-partx = participant_tab-partx.
          partic_training_tab-evtyp = i1001_ext-sclas.
          partic_training_tab-eveid = i1001_ext-sobid.
          partic_training_tab-manzl = pad25-manzl.
          partic_training_tab-priox = i1001_ext-priox.
          partic_training_tab-seqnr = i1001_ext-seqnr.
          partic_training_tab-budat = pad25-budat.
          APPEND partic_training_tab.
          event_tab-plvar = participant_tab-plvar.
          event_tab-otype = i1001_ext-sclas.
          event_tab-objid = i1001_ext-sobid.
          COLLECT event_tab.
          MOVE-CORRESPONDING pad25 TO h_kont_tab.
          h_kont_tab-plvar = i1001_ext-plvar.
          h_kont_tab-patyp = i1001_ext-otype.
          h_kont_tab-parid = i1001_ext-objid.
          h_kont_tab-evtyp = i1001_ext-sclas.
          h_kont_tab-eveid = i1001_ext-sobid.
          h_kont_tab-evbgd = i1001_ext-begda.
          h_kont_tab-evend = i1001_ext-endda.
          h_kont_tab-priox = i1001_ext-priox.
          h_kont_tab-seqnr = i1001_ext-seqnr.
          APPEND h_kont_tab.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ELSE.                                "interne Teilnehmertypen
    CALL FUNCTION 'RH_READ_INFTY_1001'
      EXPORTING
        authority      = 'DISP'
        with_stru_auth = 'X'
        istat          = pchistat
        extend         = 'X'
        subty          = $partb
        begda          = pchbegda
        endda          = pchendda
      TABLES
        i1001          = i1001
        objects        = int_objects_tab
      EXCEPTIONS
        nothing_found  = 1
        OTHERS         = 3.
    IF sy-subrc <> 1.
      LOOP AT participant_tab.
        LOOP AT i1001 WHERE plvar = participant_tab-plvar AND
                            otype = participant_tab-patyp AND
                            objid = participant_tab-parid.
          IF NOT rhthist0_auth_sobid IS INITIAL.
            CALL FUNCTION 'RH_STRU_AUTHORITY_CHECK'
              EXPORTING
                fcode                    = stru_authority
                plvar                    = i1001-plvar
                otype                    = i1001-sclas
                objid                    = i1001-sobid
                with_base_ac             = base_authority
              EXCEPTIONS
                no_stru_authority        = 1
                no_stru_authority_hyper  = 2
                no_stru_authority_at_all = 3
                no_base_authority        = 4
                OTHERS                   = 5.
            IF sy-subrc >< 0.
              DELETE i1001.
              CONTINUE.
            ENDIF.
          ENDIF.
*         YMOUNI - start
*         pad25 = i1001-adata.
          CALL METHOD cl_hr_adata_type_cast=>adata_to_padnn
            EXPORTING
              adata = i1001-adata
            IMPORTING
              padnn = pad25.
*         YMOUNI - end
          partic_training_tab-plvar = participant_tab-plvar.
          partic_training_tab-patyp = participant_tab-patyp.
          partic_training_tab-parid = participant_tab-parid.
          partic_training_tab-parsh = participant_tab-parsh.
          partic_training_tab-partx = participant_tab-partx.
          partic_training_tab-evtyp = i1001-sclas.
          partic_training_tab-eveid = i1001-sobid.
          partic_training_tab-manzl = pad25-manzl.
          partic_training_tab-priox = i1001-priox.
          partic_training_tab-seqnr = i1001-seqnr.
          partic_training_tab-budat = pad25-budat.
          APPEND partic_training_tab.
          event_tab-plvar = participant_tab-plvar.
          event_tab-otype = i1001-sclas.
          event_tab-objid = i1001-sobid.
          COLLECT event_tab.
          MOVE-CORRESPONDING pad25 TO h_kont_tab.
          h_kont_tab-plvar = i1001-plvar.
          h_kont_tab-patyp = i1001-otype.
          h_kont_tab-parid = i1001-objid.
          h_kont_tab-evtyp = i1001-sclas.
          h_kont_tab-eveid = i1001-sobid.
          h_kont_tab-evbgd = i1001-begda.
          h_kont_tab-evend = i1001-endda.
          h_kont_tab-priox = i1001-priox.
          h_kont_tab-seqnr = i1001-seqnr.
          APPEND h_kont_tab.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDIF.

* Prüfung: Raumbelegung
  CLEAR i1031. REFRESH i1031.
  CALL FUNCTION 'RH_READ_INFTY_NNNN'
    EXPORTING
*     AUTHORITY       = 'DISP'
*     WITH_STRU_AUTH  = 'X'
      infty           = '1031'
*     ISTAT           = ' '
*     EXTEND          = 'X'
*     SUBTY           = ' '
      begda           = pchbegda
      endda           = pchendda
*     CONDITION       = '00000'
*     INFTB           = '1'
*     SORT            = 'X'
    TABLES
      innnn           = i1031
      objects         = event_tab
    EXCEPTIONS
      nothing_found   = 1
      wrong_condition = 2
      OTHERS          = 3.
  IF sy-subrc EQ 0.
    LOOP AT i1031.
      DELETE: partic_training_tab WHERE eveid EQ i1031-objid,
              event_tab WHERE objid EQ i1031-objid,
              h_kont_tab WHERE eveid EQ i1031-objid.
    ENDLOOP.
  ENDIF.

* Texte und Gültigkeit zu Veranstaltungen lesen
  IF NOT event_tab IS INITIAL.
    CALL FUNCTION 'RH_TEXT_BUFFER_FILL'
      EXPORTING
        check_stru_auth = 'X'
        with_extint     = 'X'
      TABLES
        objects         = event_tab.
    LOOP AT event_tab.
      CLEAR: h_begda, h_endda, h_short, h_stext.
      CALL FUNCTION 'RH_READ_OBJECT'
        EXPORTING
          plvar           = event_tab-plvar
          otype           = event_tab-otype
          objid           = event_tab-objid
          istat           = pc-istat
          begda           = pc-begda
          endda           = pc-endda
          langu           = sy-langu
          ointerval       = 'X'
          store           = 'X'
          check_stru_auth = 'X'
        IMPORTING
          obeg            = h_begda
          oend            = h_endda
          short           = h_short
          stext           = h_stext
        EXCEPTIONS
          not_found       = 01.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      LOOP AT partic_training_tab WHERE plvar = event_tab-plvar AND
                                        evtyp = event_tab-otype AND
                                        eveid = event_tab-objid.
        tabix = sy-tabix.
        partic_training_tab-evsht = h_short.
        IF NOT h_stext IS INITIAL.
          partic_training_tab-evtxt = h_stext.
        ELSE.
          partic_training_tab-evtxt = h_short.
        ENDIF.
        partic_training_tab-evbeg = h_begda.
        partic_training_tab-evend = h_endda.
        MODIFY partic_training_tab INDEX tabix.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
* Preise
  LOOP AT h_kont_tab.
    CLEAR: kont_tab, kostl_tab.
    REFRESH: kont_tab, kostl_tab.
    MOVE-CORRESPONDING h_kont_tab TO kont_tab.
    APPEND kont_tab.
    CALL FUNCTION 'RH_GET_PRICE_DATA'
      EXPORTING
        with_price     = 'X'
      TABLES
        kont_tab_in    = kont_tab
        kont_tab_out   = kostl_tab
      EXCEPTIONS
        nothing_found  = 1
        no_price_found = 2
        OTHERS         = 3.
    IF NOT sy-subrc BETWEEN 1 AND 2.
      READ TABLE partic_training_tab WITH KEY plvar = h_kont_tab-plvar
                                                     evtyp = $kurs
                                              eveid = h_kont_tab-eveid
                                              patyp = h_kont_tab-patyp
                                              parid = h_kont_tab-parid
                                              priox = h_kont_tab-priox
                                              seqnr = h_kont_tab-seqnr
                                              manzl = h_kont_tab-manzl
                                              budat = h_kont_tab-budat.
      tabix = sy-tabix.
      IF sy-subrc = 0.
        partic_training_tab-kkost = kostl_tab-kkost.
        partic_training_tab-kwaer = kostl_tab-kwaer.
        MODIFY partic_training_tab INDEX tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.
* Veranstaltungsdauer
  CLEAR: schedule_tab, schedule_tab[].
  CALL FUNCTION 'RH_READ_EVENT_SCHEDULE'
    TABLES
      eventlist     = event_tab
      schedule_tab  = schedule_tab
    EXCEPTIONS
      nothing_found = 1
      OTHERS        = 2.
  IF sy-subrc = 0. ENDIF.
  LOOP AT event_tab.
    CLEAR : abl_days, abl_hour.
    LOOP AT schedule_tab WHERE plvar = event_tab-plvar AND
                               otype = event_tab-otype AND
                               objid = event_tab-objid.
      abl_days = schedule_tab-ndays.
      abl_hour = schedule_tab-nhours.
      EXIT.
    ENDLOOP.
    LOOP AT partic_training_tab WHERE plvar = event_tab-plvar AND
                                      evtyp = event_tab-otype AND
                                      eveid = event_tab-objid.
      partic_training_tab-evday  = abl_days.
      partic_training_tab-evhour = abl_hour.
      MODIFY partic_training_tab.
    ENDLOOP.
  ENDLOOP.

  PERFORM fill_table_for_douple_check TABLES partic_training_tab
                                             event_tab.

ENDFORM.                               " READ_1001_B025

*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
FORM alv_display.

*--> Begin of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021
  DATA: ls_p0001 TYPE p0001,
        ls_t001p TYPE t001p.

  DATA : rai_objects LIKE hrobject OCCURS 0 WITH HEADER LINE.
  DATA : rai_wplog LIKE wplog OCCURS 0 WITH HEADER LINE.
  DATA: ls_hrvpva TYPE hrvpva.
  DATA : rai_1001 LIKE p1001 OCCURS 0 WITH HEADER LINE.
*<-- End of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021

  g_repid = sy-repid.

  SORT partic_training_tab.
  REFRESH alv_itab.
  LOOP AT partic_training_tab.
    CLEAR alv_itab.
    alv_itab-patyp = partic_training_tab-patyp.
    alv_itab-parid = partic_training_tab-parid.
    alv_itab-parsh = partic_training_tab-parsh.
    alv_itab-partx = partic_training_tab-partx.
    IF partic_training_tab-evtyp EQ $kurs.
      alv_itab-kurs = 'X'.
      alv_itab-kurst = ' '.
    ELSE.
      alv_itab-kurs = ' '.
      alv_itab-kurst = 'X'.
    ENDIF.
    alv_itab-evtyp = partic_training_tab-evtyp.
    alv_itab-eveid = partic_training_tab-eveid.
    alv_itab-evsht = partic_training_tab-evsht.
    alv_itab-evtxt = partic_training_tab-evtxt.
    alv_itab-evbeg = partic_training_tab-evbeg.
    alv_itab-evend = partic_training_tab-evend.
    alv_itab-ndays = partic_training_tab-evday.
    alv_itab-nhours = partic_training_tab-evhour.
    alv_itab-kkost = partic_training_tab-kkost.
    alv_itab-kwaer = partic_training_tab-kwaer.
    alv_itab-manzl = partic_training_tab-manzl.
    alv_itab-priox = partic_training_tab-priox.
*--> Begin of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021
    READ TABLE gt_p0001 INTO ls_p0001 WITH KEY pernr = partic_training_tab-parid.
    IF sy-subrc EQ 0.
      alv_itab-btrtl = ls_p0001-btrtl.
      READ TABLE gt_t001p INTO ls_t001p WITH KEY werks = ls_p0001-werks
      btrtl = ls_p0001-btrtl.
      IF sy-subrc EQ 0.
        alv_itab-btext = ls_t001p-btext.
      ENDIF.

    ENDIF.

    REFRESH: rai_objects[], rai_wplog[].

    rai_objects-plvar = partic_training_tab-plvar.
    rai_objects-otype = partic_training_tab-evtyp.
    rai_objects-objid = partic_training_tab-eveid.
    APPEND rai_objects.

    CALL FUNCTION 'RH_READ_INFTY'
      EXPORTING
        authority            = '    '
        with_stru_auth       = ' '
        infty                = ' '
        istat                = '2'
        subty                = ' '
        begda                = pchbegda
        endda                = pchendda
        inftb                = '0'
      TABLES
        innnn                = rai_wplog
        objects              = rai_objects
      EXCEPTIONS
        all_infty_with_subty = 1
        nothing_found        = 2
        no_objects           = 3
        wrong_condition      = 4
        OTHERS               = 5.
    IF sy-subrc EQ 0.
      LOOP AT rai_wplog.
        CASE rai_wplog-infty.
          WHEN '1001'.

            CALL METHOD cl_hr_pnnnn_type_cast=>wplog_to_pnnnn "YSBUNI
              EXPORTING
                wplog = rai_wplog
              IMPORTING
                pnnnn = rai_1001.

            CASE rai_wplog-subty.
              WHEN 'A024'.

                alv_itab-evloc = rai_1001-sobid.
                ls_hrvpva-locid = rai_1001-sobid.

                CALL FUNCTION 'RH_READ_OBJECT'
                  EXPORTING
                    begda           = pchbegda
                    endda           = pchendda
                    istat           = ' '
                    langu           = sy-langu
                    objid           = ls_hrvpva-locid
                    otype           = 'F'
                    plvar           = rai_objects-plvar
                    check_stru_auth = ' '
                  IMPORTING
                    obeg            = ls_hrvpva-locbeg
                    oend            = ls_hrvpva-locend
                    ostat           = ls_hrvpva-locstat
                    stext           = ls_hrvpva-loctx
                  EXCEPTIONS
                    not_found       = 01
                    OTHERS          = 02.
                IF ls_hrvpva-loctx IS NOT INITIAL.
                  alv_itab-loctx = ls_hrvpva-loctx.

                ENDIF.
              WHEN OTHERS.
            ENDCASE.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.

    ENDIF.
*<-- End of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021
    APPEND alv_itab.
  ENDLOOP.
  SORT alv_itab BY parsh partx parid
                   evbeg DESCENDING
                   evend kurs kurst evsht evtxt.
  PERFORM display_alv TABLES alv_itab
                      USING  ' '.

ENDFORM.                               " ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  INIT_FIELDCAT                                            *
*&---------------------------------------------------------------------*
FORM init_fieldcat.
*--> Begin of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021
*  DATA: lv_prog LIKE sy-repid VALUE 'RHTHIST0'.
  DATA: lv_prog LIKE sy-repid VALUE 'ZHR_RHTHIST0'.
*<-- End of mod. - CN - 5000033306 / 	7000129070 - 04.11.2021
  DATA: lt_tab  LIKE textpool OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF ls_key,
          id  LIKE textpool-id,
          key LIKE textpool-key,
        END OF ls_key.

  CLEAR lt_tab. REFRESH lt_tab.
  ls_key-id = 'I'.
  READ TEXTPOOL lv_prog INTO lt_tab LANGUAGE sy-langu.
  LOOP AT gt_fieldcat.
    CASE gt_fieldcat-fieldname.
      WHEN 'PATYP'.
        gt_fieldcat-no_out = yes.
      WHEN 'PARID'.
        gt_fieldcat-no_out = yes.
*      WHEN 'PARSH'.
      WHEN 'PARTX'.
        gt_fieldcat-ddictxt = 'M'.
      WHEN 'KURS'.
        gt_fieldcat-checkbox = yes.
      WHEN 'KURST'.
        gt_fieldcat-checkbox = yes.
      WHEN 'EVTYP'.
        gt_fieldcat-no_out = yes.
        ls_key-key = '015'.
        READ TABLE lt_tab WITH KEY
             id  = ls_key-id
             key = ls_key-key.
        IF sy-subrc = 0.
          gt_fieldcat-seltext_l = lt_tab-entry.
        ENDIF.
      WHEN 'EVEID'.
        gt_fieldcat-no_out = yes.
        ls_key-key = '016'.
        READ TABLE lt_tab WITH KEY
             id  = ls_key-id
             key = ls_key-key.
        IF sy-subrc = 0.
          gt_fieldcat-seltext_l = lt_tab-entry.
        ENDIF.
      WHEN 'EVSHT'.
        ls_key-key = '017'.
        READ TABLE lt_tab WITH KEY
             id  = ls_key-id
             key = ls_key-key.
        IF sy-subrc = 0.
          gt_fieldcat-seltext_l = lt_tab-entry.
        ENDIF.
      WHEN 'EVTXT'.
        gt_fieldcat-no_out = yes.
      WHEN 'EVBEG'.
        gt_fieldcat-ddictxt = 'M'.
      WHEN 'EVEND'.
        gt_fieldcat-ddictxt = 'M'.
      WHEN 'NDAYS'.
        gt_fieldcat-do_sum = yes.
      WHEN 'NHOURS'.
        gt_fieldcat-do_sum = yes.
      WHEN 'KKOST'.
        gt_fieldcat-do_sum = yes.
*      WHEN 'KWAER'.
      WHEN 'MANZL'.
        gt_fieldcat-do_sum = yes.
    ENDCASE.
    MODIFY gt_fieldcat.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TOPOFPAGE
*&---------------------------------------------------------------------*
FORM topofpage.

  DATA: date1(10) TYPE c,
        date2(10) TYPE c.

*-header-----------------*
* Typ H: Heading
* Typ S: Body
  REFRESH alv_header.
  alv_header-typ  = 'H'.
  alv_header-info = sy-title.
  APPEND alv_header.

  alv_header-typ  = 'S'.
  alv_header-key  = TEXT-tim.          "'Auswahlzeitraum'.
  WRITE pchbegda DD/MM/YYYY TO date1.
  WRITE pchendda DD/MM/YYYY TO date2.
  CONCATENATE date1 '-' date2 INTO alv_header-info SEPARATED BY space.
  APPEND alv_header.
* Aufruf
  PERFORM alv_header.
*-header-----------------*


ENDFORM.                               "TOPOFPAGE

*&---------------------------------------------------------------------*
*&      Form  cua_exit
*&---------------------------------------------------------------------*
*       set cua for alv grid
*----------------------------------------------------------------------*
FORM cua_exit USING ce_func_exclude TYPE slis_t_extab.

  DATA fcode_attrib_tab LIKE smp_dyntxt OCCURS 4 WITH HEADER LINE.

  CLEAR: fcode_attrib_tab, fcode_attrib_tab[].
  PERFORM dynamic_report_fcodes(rhteiln0) TABLES  fcode_attrib_tab
                                          USING   ce_func_exclude
                                                  no no.

  SET PF-STATUS 'ALVLIST' EXCLUDING ce_func_exclude
                OF PROGRAM 'RHTEILN0'.

ENDFORM.                               " CUA_EXIT

*&---------------------------------------------------------------------*
*&      Form  ucomm_exit
*&---------------------------------------------------------------------*
*       handle alv user-commands
*----------------------------------------------------------------------*
FORM ucomm_exit USING ue_ucomm    LIKE sy-ucomm
                      ue_selfield TYPE slis_selfield.
ENDFORM.                               " UCOMM_EXIT


*&---------------------------------------------------------------------*
*&      Form  FILL_TABLE_FOR_DOUPLE_CHECK
*&---------------------------------------------------------------------*
*      -->TRAINING_TAB
*      -->EVENT_TAB
*----------------------------------------------------------------------*
FORM fill_table_for_douple_check
                  TABLES training_tab STRUCTURE partic_training_tab
                         event_tab    STRUCTURE hrobject.

  DATA: local_1001 LIKE i1001 OCCURS 0 WITH HEADER LINE.

  CLEAR: de_check_tab, de_check_tab[].

  CALL FUNCTION 'RH_READ_INFTY_1001'
    EXPORTING
      authority      = '    '
      with_stru_auth = ' '
*     ISTAT          = ' '
*     EXTEND         = 'X'
      subty          = $speca
      begda          = pchbegda
      endda          = pchendda
*     CONDITION      = '00000'
*     SORT           = 'X'
*     WITH_EV        = ' '
      adata          = ' '
*     AUTH_SOBID     = ' '
    TABLES
      i1001          = local_1001
      objects        = event_tab
    EXCEPTIONS
      OTHERS         = 0.
  SORT local_1001 BY objid.
  LOOP AT training_tab.
    READ TABLE local_1001 WITH KEY objid = training_tab-eveid
                          BINARY SEARCH.
    IF sy-subrc = 0.
      CLEAR de_check_tab.
      de_check_tab-patyp = training_tab-patyp.
      de_check_tab-parid = training_tab-parid.
      de_check_tab-evtid = local_1001-sobid.
      de_check_tab-evbeg = training_tab-evbeg.
      de_check_tab-evend = training_tab-evend.
      APPEND de_check_tab.
    ENDIF.
  ENDLOOP.
  SORT de_check_tab.

ENDFORM.                               " FILL_TABLE_FOR_DOUPLE_CHECK

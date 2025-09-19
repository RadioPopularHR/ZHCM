*&---------------------------------------------------------------------*
*& Report  ZRH_UPDATE_CLUSTER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrh_update_cluster.

TABLES: pernr, t512w, pcl1, pcl2, pc207.

INCLUDE rpcvcd09.

DATA:  BEGIN OF bt OCCURS 3.
         INCLUDE STRUCTURE pc209.
       DATA:  END   OF bt.

       INCLUDE rpc2rx02.
       INCLUDE rpc2rx29.
       INCLUDE rpc2rpp0.
       INCLUDE hptpaymacro.
       INCLUDE rpcdtd00.
       INCLUDE rpcdtd10.


       INCLUDE up50qdat.
       INCLUDE rpc2cd09.
       INCLUDE rpppxd00.                      "Daten PCLx-Puffer
       DATA: BEGIN OF COMMON PART buffer.
               INCLUDE rpppxd10.                      "Daten PCLx-Puffer
             DATA: END OF COMMON PART.
             INCLUDE rpppxm00.
             INCLUDE rpppxi19.

             CONSTANTS: co_moeda(3) VALUE 'EUR'.

DATA: payty LIKE pc261-payty,
      payid LIKE pc261-payid,
      bondt LIKE pc261-bondt.
DATA: sw_retro.
DATA: sy_repid  LIKE sy-repid,
      format    LIKE t522f-formt,
      edit_name LIKE p0001-ename,
      ret_code  LIKE sy-subrc.

TYPES: BEGIN OF ty_upload,
         pernr TYPE pernr-pernr,
         lgart TYPE p0014-lgart,
         apaga TYPE char1,
         abart TYPE abrar,
         apznr TYPE apznr,
         v0typ TYPE v0typ,
         v0znr TYPE v0znr,
         anzhl TYPE pranz,
         betrg TYPE maxbt,
         betpe TYPE betpe,
       END OF ty_upload.

TYPES: BEGIN OF ty_up_va,
         pernr     TYPE pernr-pernr,
         vagrp     TYPE pc2pf-vagrp,
         vagrpx,
         allty     TYPE pc2pf-allty,
         alltyx,
         pamnt     TYPE pc2pf-pamnt,
         pamntx,
         pdays     TYPE pc2pf-pdays,
         pdaysx,
         estam     TYPE pc2pf-estam,
         estamx,
         regty     TYPE pc2pf-regty,
         regtyx,
         termx     TYPE pc2pf-termx,
         termxx,
         fixco     TYPE pc2pf-fixco,
         fixcox,
         varco     TYPE pc2pf-varco,
         varcox,
         cpamt     TYPE pc2pf-cpamt,
         cpamtx,
         cpday     TYPE pc2pf-cpday,
         cpdayx,
         eamnt     TYPE pc2pf-eamnt,
         eamntx,
         ceamt     TYPE pc2pf-ceamt,
         ceamtx,
         vdays     TYPE pc2pf-vdays,
         vdaysx,
         cvday     TYPE pc2pf-cvday,
         cvdayx,
         nquot     TYPE pc2pf-nquot,
         nquotx,
         nlday     TYPE pc2pf-nlday,
         nldayx,
         tquot     TYPE pc2pf-tquot,
         tquotx,
         tlday     TYPE pc2pf-tlday,
         tldayx,
         tunit     TYPE pc2pf-tunit,
         tunitx,
         payfl     TYPE pc2pf-payfl,
         payflx,
         cpayf     TYPE pc2pf-cpayf,
         cpayfx,
         adjfl     TYPE pc2pf-adjfl,
         adjflx,
         terfl     TYPE pc2pf-terfl,
         terflx,
         awart     TYPE pc2pf-awart,
         awartx,
         obegd     TYPE pc2pf-obegd,
         obegdx,
         oendd     TYPE pc2pf-oendd,
         oenddx,
         gwtyp     TYPE pc2pf-gwtyp,
         gwtypx,
         gapzn     TYPE pc2pf-gapzn,
         gapznx,
         glday     TYPE pc2pf-glday,
         gldayx,
         gzein     TYPE pc2pf-gzein,
         gzeinx,
         amt_curr  TYPE pc2pf-amt_curr,
         amt_currx,
         clamt     TYPE pc2pf-clamt,
         clamtx,
         clday     TYPE pc2pf-clday,
         cldayx,
         cdurp     TYPE pc2pf-cdurp,
         cdurpx,
         ediff     TYPE pc2pf-ediff,
         ediffx,
         gener     TYPE pc2pf-gener,
         generx,
       END OF ty_up_va.

DATA: gv_cluster(10).
DATA: gt_upload TYPE TABLE OF ty_upload,
      gs_upload LIKE LINE OF gt_upload.
DATA: gt_up_va TYPE TABLE OF ty_up_va,
      gs_up_va LIKE LINE OF gt_up_va.


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
PARAMETERS: p_lgart TYPE t512w-lgart NO-DISPLAY,
            p_betrg TYPE pc207-betrg NO-DISPLAY,
            p_anzhl TYPE pc207-anzhl NO-DISPLAY,
            p_fpper TYPE faper OBLIGATORY.
PARAMETERS: p_file  TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
PARAMETERS: p_rt  RADIOBUTTON GROUP g1 DEFAULT 'X',
            p_crt RADIOBUTTON GROUP g1,
            p_bt  RADIOBUTTON GROUP g1,
            p_va  RADIOBUTTON GROUP g1.
SELECTION-SCREEN: END OF BLOCK b2.
SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-t03.
PARAMETERS: p_teste AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK b3.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'P_FILE'
    IMPORTING
      file_name  = p_file.



INITIALIZATION.

  def_rgdir evp_rgdir.

START-OF-SELECTION.

  sy_repid = sy-repid.
  IF p_rt      IS NOT INITIAL.
    gv_cluster = 'RT'.
  ELSEIF p_crt IS NOT INITIAL.
    gv_cluster = 'CRT e SCRT'.
  ELSEIF p_bt  IS NOT INITIAL.
    gv_cluster = 'BT'.
  ELSEIF p_va  IS NOT INITIAL.
    gv_cluster = 'VA'.
  ENDIF.

  PERFORM get_file_data.

  CHECK pnppernr[] IS NOT INITIAL.

GET pernr.

  PERFORM upd_data.



*&---------------------------------------------------------------------*
*&      Form  UPD_DATA
*&---------------------------------------------------------------------*
FORM upd_data.

  DATA: ls_rt    TYPE pc207,
        ls_va    TYPE pc2pf,
        ls_crt   TYPE pc208,
        ls_scrt  TYPE pc2pd,
        lv_index TYPE sy-tabix.

  FIELD-SYMBOLS: <fs_rt>   TYPE pc207,
                 <fs_va>   TYPE pc2pf,
                 <fs_bt>   TYPE pc209,
                 <fs_crt>  TYPE pc208,
                 <fs_scrt> TYPE pc2pd.

  rp-init-buffer.
  CLEAR evp_rgdir.
  REFRESH evp_rgdir.
  cd-key-pernr = pernr-pernr.
  PERFORM fill_import_export_key USING sy_repid
                                       pernr-pernr.
  rp-imp-c2-cu.
  CALL FUNCTION 'CD_CHECK_PAYROLL_RESULT'
    EXPORTING
      bonus_date      = bondt
      inper           = pn-paper
      inper_modif     = pn-permo
      pay_ident       = payid
      pay_type        = payty
    TABLES
      rgdir           = rgdir
    EXCEPTIONS
      no_record_found = 01.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'CD_EVALUATION_PERIODS'
      EXPORTING
        bonus_date      = bondt
        inper           = pn-paper
        inper_modif     = pn-permo
        pay_ident       = payid
        pay_type        = payty
      TABLES
        evpdir          = evp_rgdir
        rgdir           = rgdir
        iabkrs          = pnpabkrs
      EXCEPTIONS
        no_record_found = 01.
    IF sy-subrc EQ 0.
      LOOP AT evp_rgdir WHERE inper EQ pn-paper
                        AND   fpper EQ p_fpper.
        rx-key-pernr = pernr-pernr.
        rx-key-seqno = evp_rgdir-seqnr.
        seqno        = evp_rgdir-seqnr.

        PERFORM rmac_imp.

        LOOP AT gt_upload INTO gs_upload
                          WHERE pernr EQ pernr-pernr.
          IF p_rt IS NOT INITIAL.
            LOOP AT rt ASSIGNING <fs_rt> WHERE lgart EQ gs_upload-lgart
                                           AND abart EQ gs_upload-abart  " INS 14.11.2023
                                           AND v0typ EQ gs_upload-v0typ
                                           AND v0znr EQ gs_upload-v0znr.
              lv_index = sy-tabix.
              IF gs_upload-apaga IS INITIAL.
                IF gs_upload-abart IS NOT INITIAL.
                  <fs_rt>-abart = gs_upload-abart.
                ENDIF.
                IF gs_upload-apznr IS NOT INITIAL.
                  <fs_rt>-apznr = gs_upload-apznr.
                ENDIF.
                IF gs_upload-v0typ IS NOT INITIAL.
                  <fs_rt>-v0typ = gs_upload-v0typ.
                ENDIF.
                IF gs_upload-v0znr IS NOT INITIAL.
                  <fs_rt>-v0znr = gs_upload-v0znr.
                ENDIF.
                IF gs_upload-anzhl IS NOT INITIAL.
                  <fs_rt>-anzhl = gs_upload-anzhl.
                ENDIF.
                IF gs_upload-betrg IS NOT INITIAL.
                  <fs_rt>-betrg = gs_upload-betrg.
                ENDIF.
                IF gs_upload-betpe IS NOT INITIAL.
                  <fs_rt>-betpe = gs_upload-betpe.
                ENDIF.
              ELSE.
                DELETE rt INDEX lv_index.
              ENDIF.
            ENDLOOP.
            IF sy-subrc NE 0
           AND gs_upload-apaga IS INITIAL.  " INS 14.11.2023
              CLEAR: ls_rt.
              ls_rt-lgart = gs_upload-lgart.
              ls_rt-abart = gs_upload-abart.
              ls_rt-apznr = gs_upload-apznr.
              ls_rt-v0typ = gs_upload-v0typ.
              ls_rt-v0znr = gs_upload-v0znr.
              ls_rt-anzhl = gs_upload-anzhl.
              ls_rt-betrg = gs_upload-betrg.
              ls_rt-betpe = gs_upload-betpe.
              COLLECT ls_rt INTO rt.
              SORT rt.
            ENDIF.

          ELSEIF p_crt IS NOT INITIAL.
            LOOP AT crt ASSIGNING <fs_crt> WHERE lgart EQ gs_upload-lgart.
              lv_index = sy-tabix.
              IF gs_upload-apaga IS INITIAL.
                IF gs_upload-abart IS NOT INITIAL.
                  <fs_crt>-cumty = gs_upload-abart.
                ENDIF.
                IF gs_upload-anzhl IS NOT INITIAL.
                  <fs_crt>-anzhl = gs_upload-anzhl.
                ENDIF.
                IF gs_upload-betrg IS NOT INITIAL.
                  <fs_crt>-betrg = gs_upload-betrg.
                ENDIF.
              ELSE.
                DELETE crt INDEX lv_index.
              ENDIF.
            ENDLOOP.
            IF sy-subrc NE 0.
              CLEAR: ls_crt.
              ls_crt-cumty    = gs_upload-abart.
              ls_crt-lgart    = gs_upload-lgart.
              ls_crt-anzhl    = gs_upload-anzhl.
              ls_crt-betrg    = gs_upload-betrg.
              IF ls_crt-betrg IS NOT INITIAL.
                ls_crt-amt_curr = co_moeda.
              ENDIF.
              COLLECT ls_crt INTO crt.
            ENDIF.

            LOOP AT scrt ASSIGNING <fs_scrt> WHERE lgart EQ gs_upload-lgart.
              lv_index = sy-tabix.
              IF gs_upload-apaga IS INITIAL.
                IF gs_upload-abart IS NOT INITIAL.
                  <fs_scrt>-cumty = gs_upload-abart.
                ENDIF.
                IF gs_upload-anzhl IS NOT INITIAL.
                  <fs_scrt>-anzhl = gs_upload-anzhl.
                ENDIF.
                IF gs_upload-betrg IS NOT INITIAL.
                  <fs_scrt>-betrg = gs_upload-betrg.
                ENDIF.
              ELSE.
                DELETE scrt INDEX lv_index.
              ENDIF.
            ENDLOOP.
            IF sy-subrc NE 0.
              CLEAR: ls_scrt.
              ls_scrt-cumty    = gs_upload-abart.
              ls_scrt-lgart    = gs_upload-lgart.
              ls_scrt-anzhl    = gs_upload-anzhl.
              ls_scrt-betrg    = gs_upload-betrg.
              IF ls_scrt-betrg IS NOT INITIAL.
                ls_scrt-amt_curr = co_moeda.
              ENDIF.
              COLLECT ls_scrt INTO scrt.
            ENDIF.

          ELSEIF p_bt IS NOT INITIAL.
            LOOP AT bt ASSIGNING <fs_bt> WHERE lgart EQ gs_upload-lgart.
              IF gs_upload-betrg IS NOT INITIAL.
                <fs_bt>-betrg = gs_upload-betrg.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
        IF sy-subrc EQ 0.
          IF p_teste IS INITIAL.
            PERFORM rmac_exp.
            IF sy-subrc EQ 0.
              WRITE: / gv_cluster, 'atualizada para o empregado nº',
                       pernr-pernr, evp_rgdir-fpper, evp_rgdir-inper.
            ELSE.
              WRITE: / gv_cluster, 'não foi atualizada para o empregado nº',
                       pernr-pernr, evp_rgdir-fpper, evp_rgdir-inper COLOR COL_NEGATIVE.
            ENDIF.
          ELSE.
            WRITE: / gv_cluster, 'será atualizada para o empregado nº',
                     pernr-pernr, evp_rgdir-fpper, evp_rgdir-inper.
          ENDIF.
        ENDIF.

        LOOP AT gt_up_va INTO gs_up_va
                         WHERE pernr EQ pernr-pernr.
          IF p_va IS NOT INITIAL.
            READ TABLE va ASSIGNING <fs_va> INDEX 1.
            IF sy-subrc EQ 0.
              IF gs_up_va-vagrpx IS NOT INITIAL.
                <fs_va>-vagrp = gs_up_va-vagrp.
              ENDIF.
              IF gs_up_va-alltyx IS NOT INITIAL.
                <fs_va>-allty = gs_up_va-allty.
              ENDIF.
              IF gs_up_va-pamntx IS NOT INITIAL.
                <fs_va>-pamnt = gs_up_va-pamnt.
              ENDIF.
              IF gs_up_va-pdaysx IS NOT INITIAL.
                <fs_va>-pdays = gs_up_va-pdays.
              ENDIF.
              IF gs_up_va-estamx IS NOT INITIAL.
                <fs_va>-estam = gs_up_va-estam.
              ENDIF.
              IF gs_up_va-regtyx IS NOT INITIAL.
                <fs_va>-regty = gs_up_va-regty.
              ENDIF.
              IF gs_up_va-termxx IS NOT INITIAL.
                <fs_va>-termx = gs_up_va-termx.
              ENDIF.
              IF gs_up_va-fixcox IS NOT INITIAL.
                <fs_va>-fixco = gs_up_va-fixco.
              ENDIF.
              IF gs_up_va-varcox IS NOT INITIAL.
                <fs_va>-varco = gs_up_va-varco.
              ENDIF.
              IF gs_up_va-cpamtx IS NOT INITIAL.
                <fs_va>-cpamt = gs_up_va-cpamt.
              ENDIF.
              IF gs_up_va-cpdayx IS NOT INITIAL.
                <fs_va>-cpday = gs_up_va-cpday.
              ENDIF.
              IF gs_up_va-eamntx IS NOT INITIAL.
                <fs_va>-eamnt = gs_up_va-eamnt.
              ENDIF.
              IF gs_up_va-ceamtx IS NOT INITIAL.
                <fs_va>-ceamt = gs_up_va-ceamt.
              ENDIF.
              IF gs_up_va-vdaysx IS NOT INITIAL.
                <fs_va>-vdays = gs_up_va-vdays.
              ENDIF.
              IF gs_up_va-cvdayx IS NOT INITIAL.
                <fs_va>-cvday = gs_up_va-cvday.
              ENDIF.
              IF gs_up_va-nquotx IS NOT INITIAL.
                <fs_va>-nquot = gs_up_va-nquot.
              ENDIF.
              IF gs_up_va-nldayx IS NOT INITIAL.
                <fs_va>-nlday = gs_up_va-nlday.
              ENDIF.
              IF gs_up_va-tquotx IS NOT INITIAL.
                <fs_va>-tquot = gs_up_va-tquot.
              ENDIF.
              IF gs_up_va-tldayx IS NOT INITIAL.
                <fs_va>-tlday = gs_up_va-tlday.
              ENDIF.
              IF gs_up_va-tunitx IS NOT INITIAL.
                <fs_va>-tunit = gs_up_va-tunit.
              ENDIF.
              IF gs_up_va-payflx IS NOT INITIAL.
                <fs_va>-payfl = gs_up_va-payfl.
              ENDIF.
              IF gs_up_va-cpayfx IS NOT INITIAL.
                <fs_va>-cpayf = gs_up_va-cpayf.
              ENDIF.
              IF gs_up_va-adjflx IS NOT INITIAL.
                <fs_va>-adjfl = gs_up_va-adjfl.
              ENDIF.
              IF gs_up_va-terflx IS NOT INITIAL.
                <fs_va>-terfl = gs_up_va-terfl.
              ENDIF.
              IF gs_up_va-awartx IS NOT INITIAL.
                <fs_va>-awart = gs_up_va-awart.
              ENDIF.
              IF gs_up_va-obegdx IS NOT INITIAL.
                <fs_va>-obegd = gs_up_va-obegd.
              ENDIF.
              IF gs_up_va-oenddx IS NOT INITIAL.
                <fs_va>-oendd = gs_up_va-oendd.
              ENDIF.
              IF gs_up_va-gwtypx IS NOT INITIAL.
                <fs_va>-gwtyp = gs_up_va-gwtyp.
              ENDIF.
              IF gs_up_va-gapznx IS NOT INITIAL.
                <fs_va>-gapzn = gs_up_va-gapzn.
              ENDIF.
              IF gs_up_va-gldayx IS NOT INITIAL.
                <fs_va>-glday = gs_up_va-glday.
              ENDIF.
              IF gs_up_va-gzeinx IS NOT INITIAL.
                <fs_va>-gzein = gs_up_va-gzein.
              ENDIF.
              IF gs_up_va-amt_currx IS NOT INITIAL.
                <fs_va>-amt_curr = gs_up_va-amt_curr.
              ENDIF.
              IF gs_up_va-clamtx IS NOT INITIAL.
                <fs_va>-clamt = gs_up_va-clamt.
              ENDIF.
              IF gs_up_va-cldayx IS NOT INITIAL.
                <fs_va>-clday = gs_up_va-clday.
              ENDIF.
              IF gs_up_va-cdurpx IS NOT INITIAL.
                <fs_va>-cdurp = gs_up_va-cdurp.
              ENDIF.
              IF gs_up_va-ediffx IS NOT INITIAL.
                <fs_va>-ediff = gs_up_va-ediff.
              ENDIF.
              IF gs_up_va-generx IS NOT INITIAL.
                <fs_va>-gener = gs_up_va-gener.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF sy-subrc EQ 0.
          IF p_teste IS INITIAL.
            PERFORM rmac_exp.
            IF sy-subrc EQ 0.
              WRITE: / gv_cluster, 'atualizada para o empregado nº',
                       pernr-pernr, evp_rgdir-fpper, evp_rgdir-inper.
            ELSE.
              WRITE: / gv_cluster, 'não foi atualizada para o empregado nº',
                       pernr-pernr, evp_rgdir-fpper, evp_rgdir-inper COLOR COL_NEGATIVE.
            ENDIF.
          ELSE.
            WRITE: / gv_cluster, 'será atualizada para o empregado nº',
                     pernr-pernr, evp_rgdir-fpper, evp_rgdir-inper.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
  PERFORM put-pcl2.

ENDFORM.                               " UPD_DATA

*&---------------------------------------------------------------------*
*&      Form  RMAC_IMP
*&---------------------------------------------------------------------*
FORM rmac_imp.
  rp-imp-c2-rp.
  MOVE orp-version TO rp-version.
ENDFORM.                               " RMAC_IMP
*&---------------------------------------------------------------------*
*&      Form  RMAC_EXP
*&---------------------------------------------------------------------*
FORM rmac_exp.
  rp-exp-c2-rp.
ENDFORM.                               " RMAC_EXP
*&---------------------------------------------------------------------*
*&      Form  PUT-PCL2
*&---------------------------------------------------------------------*
FORM put-pcl2.
  PERFORM prepare_update USING ' '.
  PERFORM update(sapup50r).
ENDFORM.                               " PUT-PCL2
*&---------------------------------------------------------------------*
*&      Form  GET_FILE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_file_data .

  DATA: lv_bcol TYPE i VALUE 1,          "Coluna inicial
        lv_brow TYPE i VALUE 2,          "Linha inicial
        lv_ecol TYPE i VALUE 38,         "Coluna final
        lv_erow TYPE i VALUE 100000.     "Linha final

  DATA: lt_dados TYPE TABLE OF alsmex_tabline,
        ls_dados LIKE LINE OF lt_dados.


  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = lv_bcol
      i_begin_row             = lv_brow
      i_end_col               = lv_ecol
      i_end_row               = lv_erow
    TABLES
      intern                  = lt_dados
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE e000(zhr) WITH 'Erro na abertura do ficheiro.'.
  ENDIF.

  IF p_va IS NOT INITIAL.
    REFRESH: gt_up_va.
    CLEAR: gs_up_va.
    LOOP AT lt_dados INTO ls_dados.
      CASE ls_dados-col.
        WHEN 1.
          gs_up_va-pernr = ls_dados-value.

          CLEAR: pnppernr.
          pnppernr-sign   = 'I'.
          pnppernr-option = 'EQ'.
          pnppernr-low    = gs_up_va-pernr.
          COLLECT pnppernr.
        WHEN 2.
          gs_up_va-vagrp  = ls_dados-value.
          gs_up_va-vagrpx = 'X'.
        WHEN 3.
          gs_up_va-allty  = ls_dados-value.
          gs_up_va-alltyx = 'X'.
        WHEN 4.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-pamnt.
          gs_up_va-pamntx = 'X'.
        WHEN 5.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-pdays.
          gs_up_va-pdaysx = 'X'.
        WHEN 6.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-estam.
          gs_up_va-estamx = 'X'.
        WHEN 7.
          gs_up_va-regty  = ls_dados-value.
          gs_up_va-regtyx = 'X'.
        WHEN 8.
          gs_up_va-termx  = ls_dados-value.
          gs_up_va-termxx = 'X'.
        WHEN 9.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-fixco.
          gs_up_va-fixcox = 'X'.
        WHEN 10.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-varco.
          gs_up_va-varcox = 'X'.
        WHEN 11.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-cpamt.
          gs_up_va-cpamtx = 'X'.
        WHEN 12.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-cpday.
          gs_up_va-cpdayx = 'X'.
        WHEN 13.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-eamnt.
          gs_up_va-eamntx = 'X'.
        WHEN 14.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-ceamt.
          gs_up_va-ceamtx = 'X'.
        WHEN 15.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-vdays.
          gs_up_va-vdaysx = 'X'.
        WHEN 16.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-cvday.
          gs_up_va-cvdayx = 'X'.
        WHEN 17.
          gs_up_va-nquot  = ls_dados-value.
          gs_up_va-nquotx = 'X'.
        WHEN 18.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-nlday.
          gs_up_va-nldayx = 'X'.
        WHEN 19.
          gs_up_va-tquot  = ls_dados-value.
          gs_up_va-tquotx = 'X'.
        WHEN 20.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-tlday.
          gs_up_va-tldayx = 'X'.
        WHEN 21.
          gs_up_va-tunit  = ls_dados-value.
          gs_up_va-tunitx = 'X'.
        WHEN 22.
          gs_up_va-payfl  = ls_dados-value.
          gs_up_va-payflx = 'X'.
        WHEN 23.
          gs_up_va-cpayf  = ls_dados-value.
          gs_up_va-cpayfx = 'X'.
        WHEN 24.
          gs_up_va-adjfl  = ls_dados-value.
          gs_up_va-adjflx = 'X'.
        WHEN 25.
          gs_up_va-terfl  = ls_dados-value.
          gs_up_va-terflx = 'X'.
        WHEN 26.
          gs_up_va-awart  = ls_dados-value.
          gs_up_va-awartx = 'X'.
        WHEN 27.
          gs_up_va-obegd+0(4) = ls_dados-value+6(4).
          gs_up_va-obegd+4(2) = ls_dados-value+3(2).
          gs_up_va-obegd+6(2) = ls_dados-value+0(2).
          gs_up_va-obegdx     = 'X'.
        WHEN 28.
          gs_up_va-oendd+0(4) = ls_dados-value+6(4).
          gs_up_va-oendd+4(2) = ls_dados-value+3(2).
          gs_up_va-oendd+6(2) = ls_dados-value+0(2).
          gs_up_va-oenddx     = 'X'.
        WHEN 29.
          gs_up_va-gwtyp  = ls_dados-value.
          gs_up_va-gwtypx = 'X'.
        WHEN 30.
          gs_up_va-gapzn  = ls_dados-value.
          gs_up_va-gapznx = 'X'.
        WHEN 31.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-glday.
          gs_up_va-gldayx = 'X'.
        WHEN 32.
          gs_up_va-gzein  = ls_dados-value.
          gs_up_va-gzeinx = 'X'.
        WHEN 33.
          gs_up_va-amt_curr  = ls_dados-value.
          gs_up_va-amt_currx = 'X'.
        WHEN 34.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-clamt.
          gs_up_va-clamtx = 'X'.
        WHEN 35.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-clday.
          gs_up_va-cldayx = 'X'.
        WHEN 36.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-cdurp.
          gs_up_va-cdurpx = 'X'.
        WHEN 37.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_up_va-ediff.
          gs_up_va-ediffx = 'X'.
        WHEN 38.
          gs_up_va-gener  = ls_dados-value.
          gs_up_va-generx = 'X'.
      ENDCASE.

      AT END OF row.
        APPEND gs_up_va TO gt_up_va.
        CLEAR: gs_up_va.
      ENDAT.
    ENDLOOP.
    IF sy-subrc NE 0.
      MESSAGE e000(zhr) WITH 'O ficheiro não contém dados.'.
    ENDIF.

  ELSEIF p_rt  IS NOT INITIAL
      OR p_crt IS NOT INITIAL.
    REFRESH: gt_upload.
    CLEAR: gs_upload.
    LOOP AT lt_dados INTO ls_dados.
      CASE ls_dados-col.
        WHEN 1.
          gs_upload-pernr = ls_dados-value.

          CLEAR: pnppernr.
          pnppernr-sign   = 'I'.
          pnppernr-option = 'EQ'.
          pnppernr-low    = gs_upload-pernr.
          COLLECT pnppernr.

        WHEN 2.
          gs_upload-lgart = ls_dados-value.

        WHEN 3.
          gs_upload-apaga = ls_dados-value.

        WHEN 4.
          gs_upload-abart = ls_dados-value.

        WHEN 5.
          gs_upload-apznr = ls_dados-value.

        WHEN 6.
          gs_upload-v0typ = ls_dados-value.

        WHEN 7.
          gs_upload-v0znr = ls_dados-value.

        WHEN 8.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_upload-anzhl.

        WHEN 9.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_upload-betrg.

        WHEN 10.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_upload-betpe.
      ENDCASE.

      AT END OF row.
        APPEND gs_upload TO gt_upload.
        CLEAR: gs_upload.
      ENDAT.
    ENDLOOP.
    IF sy-subrc NE 0.
      MESSAGE e000(zhr) WITH 'O ficheiro não contém dados.'.
    ENDIF.

  ELSEIF p_bt IS NOT INITIAL.
    REFRESH: gt_upload.
    CLEAR: gs_upload.
    LOOP AT lt_dados INTO ls_dados.
      CASE ls_dados-col.
        WHEN 1.
          gs_upload-pernr = ls_dados-value.

          CLEAR: pnppernr.
          pnppernr-sign   = 'I'.
          pnppernr-option = 'EQ'.
          pnppernr-low    = gs_upload-pernr.
          COLLECT pnppernr.

        WHEN 2.
          gs_upload-lgart = ls_dados-value.

        WHEN 3.
          PERFORM convert_char_to_decimal
            USING ls_dados-value
         CHANGING gs_upload-betrg.
      ENDCASE.

      AT END OF row.
        APPEND gs_upload TO gt_upload.
        CLEAR: gs_upload.
      ENDAT.
    ENDLOOP.
    IF sy-subrc NE 0.
      MESSAGE e000(zhr) WITH 'O ficheiro não contém dados.'.
    ENDIF.

  ENDIF.

ENDFORM.                    "get_file_data
*&---------------------------------------------------------------------*
*&      Form  CONVERT_CHAR_TO_DECIMAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_DADOS_VALUE  text
*      <--P_GS_UPLOAD_ANZHL  text
*----------------------------------------------------------------------*
FORM convert_char_to_decimal  USING    p_value_char TYPE char50
                              CHANGING p_value_num. "  TYPE ppt_pdays. "maxbt.

  DATA: my_usr01 TYPE usr01.

  DATA: lv_decimal, lv_thousands.
  CONDENSE p_value_char.

  CLEAR: my_usr01.
  SELECT SINGLE * INTO my_usr01
                  FROM usr01
                  WHERE bname EQ sy-uname.
  IF my_usr01-dcpfm EQ 'X'.
    lv_decimal   = '.'.
    lv_thousands = ','.
  ELSEIF my_usr01-dcpfm EQ 'Y'.
    lv_decimal   = ','.
    lv_thousands = ' '.
  ELSE.
    lv_decimal   = ','.
    lv_thousands = '.'.
  ENDIF.

  CALL FUNCTION 'HRCM_STRING_TO_AMOUNT_CONVERT'
    EXPORTING
      string              = p_value_char
      decimal_separator   = lv_decimal
      thousands_separator = lv_thousands
    IMPORTING
      betrg               = p_value_char
    EXCEPTIONS
      convert_error       = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
  ENDIF.

  p_value_num = p_value_char.

ENDFORM.                    "convert_char_to_decimal

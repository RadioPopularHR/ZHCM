PROCESS BEFORE OUTPUT.
*         general infotype-independent operations
  MODULE before_output.
  CALL SUBSCREEN subscreen_empl   INCLUDING empl_prog empl_dynnr.
  CALL SUBSCREEN subscreen_header INCLUDING header_prog header_dynnr.
*         infotype specific operations
  MODULE p9010.
*
  MODULE hidden_data.
*
PROCESS AFTER INPUT.

* >>> INI Inetum SAM EMP/SS HR 7000228548 21.04.2025
  FIELD p9010-idchefia MODULE check_on_save.
* <<< END Inetum SAM EMP/SS HR 7000228548 21.04.2025

*---------------------------------------------------------------------*
*  process exit commands
*---------------------------------------------------------------------*
  MODULE exit AT EXIT-COMMAND.
*---------------------------------------------------------------------*
*         processing after input
*---------------------------------------------------------------------*
*
*         check and mark if there was any input: all fields that
*         accept input HAVE TO BE listed here
*---------------------------------------------------------------------*
  CHAIN.
    FIELD p9010-begda.
    FIELD p9010-endda.
    FIELD p9010-ativo.
    FIELD p9010-equipa MODULE descricao_equipa ON INPUT.
    FIELD p9010-subarea MODULE descricao_subarea ON INPUT.
    FIELD p9010-centro_cst MODULE descricao_centro_cst ON INPUT.
    FIELD p9010-titulo_trabalho MODULE descricao_funcao ON INPUT.
    FIELD p9010-idchefia.
    FIELD p9010-email MODULE valida_email ON INPUT.
    FIELD p9010-idworkvivo.
    MODULE input_status ON CHAIN-REQUEST.
  ENDCHAIN.
*---------------------------------------------------------------------*
*      process functioncodes before input-checks                      *
*---------------------------------------------------------------------*
  MODULE pre_input_checks.
*---------------------------------------------------------------------*
*         input-checks:                                               *
*---------------------------------------------------------------------*

*   insert check modules here:

*  ...

*---------------------------------------------------------------------*
*     process function code: ALL fields that appear on the
*      screen HAVE TO BE listed here (including output-only fields)
*---------------------------------------------------------------------*
  CHAIN.
    FIELD p9010-begda.
    FIELD p9010-endda.
    FIELD rp50m-sprtx.
    FIELD p9010-ativo.
    FIELD p9010-equipa.
    FIELD p9010-subarea.
    FIELD p9010-centro_cst.
    FIELD p9010-titulo_trabalho.
    FIELD p9010-idchefia.
    FIELD p9010-email.
    FIELD p9010-idworkvivo.
    MODULE post_input_checks.
  ENDCHAIN.
*

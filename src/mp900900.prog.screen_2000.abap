PROCESS BEFORE OUTPUT.
*         general infotype-independent operations
  MODULE BEFORE_OUTPUT.

  CALL SUBSCREEN subscreen_empl   INCLUDING empl_prog empl_dynnr.
  CALL SUBSCREEN subscreen_header INCLUDING header_prog header_dynnr.
*         infotype specific operations
  MODULE P9009.
*
  MODULE HIDDEN_DATA.
*
PROCESS AFTER INPUT.
*---------------------------------------------------------------------*
*  process exit commands
*---------------------------------------------------------------------*
  MODULE EXIT AT EXIT-COMMAND.
*---------------------------------------------------------------------*
*         processing after input
*---------------------------------------------------------------------*
*
*         check and mark if there was any input: all fields that
*         accept input HAVE TO BE listed here
*---------------------------------------------------------------------*
  CHAIN.
    FIELD P9009-BEGDA.
    FIELD P9009-ENDDA.
    FIELD P9009-DESC_SECCAO.
    FIELD P9009-COD_SECCAO. "verifica_cod on CHAIN-REQUEST.
    MODULE INPUT_STATUS ON CHAIN-REQUEST.
  ENDCHAIN.
*---------------------------------------------------------------------*
*      process functioncodes before input-checks                      *
*---------------------------------------------------------------------*
  MODULE PRE_INPUT_CHECKS.
*---------------------------------------------------------------------*
*         input-checks:                                               *
*---------------------------------------------------------------------*
    CHAIN.
    FIELD P9009-COD_SECCAO MODULE verifica_cod on CHAIN-REQUEST.
    ENDCHAIN.
*   insert check modules here:

*  ...

*---------------------------------------------------------------------*
*     process function code: ALL fields that appear on the
*      screen HAVE TO BE listed here (including output-only fields)
*---------------------------------------------------------------------*
  CHAIN.
    FIELD P9009-BEGDA.
    FIELD P9009-ENDDA.
    FIELD RP50M-SPRTX.
    FIELD P9009-DESC_SECCAO.
    FIELD P9009-COD_SECCAO.
    MODULE verifica_cod.
    MODULE POST_INPUT_CHECKS.
  ENDCHAIN.
*

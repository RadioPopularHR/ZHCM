*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTWFM_HCM_SUBTYP................................*
DATA:  BEGIN OF STATUS_ZTWFM_HCM_SUBTYP              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTWFM_HCM_SUBTYP              .
CONTROLS: TCTRL_ZTWFM_HCM_SUBTYP
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZTWFM_HCM_SUBTYP              .
TABLES: ZTWFM_HCM_SUBTYP               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

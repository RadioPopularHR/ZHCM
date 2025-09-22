*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHR_SECCAO......................................*
DATA:  BEGIN OF STATUS_ZHR_SECCAO                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_SECCAO                    .
CONTROLS: TCTRL_ZHR_SECCAO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHR_SECCAO                    .
TABLES: ZHR_SECCAO                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

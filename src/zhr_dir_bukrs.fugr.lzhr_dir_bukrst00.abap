*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHR_DIR_BUKRS...................................*
DATA:  BEGIN OF STATUS_ZHR_DIR_BUKRS                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_DIR_BUKRS                 .
CONTROLS: TCTRL_ZHR_DIR_BUKRS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHR_DIR_BUKRS                 .
TABLES: ZHR_DIR_BUKRS                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

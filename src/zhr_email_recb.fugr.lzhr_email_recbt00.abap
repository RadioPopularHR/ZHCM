*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHR_EMAIL_RECB..................................*
DATA:  BEGIN OF STATUS_ZHR_EMAIL_RECB                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_EMAIL_RECB                .
CONTROLS: TCTRL_ZHR_EMAIL_RECB
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHR_EMAIL_RECB                .
TABLES: ZHR_EMAIL_RECB                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

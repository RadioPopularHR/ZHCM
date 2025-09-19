*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHR_RECIBO_LOG..................................*
DATA:  BEGIN OF STATUS_ZHR_RECIBO_LOG                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_RECIBO_LOG                .
CONTROLS: TCTRL_ZHR_RECIBO_LOG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHR_RECIBO_LOG                .
TABLES: ZHR_RECIBO_LOG                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

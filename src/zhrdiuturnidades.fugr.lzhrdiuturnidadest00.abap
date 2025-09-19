*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHRDIUTURNIDADE.................................*
DATA:  BEGIN OF STATUS_ZHRDIUTURNIDADE               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHRDIUTURNIDADE               .
CONTROLS: TCTRL_ZHRDIUTURNIDADE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHRDIUTURNIDADE               .
TABLES: ZHRDIUTURNIDADE                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHR_CENTRO_CUSTO................................*
DATA:  BEGIN OF STATUS_ZHR_CENTRO_CUSTO              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_CENTRO_CUSTO              .
CONTROLS: TCTRL_ZHR_CENTRO_CUSTO
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZHR_DES_EQUIPA_V................................*
TABLES: ZHR_DES_EQUIPA_V, *ZHR_DES_EQUIPA_V. "view work areas
CONTROLS: TCTRL_ZHR_DES_EQUIPA_V
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZHR_DES_EQUIPA_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHR_DES_EQUIPA_V.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHR_DES_EQUIPA_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHR_DES_EQUIPA_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHR_DES_EQUIPA_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHR_DES_EQUIPA_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHR_DES_EQUIPA_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHR_DES_EQUIPA_V_TOTAL.

*...processing: ZHR_EMAIL.......................................*
DATA:  BEGIN OF STATUS_ZHR_EMAIL                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_EMAIL                     .
CONTROLS: TCTRL_ZHR_EMAIL
            TYPE TABLEVIEW USING SCREEN '0007'.
*...processing: ZHR_FUNCAO......................................*
DATA:  BEGIN OF STATUS_ZHR_FUNCAO                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_FUNCAO                    .
CONTROLS: TCTRL_ZHR_FUNCAO
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZHR_IDCHEFIA....................................*
DATA:  BEGIN OF STATUS_ZHR_IDCHEFIA                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_IDCHEFIA                  .
CONTROLS: TCTRL_ZHR_IDCHEFIA
            TYPE TABLEVIEW USING SCREEN '0006'.
*...processing: ZHR_SUBAREA.....................................*
DATA:  BEGIN OF STATUS_ZHR_SUBAREA                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_SUBAREA                   .
CONTROLS: TCTRL_ZHR_SUBAREA
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZHR_TITULOTRAB_V................................*
TABLES: ZHR_TITULOTRAB_V, *ZHR_TITULOTRAB_V. "view work areas
CONTROLS: TCTRL_ZHR_TITULOTRAB_V
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZHR_TITULOTRAB_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHR_TITULOTRAB_V.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHR_TITULOTRAB_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHR_TITULOTRAB_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHR_TITULOTRAB_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHR_TITULOTRAB_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHR_TITULOTRAB_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHR_TITULOTRAB_V_TOTAL.

*.........table declarations:.................................*
TABLES: *ZHR_CENTRO_CUSTO              .
TABLES: *ZHR_EMAIL                     .
TABLES: *ZHR_FUNCAO                    .
TABLES: *ZHR_IDCHEFIA                  .
TABLES: *ZHR_SUBAREA                   .
TABLES: ZHR_CENTRO_CUSTO               .
TABLES: ZHR_DES_EQUIPA                 .
TABLES: ZHR_DES_EQUIPA_T               .
TABLES: ZHR_EMAIL                      .
TABLES: ZHR_FUNCAO                     .
TABLES: ZHR_IDCHEFIA                   .
TABLES: ZHR_SUBAREA                    .
TABLES: ZHR_TITULOTRAB                 .
TABLES: ZHR_TITULOTRAB_T               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

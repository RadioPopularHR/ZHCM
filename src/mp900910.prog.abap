*----------------------------------------------------------------------*
*                                                                      *
*       Data definition for infotype 9009                              *
*                                                                      *
*----------------------------------------------------------------------*
PROGRAM MP900900 MESSAGE-ID RP.

TABLES: P9009.
* the following tables are filled globally:
* T001P, T500P
* they can be made available with a TABLES-statement

FIELD-SYMBOLS: <PNNNN> STRUCTURE P9009
                       DEFAULT P9009.

DATA: PSAVE LIKE P9009.

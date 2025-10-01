*----------------------------------------------------------------------*
*                                                                      *
*       Data definition for infotype 9010                              *
*                                                                      *
*----------------------------------------------------------------------*
PROGRAM mp901000 MESSAGE-ID rp.

TABLES: p9010.
* the following tables are filled globally:
* T001P, T500P
* they can be made available with a TABLES-statement

FIELD-SYMBOLS: <pnnnn> STRUCTURE p9010
                       DEFAULT p9010.

DATA: psave LIKE p9010.

TYPES: BEGIN OF ty_desc,
         funcao  TYPE zhr_funcao-descricao,
*         plstx   TYPE t528t-plstx,
         subarea TYPE zhr_subarea-descricao,
         btext   TYPE t001p-btext,
         equipa  TYPE zhr_des_equipa_t-descricao,
         centro  TYPE zhr_centro_custo-descricao,
         ktext   TYPE cskt-ktext,
       END OF ty_desc.

DATA: ls_desc TYPE ty_desc.

FUNCTION-POOL zhr_pt_rpciidp0 MESSAGE-ID 3i.

TYPE-POOLS: ppt02.

TYPES: ppt02_resid TYPE zhr_pt_rpciidp0_resid_amounts OCCURS 0.

CONSTANTS:
  euro(3)      TYPE c VALUE 'EUR',
  income_a     LIKE pc2pb-incat VALUE 'A',
  income_h     LIKE pc2pb-incat VALUE 'H',
  income_hp(2) TYPE c VALUE 'H2',
  income_i     LIKE pc2pb-incat VALUE 'I'.

DATA: BEGIN OF resid_amounts OCCURS 0,
        pernr       LIKE pernr-pernr,
        resid       TYPE ppt_resid,
        income      LIKE pc207-betrg,
        deduction   LIKE pc207-betrg.
DATA: END OF resid_amounts.

DATA: tax_data1    TYPE ppt02_tax_data OCCURS 10 WITH HEADER LINE.

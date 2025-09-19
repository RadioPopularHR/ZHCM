*&---------------------------------------------------------------------*
*&  Include           ZHR_WFM_INTEGRACAO_TOP
*&---------------------------------------------------------------------*

CONSTANTS: gc_active      TYPE char1 VALUE '3',
           gc_inactive    TYPE char1 VALUE '0',
           gc_stat_z1     TYPE char2 VALUE 'Z1',
           gc_stat_z3     TYPE char2 VALUE 'Z3',
           gc_stat_z4     TYPE char2 VALUE 'Z4',
           gc_stat_zz     TYPE char2 VALUE 'ZZ',
           gc_max_entries TYPE i     VALUE 40.         "Alterado de 100 para 500

TYPES: BEGIN OF ty_s_contingent,
         ktart TYPE t556a-ktart,
         type  TYPE char1,
         ktext TYPE t556b-ktext,
         begda TYPE t556a-begda,
         endda TYPE t556a-endda,
         zeinh TYPE t556a-zeinh,
         qtneg TYPE t556a-qtneg,
       END OF ty_s_contingent.

DATA: gv_state      TYPE t569v-state,
      gv_state_text TYPE pv000-statetxt.

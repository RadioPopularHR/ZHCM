TYPES: BEGIN OF ppt02_inc_prev_years,
        pernr    LIKE pernr-pernr,
        inc_type(3),
        amount   LIKE pc207-betrg,
        year     TYPE num4,
      END OF ppt02_inc_prev_years,

      ppt02_inc_prev_years_tab TYPE ppt02_inc_prev_years OCCURS 0.

























*----------------------------------------------------------------------*
*                                                                      *
*       Output-modules for infotype 9009                               *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       MODULE  P9009 OUTPUT                                           *
*----------------------------------------------------------------------*
*       Default values, Texts                                          *
*----------------------------------------------------------------------*
MODULE p9009 OUTPUT.
  LOOP AT SCREEN.
      IF screen-name EQ 'P9009-DESC_SECCAO'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  IF psyst-nselc EQ yes.
*    LOOP AT SCREEN.
*      IF screen-name EQ 'P9009-DESC_SECCAO'.
*        screen-input = '0'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
* read text fields etc.; do this whenever the screen is show for the
*  first time:
*   PERFORM RExxxx.
    IF psyst-iinit = yes AND psyst-ioper = insert.
* generate default values; do this the very first time on insert only:
*     PERFORM GET_DEFAULT.
    ENDIF.
  ENDIF.
ENDMODULE.                    "P9009 OUTPUT
*----------------------------------------------------------------------*
*       MODULE  P9009L OUTPUT                                          *
*----------------------------------------------------------------------*
*       read texts for listscreen
*----------------------------------------------------------------------*
MODULE p9009l OUTPUT.
  LOOP AT SCREEN.
      IF screen-name EQ 'P9009-DESC_SECCAO'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
* PERFORM RExxxx.
ENDMODULE.                    "P9009L OUTPUT

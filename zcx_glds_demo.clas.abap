class ZCX_GLDS_DEMO definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  constants ZCX_GLDS_DEMO type SOTR_CONC value '005056A038571EE99B89DA0DCB0E43E5' ##NO_TEXT.
  data INFO type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !INFO type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_GLDS_DEMO IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = ZCX_GLDS_DEMO .
 ENDIF.
me->INFO = INFO .
  endmethod.
ENDCLASS.

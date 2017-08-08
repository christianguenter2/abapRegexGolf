CLASS zcx_regex_golf_regex_error DEFINITION
  PUBLIC
  INHERITING FROM zcx_regex_golf_error
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: get_text REDEFINITION.

ENDCLASS.



CLASS ZCX_REGEX_GOLF_REGEX_ERROR IMPLEMENTATION.


  METHOD get_text.

    result = COND #( WHEN previous IS BOUND THEN  previous->get_text( )
                     ELSE |Erroneous regex!| ).

  ENDMETHOD.
ENDCLASS.

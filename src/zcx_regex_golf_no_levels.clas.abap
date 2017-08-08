CLASS zcx_regex_golf_no_levels DEFINITION
  PUBLIC
  INHERITING FROM zcx_regex_golf_error
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      get_text REDEFINITION.

ENDCLASS.



CLASS zcx_regex_golf_no_levels IMPLEMENTATION.

  METHOD get_text.

    result = |Keine Levels gefunden|.

  ENDMETHOD.

ENDCLASS.

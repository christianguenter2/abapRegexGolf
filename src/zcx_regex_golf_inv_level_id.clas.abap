CLASS zcx_regex_golf_inv_level_id DEFINITION
  PUBLIC
  INHERITING FROM zcx_regex_golf_error
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      get_text REDEFINITION.

ENDCLASS.



CLASS zcx_regex_golf_inv_level_id IMPLEMENTATION.

  METHOD get_text.

    result = |Invalid Level ID|.

  ENDMETHOD.


ENDCLASS.

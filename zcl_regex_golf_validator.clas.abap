CLASS zcl_regex_golf_validator DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_result,
        word       TYPE string,
        pre_match  TYPE string,
        match      TYPE string,
        post_match TYPE string,
        matched    TYPE abap_bool,
      END OF ty_result .

    TYPES:
      tty_result TYPE STANDARD TABLE OF ty_result
                      WITH NON-UNIQUE EMPTY KEY
                      WITH NON-UNIQUE SORTED KEY matched_key
                           COMPONENTS matched .

    METHODS:
      validate
        IMPORTING
          !i_regex         TYPE csequence
          !it_tokens       TYPE stringtab
        RETURNING
          VALUE(rt_result) TYPE tty_result
        RAISING
          zcx_regex_golf_error.

  PRIVATE SECTION.

    METHODS:
      _find_first_occurence_of_regex
        IMPORTING
          !i_regex        TYPE csequence
          !i_token        TYPE csequence
        RETURNING
          VALUE(r_result) TYPE match_result
        RAISING
          zcx_regex_golf_error.

ENDCLASS.



CLASS ZCL_REGEX_GOLF_VALIDATOR IMPLEMENTATION.


  METHOD validate.

    DATA: results TYPE match_result.

    LOOP AT it_tokens ASSIGNING FIELD-SYMBOL(<token>).

      results = _find_first_occurence_of_regex( i_regex = i_regex
                                                i_token = <token> ).

      DATA(match_end_offset) = results-offset + results-length.

      APPEND VALUE #( word       = <token>
                      pre_match  = substring( val = <token>
                                              off = 0
                                              len = results-offset )
                      match      = substring( val = <token>
                                              off = results-offset
                                              len = results-length )
                      post_match = substring( val = <token>
                                              off = match_end_offset
                                              len = strlen( <token> ) - match_end_offset )
                      matched    = xsdbool( results IS NOT INITIAL ) )
            TO rt_result.

    ENDLOOP.


  ENDMETHOD.


  METHOD _find_first_occurence_of_regex.

    CHECK i_regex IS NOT INITIAL.

    TRY.

        FIND FIRST OCCURRENCE OF REGEX i_regex
                   IN i_token
                   RESULTS r_result.

      CATCH cx_sy_regex  INTO DATA(error).

        RAISE EXCEPTION TYPE zcx_regex_golf_regex_error
          EXPORTING
            previous = error.

    ENDTRY.

  ENDMETHOD.
ENDCLASS.

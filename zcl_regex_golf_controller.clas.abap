CLASS zcl_regex_golf_controller DEFINITION
  PUBLIC
  INHERITING FROM zcl_regex_golf_abs_controller
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: ty_regex_string TYPE c LENGTH 60.

    ALIASES:
      add_level 				 FOR zif_regex_golf_controller~add_level,
      delete_level			 FOR zif_regex_golf_controller~delete_level,
      get_current_level  FOR zif_regex_golf_controller~get_current_level,
      get_match_html		 FOR zif_regex_golf_controller~get_match_html,
      get_non_match_html FOR zif_regex_golf_controller~get_non_match_html,
      get_top_html			 FOR zif_regex_golf_controller~get_top_html,
      pick_level				 FOR zif_regex_golf_controller~pick_level,
      random_level			 FOR zif_regex_golf_controller~random_level,
      validate					 FOR zif_regex_golf_controller~validate.

    METHODS:
      constructor
        IMPORTING
          i_regex_input TYPE REF TO ty_regex_string
        RAISING
          zcx_regex_golf_error,

      zif_regex_golf_controller~delete_level			 REDEFINITION,
      zif_regex_golf_controller~get_current_level  REDEFINITION,
      zif_regex_golf_controller~get_match_html		 REDEFINITION,
      zif_regex_golf_controller~get_non_match_html REDEFINITION,
      zif_regex_golf_controller~get_top_html			 REDEFINITION,
      zif_regex_golf_controller~pick_level				 REDEFINITION,
      zif_regex_golf_controller~random_level			 REDEFINITION,
      zif_regex_golf_controller~validate					 REDEFINITION.

  PRIVATE SECTION.

    DATA: mo_validator          TYPE REF TO zcl_regex_golf_validator,
          mt_result_matches     TYPE zcl_regex_golf_validator=>tty_result,
          mt_result_non_matches TYPE zcl_regex_golf_validator=>tty_result,
          mr_regex_input        TYPE REF TO ty_regex_string.

    METHODS:
      _get_html
        IMPORTING
          !it_result     TYPE zcl_regex_golf_validator=>tty_result
          !i_match       TYPE abap_bool
        RETURNING
          VALUE(rt_html) TYPE w3htmltab,

      _get_html_for_token
        IMPORTING
          !i_result       TYPE zcl_regex_golf_validator=>ty_result
          !i_match        TYPE abap_bool
        RETURNING
          VALUE(r_result) TYPE string,

      _validate
        IMPORTING
          !i_regex         TYPE csequence
          !it_tokens       TYPE stringtab
        RETURNING
          VALUE(rt_result) TYPE zcl_regex_golf_validator=>tty_result
        RAISING
          zcx_regex_golf_error,

      _level_solved
        RETURNING
          VALUE(r_level_solved) TYPE abap_bool,

      _initialize
        RAISING
          zcx_regex_golf_error.

ENDCLASS.



CLASS zcl_regex_golf_controller IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    IF lines( mo_level->mt_levels ) = 0.
      RAISE EXCEPTION TYPE zcx_regex_golf_no_levels.
    ENDIF.

    mr_regex_input = i_regex_input.

    _initialize( ).

  ENDMETHOD.


  METHOD zif_regex_golf_controller~delete_level.

    DATA: level_to_delete TYPE i.

    cl_demo_input=>request(
      EXPORTING
        text        = |Level to Delete 1 - { mo_level->get_level_count( ) }|
      CHANGING
        field       = level_to_delete ).

    TRY.

        mo_level->delete_level( level_to_delete ).

      CATCH zcx_regex_golf_error INTO DATA(error).

        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.

    mo_level->random_level( ).

    MESSAGE |Level { level_to_delete } deleted| TYPE 'S'.

  ENDMETHOD.


  METHOD zif_regex_golf_controller~get_current_level.

    r_current_level = mo_level->get_current_level( ).

  ENDMETHOD.


  METHOD zif_regex_golf_controller~get_match_html.

    rt_html = _get_html( it_result = mt_result_matches
                         i_match   = abap_true ).

  ENDMETHOD.


  METHOD zif_regex_golf_controller~get_non_match_html.

    rt_html = _get_html( it_result = mt_result_non_matches
                         i_match   = abap_false ).

  ENDMETHOD.


  METHOD zif_regex_golf_controller~get_top_html.

    DATA(html) = `<html><head><style type="text/css">`
              && `body {`
              && `   overflow: hidden;`
              && `   font-family: arial;`
              && ` }`
              && `.success {`
              && `   color: green;`
              && `   font-size: 24px;`
              && `   width: 30%;`
              && `   margin: auto;`
              && `}`
              && `</style></head><body>`
              && COND #( WHEN _level_solved( ) = abap_true THEN `<div class="success">Level solved!</div>` )
              && `</body><html>` ##NO_TEXT.

    cl_bizc_xml_services=>convert_string_to_table(
      EXPORTING
        ip_string    = html
      IMPORTING
        et_table     = rt_html ).

  ENDMETHOD.


  METHOD zif_regex_golf_controller~pick_level.

    DATA: new_level_id TYPE zcl_regex_golf_level=>ty_level_id,
          level_count  TYPE i.

    LOOP AT mo_level->mt_levels ASSIGNING FIELD-SYMBOL(<level>).

      level_count = level_count + 1.

      cl_demo_input=>add_text( |{ level_count } { <level>-description } | ).

    ENDLOOP.

    cl_demo_input=>request(
      EXPORTING
        text        = |Choose Level 1 - { mo_level->get_level_count( ) }|
      CHANGING
        field       = new_level_id ).

    IF new_level_id IS INITIAL.

      RETURN.

    ENDIF.

    TRY.

        mo_level->set_level( new_level_id ).

        CLEAR mr_regex_input->*.

        _initialize( ).

      CATCH zcx_regex_golf_error INTO DATA(error).

        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

  ENDMETHOD.


  METHOD zif_regex_golf_controller~random_level.

    mo_level->random_level( ).

    CLEAR mr_regex_input->*.

    _initialize( ).

  ENDMETHOD.


  METHOD zif_regex_golf_controller~validate.

    mt_result_matches = _validate( i_regex   = i_regex
                                   it_tokens = mo_level->get_matches( ) ).

    mt_result_non_matches = _validate( i_regex   = i_regex
                                       it_tokens = mo_level->get_non_matches( ) ).

  ENDMETHOD.


  METHOD _get_html.

    CONSTANTS: co_max_lines_for_one_page TYPE i VALUE 22.

    DATA(html) = `<html><head><style type="text/css">` ##NO_TEXT
              && `.inline {`
              && `  display: inline;`
              && `}`
              && `.match {`
              && `  font-weight: bold;`
              && `}`
              && `.wrong-match {`
              && `  color: red;`
              && `}`
              && `.right-match {`
              && `  color: green;`
              && `}`
              && `.grayed {`
              && `  color: #e6e6e6;`
              && `}`
              && `body {`
              && COND #( WHEN lines( it_result ) < co_max_lines_for_one_page THEN `   overflow: hidden;` )
              && `   font-family: arial;`
              && ` }`
              && `</style></head><body>`
              && REDUCE string( INIT result = ||
                                FOR <result> IN it_result
                                NEXT result = result && _get_html_for_token( i_result = <result>
                                                                             i_match  = i_match ) )
              && `</body><html>`.

    cl_bizc_xml_services=>convert_string_to_table(
      EXPORTING
        ip_string    = html
      IMPORTING
        et_table     = rt_html ).

  ENDMETHOD.


  METHOD _get_html_for_token.

    CONSTANTS:
      BEGIN OF html_sign,
        cross      TYPE string VALUE '&#x2717;' ##NO_TEXT,
        check_mark TYPE string VALUE '&#x2713;' ##NO_TEXT,
      END OF html_sign.

    r_result = `<p>`
            && `<div class="inline">`
            && COND string( WHEN i_match = abap_true AND i_result-matched = abap_true
                             THEN |<div class="inline right-match">{ html_sign-check_mark }</div>|
                            WHEN i_match = abap_false AND i_result-matched = abap_true
                             THEN |<div class="inline wrong-match">{ html_sign-cross }</div>|
                            WHEN i_match = abap_true AND i_result-matched = abap_false
                             THEN |<div class="inline grayed">{ html_sign-check_mark }</div>|
                            WHEN i_match = abap_false AND i_result-matched = abap_false
                             THEN |<div class="inline grayed">{ html_sign-cross }</div>| )
            && i_result-pre_match
            && `<div class="match inline">`
            && i_result-match
            && `</div>`
            && i_result-post_match
            && `</div>`
            && `</p>`.

  ENDMETHOD.


  METHOD _initialize.

    mo_validator = NEW zcl_regex_golf_validator( ).

    validate( mr_regex_input->* ).

  ENDMETHOD.


  METHOD _level_solved.

    DATA(success_matches) = FILTER zcl_regex_golf_validator=>tty_result( mt_result_matches USING KEY matched_key
                                                                                WHERE matched = abap_true ).

    DATA(success_non_matches) = FILTER zcl_regex_golf_validator=>tty_result( mt_result_non_matches USING KEY matched_key
                                                                                    WHERE matched = abap_true ).

    r_level_solved = boolc( lines( success_matches ) = lines(  mt_result_matches )
                        AND lines( success_non_matches ) = 0 ).

  ENDMETHOD.


  METHOD _validate.

    mo_validator->validate(
      EXPORTING
        i_regex   = i_regex
        it_tokens = it_tokens
      RECEIVING
        rt_result = rt_result ).

  ENDMETHOD.
ENDCLASS.

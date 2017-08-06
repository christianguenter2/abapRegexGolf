*&---------------------------------------------------------------------*
*& Report  z_test_abap_regex_golf
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT z_abap_regex_golf.

DATA: ok_code     TYPE sy-ucomm,
      regex_input TYPE c LENGTH 60.

CLASS cx_error DEFINITION INHERITING FROM cx_static_check.

  PUBLIC SECTION.

    METHODS:

      constructor
        IMPORTING
          i_syst_message TYPE abap_bool OPTIONAL
          i_msgid        TYPE symsgid DEFAULT sy-msgid
          i_msgno        TYPE symsgno DEFAULT sy-msgno
          i_msgv1        TYPE symsgv DEFAULT sy-msgv1
          i_msgv2        TYPE symsgv DEFAULT sy-msgv2
          i_msgv3        TYPE symsgv DEFAULT sy-msgv3
          i_msgv4        TYPE symsgv DEFAULT sy-msgv4
          previous       TYPE REF TO cx_root OPTIONAL,

      get_text REDEFINITION.

  PRIVATE SECTION.

    DATA: _text TYPE string.

ENDCLASS.

CLASS cx_error IMPLEMENTATION.

  METHOD constructor.

    DATA: message TYPE bapi_msg.

    super->constructor( previous = previous ).

    IF i_syst_message = abap_false.

      RETURN.

    ENDIF.

    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = i_msgid
        number     = i_msgno
        textformat = 'ASC' ##NO_TEXT
        message_v1 = i_msgv1
        message_v2 = i_msgv2
        message_v3 = i_msgv3
        message_v4 = i_msgv4
      IMPORTING
        message    = message.

    _text = message.

  ENDMETHOD.

  METHOD get_text.

    result = COND #( WHEN _text IS NOT INITIAL THEN _text
                     ELSE super->get_text( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS cx_invalid_input DEFINITION INHERITING FROM cx_error FINAL.

ENDCLASS.

CLASS cx_invalid_input IMPLEMENTATION.

ENDCLASS.

CLASS cx_regex_error DEFINITION INHERITING FROM cx_error FINAL.

  PUBLIC SECTION.

    METHODS: get_text REDEFINITION.

ENDCLASS.

CLASS cx_regex_error IMPLEMENTATION.

  METHOD get_text.

    result = COND #( WHEN previous IS BOUND THEN  previous->get_text( )
                     ELSE |Erroneous regex!| ).

  ENDMETHOD.

ENDCLASS.

CLASS cx_invalid_level_id DEFINITION INHERITING FROM cx_error FINAL.

  PUBLIC SECTION.

    METHODS: get_text REDEFINITION.

ENDCLASS.

CLASS cx_invalid_level_id IMPLEMENTATION.

  METHOD get_text.

    result = |Invalid Level ID|.

  ENDMETHOD.

ENDCLASS.

CLASS validator DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:

      BEGIN OF ty_result,
        word       TYPE string,
        pre_match  TYPE string,
        match      TYPE string,
        post_match TYPE string,
        matched    TYPE abap_bool,
      END OF ty_result,

      tty_result TYPE STANDARD TABLE OF ty_result
                 WITH NON-UNIQUE EMPTY KEY
                 WITH NON-UNIQUE SORTED KEY matched_key
                      COMPONENTS matched.

    METHODS:

      validate
        IMPORTING
          i_regex          TYPE csequence
          it_tokens        TYPE stringtab
        RETURNING
          VALUE(rt_result) TYPE tty_result
        RAISING
          cx_error.

  PRIVATE SECTION.

    METHODS:

      _find_first_occurence_of_regex
        IMPORTING
          i_regex         TYPE csequence
          i_token         TYPE csequence
        RETURNING
          VALUE(r_result) TYPE match_result
        RAISING
          cx_error.

ENDCLASS.

CLASS validator IMPLEMENTATION.

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

        RAISE EXCEPTION TYPE cx_regex_error
          EXPORTING
            previous = error.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS level DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:

      constructor
        IMPORTING
          i_level TYPE i OPTIONAL
        RAISING
          cx_error,

      get_matches
        RETURNING VALUE(rt_matches) TYPE stringtab,

      get_non_matches
        RETURNING VALUE(rt_non_matches) TYPE stringtab,

      get_current_level
        RETURNING
          VALUE(r_current_level) TYPE i,

      random_level,

      set_level
        IMPORTING
          i_new_level_id TYPE i
        RAISING
          cx_error,

      get_level_count
        RETURNING VALUE(r_count) TYPE i,

      add_level
        IMPORTING
          i_matches             TYPE string
          i_non_matches         TYPE string
        RETURNING
          VALUE(r_new_level_id) TYPE i
        RAISING
          cx_error,

      delete_level
        IMPORTING
          i_level_id TYPE i
        RAISING
          cx_error.

  PRIVATE SECTION.

    TYPES:

      BEGIN OF ty_level,
        id          TYPE i,
        matches     TYPE stringtab,
        non_matches TYPE stringtab,
      END OF ty_level,

      tty_level TYPE HASHED TABLE OF ty_level
                WITH UNIQUE KEY id.

    CONSTANTS: co_level_xml_file TYPE string VALUE `/SAP/PUBLIC/levels.xml` ##NO_TEXT.

    DATA:

      _levels              TYPE tty_level,
      _current_level       LIKE LINE OF _levels,
      _mime_repository_api TYPE REF TO if_mr_api.

    METHODS:

      _get_random_level_id
        RETURNING
          VALUE(r_random_level_id) TYPE i,

      _load_levels
        RETURNING
          VALUE(r_levels) TYPE level=>tty_level
        RAISING
          cx_invalid_input,

      _save_levels
        RAISING
          cx_error.

ENDCLASS.

CLASS level IMPLEMENTATION.

  METHOD constructor.

    _mime_repository_api = cl_mime_repository_api=>get_api( ).

    _levels = _load_levels( ).

    DATA(level_id) = COND #( WHEN i_level IS SUPPLIED THEN i_level
                             ELSE _get_random_level_id( ) ).

    set_level( level_id ).

  ENDMETHOD.

  METHOD _get_random_level_id.

    DATA(random) = cl_abap_random_int=>create( seed = CONV #( sy-uzeit )
                                               min  = 1
                                               max  = lines( _levels  ) ).

    DO.

      r_random_level_id = random->get_next( ).

      IF r_random_level_id <> _current_level-id
         OR lines( _levels ) <= 1.

        EXIT.

      ENDIF.

    ENDDO.

  ENDMETHOD.

  METHOD get_matches.

    rt_matches = _current_level-matches.

  ENDMETHOD.

  METHOD get_non_matches.

    rt_non_matches = _current_level-non_matches.

  ENDMETHOD.

  METHOD get_current_level.

    r_current_level = _current_level-id.

  ENDMETHOD.

  METHOD random_level.

    _current_level = _levels[ id = _get_random_level_id( ) ].

  ENDMETHOD.

  METHOD set_level.

    IF NOT line_exists( _levels[ id = i_new_level_id ] ).

      RAISE EXCEPTION TYPE cx_invalid_level_id.

    ENDIF.

    _current_level = _levels[ id = i_new_level_id ].

  ENDMETHOD.

  METHOD get_level_count.

    r_count = lines( _levels ).

  ENDMETHOD.

  METHOD _load_levels.

    _mime_repository_api->get(
      EXPORTING
        i_url                  = co_level_xml_file    " Object URL
        i_check_authority      = ' '    " X Check Authorization, '' No Authorization Check
      IMPORTING
        e_content              = DATA(content)    " Object Content
      EXCEPTIONS
        OTHERS                 = 1 ).

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE cx_invalid_input
        EXPORTING
          i_syst_message = abap_true.

    ENDIF.

    CALL TRANSFORMATION id SOURCE XML content
                           RESULT levels = r_levels.

  ENDMETHOD.

  METHOD add_level.

    SPLIT:
      i_matches     AT space INTO TABLE DATA(lt_matches),
      i_non_matches AT space INTO TABLE DATA(lt_non_matches).

    INSERT VALUE ty_level( id          = lines( _levels ) + 1
                           matches     = lt_matches
                           non_matches = lt_non_matches )
           INTO TABLE _levels ASSIGNING FIELD-SYMBOL(<new_level>).

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE cx_invalid_input.

    ENDIF.

    _save_levels( ).

    r_new_level_id = <new_level>-id.

  ENDMETHOD.

  METHOD _save_levels.

    CALL TRANSFORMATION id SOURCE levels = _levels
                           RESULT XML DATA(content).

    _mime_repository_api->put(
      EXPORTING
        i_url     = co_level_xml_file    " Object URL
        i_content = content    " Object Contents (if exists -> overwrite contents)
      EXCEPTIONS
        OTHERS    = 1 ).

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE cx_invalid_input
        EXPORTING
          i_syst_message = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD delete_level.

    CHECK i_level_id IS NOT INITIAL.

    DELETE _levels WHERE id = i_level_id.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE cx_invalid_level_id.

    ENDIF.

    _save_levels( ).

  ENDMETHOD.

ENDCLASS.

CLASS controller DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS:
      constructor
        RAISING
          cx_error,

      get_match_html
        RETURNING VALUE(rt_html) TYPE w3htmltab,

      get_non_match_html
        RETURNING VALUE(rt_html) TYPE w3htmltab,

      get_current_level
        RETURNING
          VALUE(r_current_level) TYPE i,

      random_level
        RAISING
          cx_error,

      pick_level,

      add_level,

      get_top_html
        RETURNING VALUE(rt_html) TYPE w3htmltab,

      validate
        IMPORTING
          i_regex TYPE csequence
        RAISING
          cx_error,

      delete_level.

  PRIVATE SECTION.

    DATA:
      _validator            TYPE REF TO validator,
      _level                TYPE REF TO level,
      _t_result_matches     TYPE validator=>tty_result,
      _t_result_non_matches TYPE validator=>tty_result.

    METHODS:

      _get_html
        IMPORTING
          it_result      TYPE validator=>tty_result
          i_match        TYPE abap_bool
        RETURNING
          VALUE(rt_html) TYPE w3htmltab,

      _get_html_for_token
        IMPORTING
          i_result        TYPE validator=>ty_result
          i_match         TYPE abap_bool
        RETURNING
          VALUE(r_result) TYPE string,

      _validate
        IMPORTING
          i_regex          TYPE csequence
          it_tokens        TYPE stringtab
        RETURNING
          VALUE(rt_result) TYPE validator=>tty_result
        RAISING
          cx_error,

      _level_solved
        RETURNING
          VALUE(r_level_solved) TYPE abap_bool,

      _initialize
        RAISING
          cx_error.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD constructor.

    _level = NEW level( ).

    _initialize( ).

  ENDMETHOD.

  METHOD get_match_html.

    rt_html = _get_html( it_result = _t_result_matches
                         i_match   = abap_true ).

  ENDMETHOD.

  METHOD get_non_match_html.

    rt_html = _get_html( it_result = _t_result_non_matches
                         i_match   = abap_false ).

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

  METHOD _validate.

    _validator->validate(
      EXPORTING
        i_regex   = i_regex
        it_tokens = it_tokens
      RECEIVING
        rt_result = rt_result ).

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

  METHOD get_current_level.

    r_current_level = _level->get_current_level( ).

  ENDMETHOD.

  METHOD random_level.

    _level->random_level( ).

    _initialize( ).

  ENDMETHOD.

  METHOD pick_level.

    DATA: new_level_id TYPE i.

    cl_demo_input=>request(
      EXPORTING
        text        = |Choose Level 1 - { _level->get_level_count( ) }|
      CHANGING
        field       = new_level_id ).

    IF new_level_id IS INITIAL.

      RETURN.

    ENDIF.

    TRY.

        _level->set_level( new_level_id ).

        _initialize( ).

      CATCH cx_error INTO DATA(error).

        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

  ENDMETHOD.

  METHOD add_level.

    DATA: matches     TYPE string,
          non_matches TYPE string.

    cl_demo_input=>new(
                )->add_field( EXPORTING
                                text  = 'Matches (separated by whitespaces)'
                              CHANGING
                                field = matches
                )->add_field( EXPORTING
                                text = 'Non-Matches (separated by whitespaces)'
                              CHANGING
                                field = non_matches
                )->request( ) ##NO_TEXT.

    TRY.

        DATA(new_level_id) = _level->add_level( i_matches     = matches
                                                i_non_matches = non_matches ).

      CATCH cx_error INTO DATA(error).

        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.

    MESSAGE |Level { new_level_id } created| TYPE 'S'.

  ENDMETHOD.

  METHOD get_top_html.

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

  METHOD validate.

    _t_result_matches = _validate( i_regex   = i_regex
                                   it_tokens = _level->get_matches( ) ).

    _t_result_non_matches = _validate( i_regex   = i_regex
                                       it_tokens = _level->get_non_matches( ) ).

  ENDMETHOD.

  METHOD _level_solved.

    DATA(success_matches) = FILTER validator=>tty_result( _t_result_matches USING KEY matched_key
                                                                                WHERE matched = abap_true ).

    DATA(success_non_matches) = FILTER validator=>tty_result( _t_result_non_matches USING KEY matched_key
                                                                                    WHERE matched = abap_true ).

    r_level_solved = boolc( lines( success_matches ) = lines(  _t_result_matches )
                        AND lines( success_non_matches ) = 0 ).

  ENDMETHOD.

  METHOD _initialize.

    _validator = NEW validator( ).

    validate( regex_input ).

  ENDMETHOD.

  METHOD delete_level.

    DATA: level_to_delete TYPE i.

    cl_demo_input=>request(
      EXPORTING
        text        = |Level to Delete 1 - { _level->get_level_count( ) }|
      CHANGING
        field       = level_to_delete ).

    TRY.

        _level->delete_level( level_to_delete ).

      CATCH cx_error INTO DATA(error).

        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.

    _level->random_level( ).

    MESSAGE |Level { level_to_delete } deleted| TYPE 'S'.

  ENDMETHOD.

ENDCLASS.

CLASS view DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS get
      RETURNING
        VALUE(r_instance) TYPE REF TO view.

    INTERFACES: if_amc_message_receiver_text.

    METHODS:

      constructor,

      pbo,

      pai.

  PRIVATE SECTION.

    CLASS-DATA:

      _instance TYPE REF TO view.

    DATA:

      _controller TYPE REF TO controller,

      BEGIN OF _custom_container,
        top   TYPE REF TO cl_gui_custom_container,
        left  TYPE REF TO cl_gui_custom_container,
        right TYPE REF TO cl_gui_custom_container,
      END OF _custom_container,

      BEGIN OF _custom_control,
        top   TYPE REF TO cl_gui_html_viewer,
        left  TYPE REF TO cl_gui_html_viewer,
        right TYPE REF TO cl_gui_html_viewer,
      END OF _custom_control,
      first_time TYPE abap_bool.

    METHODS:

      _left_control
        RAISING
          cx_error,

      _right_control
        RAISING
          cx_error,

      _control_processing
        IMPORTING
          i_container_name TYPE csequence
          it_html          TYPE w3htmltab
        CHANGING
          co_container     TYPE REF TO cl_gui_custom_container
          co_control       TYPE REF TO cl_gui_html_viewer,

      _top_control,

      _dispatch_ok_code
        IMPORTING
          i_ok_code TYPE sy-ucomm
        RAISING
          cx_error,

      _paint_controls
        RAISING
          cx_error,

      _initialize_amc_receiver.

    CONSTANTS:

      BEGIN OF co_ok_code,
        exit   TYPE sy-ucomm VALUE '&EXIT',
        docu   TYPE sy-ucomm VALUE '&DOCU',
        enter  TYPE sy-ucomm VALUE '&ENTER',
        pick   TYPE sy-ucomm VALUE '&PICK',
        rand   TYPE sy-ucomm VALUE '&RAND',
        add    TYPE sy-ucomm VALUE '&ADD',
        delete TYPE sy-ucomm VALUE '&DEL',
      END OF co_ok_code,

      BEGIN OF co_custom_control_name,
        top   TYPE c LENGTH 30 VALUE `CC_TOP`,
        left  TYPE c LENGTH 30 VALUE `CC_LEFT`,
        right TYPE c LENGTH 30 VALUE `CC_RIGHT`,
      END OF co_custom_control_name.

ENDCLASS.

CLASS view IMPLEMENTATION.

  METHOD constructor.

    first_time = abap_true.

    TRY.
        _controller = NEW controller( ).

      CATCH cx_error INTO DATA(error).

        MESSAGE error TYPE 'I'.

    ENDTRY.

    _initialize_amc_receiver( ).

  ENDMETHOD.

  METHOD _initialize_amc_receiver.

    DATA: consumer TYPE REF TO if_amc_message_consumer.

    TRY.
        SET PARAMETER ID 'SAPGUI_PUSH_CHANNEL' FIELD abap_true.

        CALL METHOD (`\PROGRAM=CL_AMC_CHANNEL_MANAGER========CP\CLASS=LCL_SAPGUI_CHANNEL_MANAGER`)=>create_message_consumer
          EXPORTING
            i_application_id = |ZAMC_ABAP_REGEX_GOLF|
            i_channel_id     = |/golf|
          RECEIVING
            r_consumer       = consumer.

        consumer->start_message_delivery( me ).

      CATCH cx_amc_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD get.

    r_instance = _instance = COND #( WHEN _instance IS BOUND THEN _instance
                                     ELSE NEW view( ) ).

  ENDMETHOD.

  METHOD pbo.

    DATA(current_level) = |{ _controller->get_current_level( ) }|.

    SET:
      PF-STATUS 'MAIN_0100',
      TITLEBAR 'MAIN_0100' WITH current_level.

    TRY.

        _paint_controls( ).

      CATCH cx_error INTO DATA(error).

        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

    cl_dsh_dynpro_properties=>enable_type_ahead( VALUE #( ( `REGEX_INPUT` ) ) ).

  ENDMETHOD.

  METHOD pai.

    DATA(l_ok_code) = ok_code.

    CLEAR ok_code.

    TRY.
        _dispatch_ok_code( l_ok_code ).

      CATCH cx_error INTO DATA(error).

        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

  ENDMETHOD.

  METHOD _left_control.

    _control_processing(
      EXPORTING
        i_container_name = co_custom_control_name-left
        it_html          = _controller->get_match_html( )
      CHANGING
        co_container = _custom_container-left
        co_control   = _custom_control-left ).

  ENDMETHOD.

  METHOD _control_processing.

    CONSTANTS url TYPE sysuuid_c32 VALUE 'BA2B6CF0A7031EE5BD99016A2B1114B5' ##NO_TEXT.

    co_container = COND #( WHEN co_container IS BOUND THEN co_container
                           ELSE NEW cl_gui_custom_container( container_name  = i_container_name ) ).

    co_control = COND #( WHEN co_control IS BOUND THEN co_control
                         ELSE NEW cl_gui_html_viewer( parent = co_container ) ).

    DATA(lt_html) = it_html.

    co_control->load_data(
      EXPORTING
        url        = url
      CHANGING
        data_table = lt_html
      EXCEPTIONS
        OTHERS     = 1 ).

    ASSERT sy-subrc = 0.

    co_control->show_url(
      EXPORTING
        url        = url
      EXCEPTIONS
        OTHERS     = 1 ).

    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD _right_control.

    _control_processing(
      EXPORTING
        i_container_name = co_custom_control_name-right
        it_html          = _controller->get_non_match_html( )
      CHANGING
        co_container = _custom_container-right
        co_control   = _custom_control-right ).

  ENDMETHOD.

  METHOD _top_control.

    _control_processing(
      EXPORTING
        i_container_name = co_custom_control_name-top
        it_html          = _controller->get_top_html( )
      CHANGING
        co_container = _custom_container-top
        co_control   = _custom_control-top ).

  ENDMETHOD.

  METHOD _dispatch_ok_code.

    CASE i_ok_code.

      WHEN co_ok_code-exit.

        SET SCREEN 0.
        RETURN.

      WHEN co_ok_code-docu.

        cl_abap_docu=>show(
          EXPORTING
            area = |ABEN|
            name = |REGEX_SYNTAX| ).

      WHEN co_ok_code-enter.

        _controller->validate( regex_input ).

      WHEN co_ok_code-pick.

        _controller->pick_level( ).

      WHEN co_ok_code-rand.

        _controller->random_level( ).

      WHEN co_ok_code-add.

        _controller->add_level( ).

      WHEN co_ok_code-delete.

        _controller->delete_level( ).

    ENDCASE.

  ENDMETHOD.

  METHOD if_amc_message_receiver_text~receive.

    " strip last character, because it's a * added by the search help
    DATA(message) = COND #( LET len = strlen( i_message ) IN
                            WHEN len > 0
                            THEN substring( val = i_message
                                            off = 0
                                            len = len - 1 ) ).

    TRY.
        _controller->validate( message ).
        _paint_controls( ).

      CATCH cx_error INTO DATA(error).

        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

  ENDMETHOD.

  METHOD _paint_controls.

    _top_control( ).
    _left_control( ).
    _right_control( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  CALL SCREEN 0100.

MODULE status_0100 OUTPUT.

  view=>get( )->pbo( ).

ENDMODULE.

MODULE user_command_0100 INPUT.

  view=>get( )->pai( ).

ENDMODULE.

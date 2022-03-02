CLASS zcl_regex_golf_level DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_level_id TYPE string .
    TYPES:
      BEGIN OF ty_level,
        id          TYPE ty_level_id,
        description TYPE string,
        matches     TYPE stringtab,
        non_matches TYPE stringtab,
      END OF ty_level .
    TYPES:
      tty_level TYPE STANDARD TABLE OF ty_level
                       WITH NON-UNIQUE DEFAULT KEY .

    DATA mt_levels TYPE tty_level READ-ONLY .

    METHODS:
      constructor
        IMPORTING
          !i_level TYPE ty_level_id OPTIONAL
        RAISING
          zcx_regex_golf_error,

      get_matches
        RETURNING
          VALUE(rt_matches) TYPE stringtab,

      get_non_matches
        RETURNING
          VALUE(rt_non_matches) TYPE stringtab,

      get_current_level
        RETURNING
          VALUE(r_current_level) TYPE ty_level_id,

      random_level,

      set_level
        IMPORTING
          !i_new_level_id TYPE ty_level_id
        RAISING
          zcx_regex_golf_error,

      get_level_count
        RETURNING
          VALUE(r_count) TYPE ty_level_id,

      add_level
        IMPORTING
          !i_description        TYPE csequence
          !i_matches            TYPE csequence
          !i_non_matches        TYPE csequence
        RETURNING
          VALUE(r_new_level_id) TYPE zcl_regex_golf_level=>ty_level_id
        RAISING
          zcx_regex_golf_error,

      delete_level
        IMPORTING
          !i_level_index TYPE i
        RAISING
          zcx_regex_golf_error.

  PRIVATE SECTION.

    CONSTANTS: co_level_xml_file TYPE string VALUE `/SAP/PUBLIC/levels.xml` ##NO_TEXT.

    DATA:
      m_current_level       LIKE LINE OF mt_levels,
      m_mime_repository_api TYPE REF TO if_mr_api.

    METHODS:
      _get_random_level_id
        RETURNING
          VALUE(r_random_level_id) TYPE zcl_regex_golf_level=>ty_level_id,

      _load_levels
        RETURNING
          VALUE(r_levels) TYPE zcl_regex_golf_level=>tty_level
        RAISING
          zcx_regex_golf_invalid_input
          zcx_regex_golf_no_levels,

      _save_levels
        RAISING
          zcx_regex_golf_error.

ENDCLASS.



CLASS zcl_regex_golf_level IMPLEMENTATION.


  METHOD add_level.


    SPLIT:
      i_matches     AT space INTO TABLE DATA(lt_matches),
      i_non_matches AT space INTO TABLE DATA(lt_non_matches).

    INSERT VALUE ty_level( id          = cl_system_uuid=>create_uuid_c32_static( )
                           description = i_description
                           matches     = lt_matches
                           non_matches = lt_non_matches )
           INTO TABLE mt_levels ASSIGNING FIELD-SYMBOL(<new_level>).

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_regex_golf_invalid_input.

    ENDIF.

    _save_levels( ).

    r_new_level_id = <new_level>-id.


  ENDMETHOD.


  METHOD constructor.


    m_mime_repository_api = cl_mime_repository_api=>get_api( ).

    mt_levels = _load_levels( ).

    DATA(level_id) = COND #( WHEN i_level IS SUPPLIED THEN i_level
                             WHEN lines( mt_levels ) = 0 THEN 0
                             ELSE _get_random_level_id( ) ).

    set_level( level_id ).


  ENDMETHOD.


  METHOD delete_level.


    CHECK i_level_index IS NOT INITIAL.

    DELETE mt_levels INDEX i_level_index.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_regex_golf_inv_level_id.

    ENDIF.

    _save_levels( ).


  ENDMETHOD.


  METHOD get_current_level.


    r_current_level = m_current_level-id.


  ENDMETHOD.


  METHOD get_level_count.


    r_count = lines( mt_levels ).


  ENDMETHOD.


  METHOD get_matches.


    rt_matches = m_current_level-matches.


  ENDMETHOD.


  METHOD get_non_matches.


    rt_non_matches = m_current_level-non_matches.


  ENDMETHOD.


  METHOD random_level.


    m_current_level = mt_levels[ id = _get_random_level_id( ) ].


  ENDMETHOD.


  METHOD set_level.

    IF NOT line_exists( mt_levels[ id = i_new_level_id ] ).

      RAISE EXCEPTION TYPE zcx_regex_golf_inv_level_id.

    ENDIF.

    m_current_level = mt_levels[ id = i_new_level_id ].

  ENDMETHOD.


  METHOD _get_random_level_id.


    DATA(random) = cl_abap_random_int=>create( seed = CONV #( sy-uzeit )
                                               min  = 1
                                               max  = lines( mt_levels  ) ).

    DO.

      DATA(random_int) = random->get_next( ).
      DATA(new_level) = VALUE #( mt_levels[ random_int ] OPTIONAL ).

      IF new_level-id <> m_current_level-id
         OR lines( mt_levels ) <= 1.

        EXIT.

      ENDIF.

    ENDDO.

    r_random_level_id = new_level-id.


  ENDMETHOD.


  METHOD _load_levels.


    m_mime_repository_api->get(
      EXPORTING
        i_url                  = co_level_xml_file    " Object URL
        i_check_authority      = ' '    " X Check Authorization, '' No Authorization Check
      IMPORTING
        e_content              = DATA(content)    " Object Content
      EXCEPTIONS
        OTHERS                 = 1 ).

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_regex_golf_invalid_input
        EXPORTING
          i_syst_message = abap_true.

    ENDIF.

    CALL TRANSFORMATION id SOURCE XML content
                           RESULT levels = r_levels.


  ENDMETHOD.


  METHOD _save_levels.


    CALL TRANSFORMATION id SOURCE levels = mt_levels
                           RESULT XML DATA(content).

    content = NEW xml( )->pretty_print( content ).

    m_mime_repository_api->put(
      EXPORTING
        i_url     = co_level_xml_file    " Object URL
        i_content = content    " Object Contents (if exists -> overwrite contents)
      EXCEPTIONS
        OTHERS    = 1 ).

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_regex_golf_invalid_input
        EXPORTING
          i_syst_message = abap_true.

    ENDIF.


  ENDMETHOD.
ENDCLASS.

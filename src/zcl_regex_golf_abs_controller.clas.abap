CLASS zcl_regex_golf_abs_controller DEFINITION PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      zif_regex_golf_controller
        ABSTRACT METHODS
          delete_level
          get_current_level
          get_match_html
          get_non_match_html
          get_top_html
          pick_level
          random_level
          validate.

    METHODS:
      constructor
        RAISING
          zcx_regex_golf_error .

  PROTECTED SECTION.
    DATA:
      mo_level TYPE REF TO zcl_regex_golf_level .

ENDCLASS.



CLASS zcl_regex_golf_abs_controller IMPLEMENTATION.

  METHOD constructor.

    mo_level = NEW zcl_regex_golf_level( ).

  ENDMETHOD.


  METHOD zif_regex_golf_controller~add_level.

    DATA: matches     TYPE string,
          non_matches TYPE string,
          description TYPE string.

    cl_demo_input=>new(
                )->add_field( EXPORTING
                                text  = 'Description'
                              CHANGING
                                field = description
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

        DATA(new_level_id) = mo_level->add_level( i_description = description
                                                i_matches     = matches
                                                i_non_matches = non_matches ).

      CATCH zcx_regex_golf_error INTO DATA(error).

        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

    ENDTRY.

    MESSAGE |Level { new_level_id } created| TYPE 'S'.

  ENDMETHOD.

ENDCLASS.

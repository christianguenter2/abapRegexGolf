CLASS zcl_regex_golf_null_controller DEFINITION
  PUBLIC
  INHERITING FROM zcl_regex_golf_abs_controller
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      zif_regex_golf_controller~delete_level 			REDEFINITION,
      zif_regex_golf_controller~get_current_level	REDEFINITION,
      zif_regex_golf_controller~get_match_html 		REDEFINITION,
      zif_regex_golf_controller~get_non_match_html REDEFINITION,
      zif_regex_golf_controller~get_top_html 			REDEFINITION,
      zif_regex_golf_controller~pick_level 				REDEFINITION,
      zif_regex_golf_controller~random_level 			REDEFINITION,
      zif_regex_golf_controller~validate 					REDEFINITION .

ENDCLASS.



CLASS zcl_regex_golf_null_controller IMPLEMENTATION.


  METHOD zif_regex_golf_controller~delete_level.

  ENDMETHOD.


  METHOD zif_regex_golf_controller~get_current_level.

  ENDMETHOD.


  METHOD zif_regex_golf_controller~get_match_html.

  ENDMETHOD.


  METHOD zif_regex_golf_controller~get_non_match_html.

  ENDMETHOD.


  METHOD zif_regex_golf_controller~get_top_html.

  ENDMETHOD.


  METHOD zif_regex_golf_controller~pick_level.

  ENDMETHOD.


  METHOD zif_regex_golf_controller~random_level.

  ENDMETHOD.


  METHOD zif_regex_golf_controller~validate.

  ENDMETHOD.

ENDCLASS.

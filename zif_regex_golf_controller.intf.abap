INTERFACE zif_regex_golf_controller PUBLIC.

  METHODS:
    get_match_html
      RETURNING
        VALUE(rt_html) TYPE w3htmltab,

    get_non_match_html
      RETURNING
        VALUE(rt_html) TYPE w3htmltab,

    get_current_level
      RETURNING
        VALUE(r_current_level) TYPE zcl_regex_golf_level=>ty_level_id,

    random_level
      RAISING
        zcx_regex_golf_error,

    pick_level,

    add_level,

    get_top_html
      RETURNING
        VALUE(rt_html) TYPE w3htmltab,

    validate
      IMPORTING
        !i_regex TYPE csequence
      RAISING
        zcx_regex_golf_error,

    delete_level.

ENDINTERFACE.

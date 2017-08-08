CLASS zcx_regex_golf_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING
        !i_syst_message TYPE abap_bool OPTIONAL
        !i_msgid        TYPE symsgid DEFAULT sy-msgid
        !i_msgno        TYPE symsgno DEFAULT sy-msgno
        !i_msgv1        TYPE symsgv DEFAULT sy-msgv1
        !i_msgv2        TYPE symsgv DEFAULT sy-msgv2
        !i_msgv3        TYPE symsgv DEFAULT sy-msgv3
        !i_msgv4        TYPE symsgv DEFAULT sy-msgv4
        !previous       TYPE REF TO cx_root OPTIONAL,

      get_text
        REDEFINITION .

  PRIVATE SECTION.
    DATA: m_text TYPE string .

ENDCLASS.



CLASS ZCX_REGEX_GOLF_ERROR IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

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

    m_text = message.

  ENDMETHOD.


  METHOD get_text.

    result = COND #( WHEN m_text IS NOT INITIAL THEN m_text
                     ELSE super->get_text( ) ).

  ENDMETHOD.
ENDCLASS.

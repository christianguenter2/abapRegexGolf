*&---------------------------------------------------------------------
*& Report  z_test_abap_regex_golf
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT z_abap_regex_golf.

DATA: ok_code     TYPE sy-ucomm,
      regex_input TYPE zcl_regex_golf_controller=>ty_regex_string.

CLASS zcl_regex_golf_view DEFINITION.

  PUBLIC SECTION.

    INTERFACES:
      if_amc_message_receiver,
      if_amc_message_receiver_text .

    CLASS-METHODS:
      get
        RETURNING
          VALUE(r_instance) TYPE REF TO zcl_regex_golf_view
        RAISING
          zcx_regex_golf_error.

    METHODS:
      constructor
        RAISING
          zcx_regex_golf_error,

      pbo,

      pai.

  PRIVATE SECTION.

    CLASS-DATA:
      mo_instance TYPE REF TO zcl_regex_golf_view .

    DATA:
      mo_controller TYPE REF TO zif_regex_golf_controller,

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
      END OF co_custom_control_name .

    METHODS:
      _left_control
        RAISING
          zcx_regex_golf_error,

      _right_control
        RAISING
          zcx_regex_golf_error,

      _control_processing
        IMPORTING
          !i_container_name TYPE csequence
          !it_html          TYPE w3htmltab
        CHANGING
          !co_container     TYPE REF TO cl_gui_custom_container
          !co_control       TYPE REF TO cl_gui_html_viewer,

      _top_control,

      _dispatch_ok_code
        IMPORTING
          !i_ok_code TYPE sy-ucomm
        RAISING
          zcx_regex_golf_error,

      _paint_controls
        RAISING
          zcx_regex_golf_error,

      _initialize_amc_receiver .


ENDCLASS.

CLASS zcl_regex_golf_view IMPLEMENTATION.

  METHOD constructor.

    first_time = abap_true.

    TRY.
        mo_controller = NEW zcl_regex_golf_controller( REF #( regex_input ) ).

      CATCH zcx_regex_golf_error INTO DATA(error).

        mo_controller = NEW zcl_regex_golf_null_controller( ).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

    _initialize_amc_receiver( ).

  ENDMETHOD.


  METHOD get.

    r_instance = mo_instance = COND #( WHEN mo_instance IS BOUND THEN mo_instance
                                       ELSE NEW zcl_regex_golf_view( ) ).

  ENDMETHOD.


  METHOD if_amc_message_receiver_text~receive.

    " strip last character, because it's a * added by the search help
    DATA(message) = COND #( LET len = strlen( i_message ) IN
                            WHEN len > 0
                            THEN substring( val = i_message
                                            off = 0
                                            len = len - 1 ) ).

    TRY.
        mo_controller->validate( message ).
        _paint_controls( ).

      CATCH zcx_regex_golf_error INTO DATA(error).

        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

  ENDMETHOD.


  METHOD pai.

    DATA(l_ok_code) = ok_code.

    CLEAR ok_code.

    TRY.
        _dispatch_ok_code( l_ok_code ).

      CATCH zcx_regex_golf_error INTO DATA(error).

        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

  ENDMETHOD.


  METHOD pbo.

    DATA(current_level) = |{ mo_controller->get_current_level( ) }|.

    SET:
      PF-STATUS 'MAIN_0100',
      TITLEBAR 'MAIN_0100' WITH current_level.

    TRY.

        _paint_controls( ).

      CATCH zcx_regex_golf_error INTO DATA(error).

        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

    cl_dsh_dynpro_properties=>enable_type_ahead( VALUE #( ( `REGEX_INPUT` ) ) ).

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

        mo_controller->validate( regex_input ).

      WHEN co_ok_code-pick.

        mo_controller->pick_level( ).

      WHEN co_ok_code-rand.

        mo_controller->random_level( ).

      WHEN co_ok_code-add.

        mo_controller->add_level( ).

      WHEN co_ok_code-delete.

        mo_controller->delete_level( ).

    ENDCASE.

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


  METHOD _left_control.

    _control_processing(
      EXPORTING
        i_container_name = co_custom_control_name-left
        it_html          = mo_controller->get_match_html( )
      CHANGING
        co_container = _custom_container-left
        co_control   = _custom_control-left ).

  ENDMETHOD.


  METHOD _paint_controls.

    _top_control( ).
    _left_control( ).
    _right_control( ).

  ENDMETHOD.


  METHOD _right_control.

    _control_processing(
      EXPORTING
        i_container_name = co_custom_control_name-right
        it_html          = mo_controller->get_non_match_html( )
      CHANGING
        co_container = _custom_container-right
        co_control   = _custom_control-right ).

  ENDMETHOD.

  METHOD _top_control.

    _control_processing(
      EXPORTING
        i_container_name = co_custom_control_name-top
        it_html          = mo_controller->get_top_html( )
      CHANGING
        co_container = _custom_container-top
        co_control   = _custom_control-top ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  CALL SCREEN 0100.

MODULE status_0100 OUTPUT.

  zcl_regex_golf_view=>get( )->pbo( ).

ENDMODULE.

MODULE user_command_0100 INPUT.

  zcl_regex_golf_view=>get( )->pai( ).

ENDMODULE.

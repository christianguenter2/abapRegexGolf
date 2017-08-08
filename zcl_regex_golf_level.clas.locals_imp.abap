*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS xml DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor,
      pretty_print
        IMPORTING
          i_xml        TYPE xstring
        RETURNING
          VALUE(r_xml) TYPE xstring.

  PRIVATE SECTION.
    DATA: mi_ixml    TYPE REF TO if_ixml,
          mi_xml_doc TYPE REF TO if_ixml_document.

ENDCLASS.

CLASS xml IMPLEMENTATION.

  METHOD constructor.
    mi_ixml = cl_ixml=>create( ).
    mi_xml_doc = mi_ixml->create_document( ).
  ENDMETHOD.

  METHOD pretty_print.

    DATA: li_ostream        TYPE REF TO if_ixml_ostream,
          li_renderer       TYPE REF TO if_ixml_renderer,
          li_istream        TYPE REF TO if_ixml_istream,
          li_element        TYPE REF TO if_ixml_element,
          li_version        TYPE REF TO if_ixml_node,
          li_parser         TYPE REF TO if_ixml_parser,
          li_stream_factory TYPE REF TO if_ixml_stream_factory.

    li_stream_factory = mi_ixml->create_stream_factory( ).

    li_istream = li_stream_factory->create_istream_xstring( i_xml ).

    li_parser = mi_ixml->create_parser( stream_factory = li_stream_factory
                                        istream        = li_istream
                                        document       = mi_xml_doc ).
    li_parser->set_normalizing( abap_true  ).
    li_parser->parse( ).

    li_istream->close( ).

    li_ostream = li_stream_factory->create_ostream_xstring( r_xml ).

    li_renderer = mi_ixml->create_renderer( ostream  = li_ostream
                                            document = mi_xml_doc ).
    li_renderer->set_normalizing( abap_true  ).

    li_renderer->render( ).

  ENDMETHOD.

ENDCLASS.

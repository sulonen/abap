*&---------------------------------------------------------------------*
*& Report  Z_Z857_CHECK
*&
*& Utility for finding unprocessed Z857 (Drop Ship Invoices)
*&
*& This tool actually just checks the processing status of a message -
*& if the document (nast-objky) hasn't been processed or has been
*& incorrectly processed (if the processing status <> 1), it'll be
*& displayed in an ALV grid.
*&---------------------------------------------------------------------*
*& Created: kps 6/13/14
*&
*&---------------------------------------------------------------------*

report z_z857_check.

tables nast.

parameters p_part type nast-parnr.

select-options s_dates for nast-erdat.

*----------------------------------------------------------------------*
*       CLASS lcl_messages DEFINITION
*----------------------------------------------------------------------*
class lcl_messages definition.
  public section.

    class-methods: main.

  private section.

    types: begin of ty_message,
              message_type type nast-kschl,
              partner      type nast-parnr,
              document     type nast-objky,
              message_date type nast-erdat,
              user         type nast-usnam,
           end of ty_message.

    data: t_messages  type standard table of ty_message,
          l_functions type ref to cl_salv_functions_list,
          l_columns   type ref to cl_salv_columns_table,
          l_oref      type ref to cx_root,
          l_text      type string,
          l_table     type ref to cl_salv_table.

    methods: get_data,
             display_alv.

endclass.                    "lcl_invoices DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_messages IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_messages implementation.

  method get_data.
    select kschl parnr objky erdat usnam from nast
      into table t_messages
     where kschl = 'Z857'
       and parnr = p_part
       and erdat in s_dates
       and vstat <> 1.
  endmethod.                    "get_data

  method display_alv.
* if program is running in background, don't generate alv grid
    if sy-batch = abap_true.
      if t_messages[] is initial.
        exit.
      endif.
    endif.

    clear l_text.
    try.
        cl_salv_table=>factory(
          importing
            r_salv_table = l_table
          changing
            t_table      = t_messages ).
      catch cx_salv_msg into l_oref.
        l_text = l_oref->get_text( ).
    endtry.

    l_functions = l_table->get_functions( ).
    l_functions->set_default( abap_true ).
    l_columns = l_table->get_columns( ).
    l_columns->set_optimize( abap_true ).

    l_table->display( ).

  endmethod.                    "display_alv

  method main.
    data o_messages type ref to lcl_messages.
    create object o_messages.
    o_messages->get_data( ).
    o_messages->display_alv( ).
  endmethod.                    "main

endclass.                    "lcl_invoices IMPLEMENTATION

start-of-selection.

  lcl_messages=>main( ).
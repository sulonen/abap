*&-------------------------------------------------------------------*
*& Report ZB2BSTARTCONTENT
*&
*& Utility for maintaining table ZB2BDEFSTARTCONT
*&-------------------------------------------------------------------*
*& Created: kevins 3/19/13
*&
*&-------------------------------------------------------------------*

report zb2bstartcontent.

*--------------------------------------------------------------------*
* Class lcl_content definition
*--------------------------------------------------------------------*
class lcl_content definition.
  public section.

    data: t_content type standard table of zb2bdefstartcont,
          g_wa_content type zb2bdefstartcont.

    methods:
      get_data,
      display_alv,
      refresh_alv,
      event_handler importing im_action type string,
      update_table importing im_action type string.

  private section.
    data:
      l_functions type ref to cl_salv_functions_list,
      l_columns   type ref to cl_salv_columns_table,
      l_one_col   type ref to cl_salv_column_table,
      l_display   type ref to cl_salv_display_settings,
      l_col_tab   type salv_t_column_ref,
      l_sort      type ref to cl_salv_sorts,
      l_key       type salv_s_layout_key,
      l_layout    type ref to cl_salv_layout,
      l_oref      type ref to cx_root,
      l_text      type string,
      l_ltext     type scrtext_l,
      l_mtext     type scrtext_m,
      l_stext     type scrtext_s,
      l_mhtml     type xstring,
      l_stream    type etxml_xline_tabtype,
      l_select    type ref to cl_salv_selections,
      l_size      type i,
      l_table     type ref to cl_salv_table,
      o_container type ref to cl_gui_custom_container,
      wa_rows     type int4.

endclass.                    "lcl_content DEFINITION

*--------------------------------------------------------------------*
* Class lcl_content implementation
*--------------------------------------------------------------------*
class lcl_content implementation.

  method get_data.

    select * from zb2bdefstartcont
      into table t_content.

  endmethod.                    "get_data

  method display_alv.

    field-symbols: <l_wa_col> type salv_s_column_ref.

    create object o_container
      exporting
        container_name              = 'ALV_DISPLAY'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    clear l_text.
    try.
        cl_salv_table=>factory(
          exporting
            r_container = o_container
          importing
            r_salv_table = l_table
          changing
            t_table      = t_content ).
      catch cx_salv_msg into l_oref.
        l_text = l_oref->get_text( ).
    endtry.

    l_functions = l_table->get_functions( ).
    l_functions->set_all( ).
    l_columns = l_table->get_columns( ).
    l_columns->set_optimize( if_salv_c_bool_sap=>true ).
    l_col_tab = l_columns->get( ).
    l_display = l_table->get_display_settings( ).
    l_display->set_striped_pattern( if_salv_c_bool_sap=>true ).

    l_layout = l_table->get_layout( ).
    l_key-report = sy-repid.
    l_layout->set_key( l_key ).
    l_layout->set_default( abap_true ).
    l_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
    l_select = l_table->get_selections( ).
    l_select->set_selection_mode( if_salv_c_selection_mode=>single ).

    l_sort = l_table->get_sorts( ).
    try.
        l_sort->add_sort( columnname = 'WEB_SHOP'
                            sequence = if_salv_c_sort=>sort_up ).
      catch cx_salv_data_error into l_oref.
      catch cx_salv_existing into l_oref.
      catch cx_salv_not_found into l_oref.
    endtry.

    try.
        l_sort->add_sort( columnname = 'CAMPAIGN'
                            sequence = if_salv_c_sort=>sort_up ).
      catch cx_salv_data_error into l_oref.
      catch cx_salv_existing into l_oref.
      catch cx_salv_not_found into l_oref.
    endtry.

    l_table->display( ).

    call screen 100.

  endmethod.                    "display

  method refresh_alv.
    o_container->free( ).
    me->get_data( ).
    me->display_alv( ).

    call screen 100.
  endmethod.                    "refresh_alv

  method event_handler.
    data: l_select type ref to cl_salv_selections,
      l_rows type salv_t_row.

    if im_action = 'MODIFY'.
      l_table->get_metadata( ).
      l_select = l_table->get_selections( ).
      l_rows = l_select->get_selected_rows( ).

      clear wa_rows.
      read table l_rows into wa_rows index 1.
      if sy-subrc = 0.
        clear g_wa_content.
        read table t_content into g_wa_content index wa_rows.
        call screen 200.
      else.
        message 'Please select a row.' type 'I'.
      endif.

    elseif im_action = 'COPY'.
      l_table->get_metadata( ).
      l_select = l_table->get_selections( ).
      l_rows = l_select->get_selected_rows( ).

      clear wa_rows.
      read table l_rows into wa_rows index 1.
      if sy-subrc = 0.
        clear g_wa_content.
        read table t_content into g_wa_content index wa_rows.
        call screen 200.
      else.
        message 'Please select a row.' type 'I'.
      endif.

    elseif im_action = 'ADD'.
      clear wa_rows.
      clear g_wa_content.
      call screen 200.

    elseif im_action = 'DELETE'.
      l_table->get_metadata( ).
      l_select = l_table->get_selections( ).
      l_rows = l_select->get_selected_rows( ).

      clear wa_rows.
      read table l_rows into wa_rows index 1.
      if sy-subrc = 0.
        clear g_wa_content.
        read table t_content into g_wa_content index wa_rows.
        call screen 200.
      else.
        message 'Please select a row.' type 'I'.
      endif.
    endif.
  endmethod.                    "event_handler

  method update_table.
    if im_action = 'MODIFY'.
      modify t_content index wa_rows from g_wa_content.
      update zb2bdefstartcont from table t_content.
      commit work and wait.
      message 'Record saved successfully.' type 'S'.

    elseif im_action = 'COPY'.
      g_wa_content-client = 100.
      read table t_content with key client         = g_wa_content-client
                                    web_shop       = g_wa_content-web_shop
                                    langu          = g_wa_content-langu
                                    campaign       = g_wa_content-campaign
                                    startpage_slot = g_wa_content-startpage_slot transporting no fields.
      if sy-subrc = 0.
        message 'A record with these key fields already exists.' type 'S'.
        me->event_handler( im_action = 'COPY' ).
      else.
        modify zb2bdefstartcont from g_wa_content.
        commit work and wait.
        message 'Record saved successfully.' type 'S'.
      endif.

    elseif im_action = 'ADD'.
      g_wa_content-client = 100.
      read table t_content with key client         = g_wa_content-client
                                    web_shop       = g_wa_content-web_shop
                                    langu          = g_wa_content-langu
                                    campaign       = g_wa_content-campaign
                                    startpage_slot = g_wa_content-startpage_slot transporting no fields.
      if sy-subrc = 0.
        message 'A record with these key fields already exists.' type 'S'.
        me->event_handler( im_action = 'ADD' ).
      else.
        modify zb2bdefstartcont from g_wa_content.
        commit work and wait.
        message 'Record saved successfully.' type 'S'.
      endif.

    elseif im_action = 'DELETE'.
      delete zb2bdefstartcont from g_wa_content.
      message 'Record deleted.' type 'S'.
    endif.

  endmethod.                    "update_table

endclass.                    "lcl_tcontent IMPLEMENTATION

data: o_content type ref to lcl_content,
      g_action type string.

start-of-selection.
  create object o_content.
  o_content->get_data( ).
  o_content->display_alv( ).

*&---------------------------------------------------------------------*
*& Module  initialize_100  OUTPUT
*&---------------------------------------------------------------------*
module initialize_100 output.
  set pf-status '100'.
  set titlebar '100'.
endmodule.                 " initialize_100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INITIALIZE_200  OUTPUT
*&---------------------------------------------------------------------*
module initialize_200 output.
  set pf-status '200'.
  set titlebar '100'.
  loop at screen.
    if screen-name = 'DELETION_ALERT'.
      screen-invisible = '0'.
    endif.
  endloop.
endmodule.                 " INITIALIZE_200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  MODIFY_200  OUTPUT
*&---------------------------------------------------------------------*
module modify_200 output.
  if g_action = 'MODIFY'.
    loop at screen.
      if screen-group1 = 'ADD'.
        screen-input = 0.
      endif.
      if screen-name = 'DELETION_ALERT'.
        screen-active = 0.
        screen-invisible = 0.
      endif.
      if screen-group3 = 'KEY'.
        screen-active = 0.
        screen-invisible = 0.
        screen-intensified = 0.
      endif.
      modify screen.
    endloop.
  elseif g_action = 'COPY'.
    loop at screen.
      if screen-group1 = 'ADD'.
        screen-input = 1.
      endif.
      if screen-name = 'DELETION_ALERT'.
        screen-active = 0.
        screen-invisible = 0.
      endif.
      if screen-group3 = 'KEY'.
        screen-active = 1.
        screen-invisible = 0.
        screen-intensified = 1.
      endif.
      modify screen.
    endloop.
  elseif g_action = 'ADD'.
    loop at screen.
      if screen-group1 = 'ADD'.
        screen-input = 1.
      endif.
      if screen-name = 'DELETION_ALERT'.
        screen-active = 0.
        screen-invisible = 0.
      endif.
      if screen-group3 = 'KEY'.
        screen-active = 1.
        screen-invisible = 0.
        screen-intensified = 1.
      endif.
      modify screen.
    endloop.
  elseif g_action = 'DELETE'.
    loop at screen.
      if screen-group2 = 'DEL'.
        screen-input = 0.
      endif.
      if screen-name = 'DELETION_ALERT'.
        screen-active = 1.
        screen-invisible = 0.
      endif.
      if screen-group3 = 'KEY'.
        screen-active = 0.
        screen-invisible = 0.
        screen-intensified = 0.
      endif.
      modify screen.
    endloop.

  endif.
endmodule.                 " MODIFY_200  OUTPUT

*&---------------------------------------------------------------------*
*& Module  user_command_100  INPUT
*&---------------------------------------------------------------------*
module user_command_100 input.
  data: ok_code  type sy-ucomm,
        save_ok  type sy-ucomm,
        output   type sy-ucomm.

  ok_code = sy-ucomm.
  save_ok = ok_code.
  clear ok_code.
  case save_ok.
    when '&F03' or '&F15' or '&F12'.
      leave program.
    when 'MODIFY'.
      clear g_action.
      g_action = 'MODIFY'.
      o_content->event_handler( im_action = g_action ).
    when 'COPY'.
      clear g_action.
      g_action = 'COPY'.
      o_content->event_handler( im_action = g_action ).
    when 'ADD'.
      clear g_action.
      g_action = 'ADD'.
      o_content->event_handler( im_action = g_action ).
    when 'DELETE'.
      clear g_action.
      g_action = 'DELETE'.
      o_content->event_handler( im_action = g_action ).
    when others.
      output = save_ok.
  endcase.
endmodule.                 " user_command_100  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_200  INPUT
*&---------------------------------------------------------------------*
module user_command_200 input.
  data l_answer type string.
  ok_code = sy-ucomm.
  save_ok = ok_code.
  clear ok_code.
  case save_ok.
    when '&F15' or '&F12'.
      leave program.

    when '&F03'.
      if sy-datar = abap_true.
        call function 'POPUP_TO_CONFIRM'
          exporting
            titlebar              = 'Data has been changed'
            text_question         = 'Data has been changed. Do you want to save?'
            text_button_1         = 'Yes'(001)
            text_button_2         = 'No'(002)
            default_button        = '1'
            display_cancel_button = 'X'
          importing
            answer                = l_answer
          exceptions
            text_not_found        = 1
            others                = 2.
        case l_answer.
          when '1'.
            g_action = 'MODIFY'.
            o_content->update_table( g_action ).
            o_content->refresh_alv( ).
          when '2'.
            o_content->refresh_alv( ).
          when 'A'.
            message 'Operation canceled by user.' type 'S'.
        endcase.
      else.
        o_content->refresh_alv( ).
      endif.

    when 'SAVE'.
      o_content->update_table( g_action ).
      o_content->refresh_alv( ).
    when others.
      output = save_ok.
  endcase.
endmodule.                 " user_command_200  INPUT
*----------------------------------------------------------------------*
* Report  Z_B2C_ORDER_EXTRACT
*
* Utility to extract b2C web orders for use by external vendor.
*
*----------------------------------------------------------------------*
* Created: kps 10/1/2014
*
*----------------------------------------------------------------------*

report  z_b2c_order_extract.

class lcl_exception definition deferred.
class lcl_extract   definition deferred.

*----------------------------------------------------------------------*
*       Tables
*----------------------------------------------------------------------*
tables: vbak.

*----------------------------------------------------------------------*
*       Selection Screen
*----------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-001.
select-options: s_audat for vbak-audat, "Order Date
                s_erzet for vbak-erzet, "Entry Time
                s_auart for vbak-auart, "Document Type
                s_kunnr for vbak-kunnr. "Sold To
selection-screen skip.
selection-screen begin of block b2 with frame title text-002.
parameters: p_alv   radiobutton group g1 default 'X',
            p_ofile radiobutton group g1,
            p_file  type localfile default 'c:\temp\'.
selection-screen end of block b2.
selection-screen end of block b1.

*----------------------------------------------------------------------*
*       Variables
*----------------------------------------------------------------------*
data: o_extract   type ref to lcl_extract,
      o_exception type ref to lcl_exception,
      l_message   type bapi_msg.

*----------------------------------------------------------------------*
*       CLASS lcl_exception DEFINITION
*----------------------------------------------------------------------*
class lcl_exception definition
  inheriting from cx_dynamic_check.
  public section.
    methods:
      constructor importing
                      im_textid   like textid optional
                      im_previous like previous optional
                      im_message  type bapi_msg optional,

      get_message returning value(re_message) type bapi_msg.

  private section.
    data: g_message type bapi_msg.
endclass.                    "lcl_exception DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_exception IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_exception implementation.

  method constructor.
    super->constructor( textid   = im_textid
                        previous = im_previous ).
    clear g_message.
    g_message = im_message.
  endmethod.                    "constructor

  method get_message.
    re_message = g_message.
  endmethod.                    "get_messtype

endclass.                    "lcl_exception IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_extract DEFINITION
*----------------------------------------------------------------------*
class lcl_extract definition create private final.
  public section.
    class-methods:
      "Static factory method that returns a pointer to itself.
      main
        returning value(re_obj) type ref to lcl_extract
        raising lcl_exception,

    get_file_name
      returning value(re_file) type localfile,

    set_file_name
      returning value(re_file) type localfile.

    methods:
      execute
        raising lcl_exception.

  private section.
    constants: c_delimiter type abap_char1 value '|'.

    types: begin of ty_data,
             order_type     type auart,                 "Order Type
             customer       type kunnr,                 "Customer
             sales_order    type vbeln_va,              "Order Number
             doc_condition  type knumv,                 "Document Condition
             order_item     type posnr_va,              "Order Item
             material       type matnr,                 "Material
             purchase_order type bstnk,                 "Purchase Order
             order_date     type erdat,                 "Order Date
             order_time     type erzet,                 "Order Time
             price_each     type kzwi3,                 "Subtotal 3 from pricing procedure for condition
             quantity       type kwmeng,                "Quantity
             address_key    type adrnr,                 "Address Key
             title_key      type ad_title,              "Title Key
             name           type ad_name1,              "Name
             street1        type ad_street,             "Street 1
             street2        type ad_strspp1,            "Street 2
             city           type ad_city1,              "City
             state          type regio,                 "State
             postal_code    type ad_pstcd1,             "Postal Code
             country        type land1,                 "Country
             phone          type ad_tlnmbr1,            "Phone
             email          type ad_smtpadr,            "Email
           end of ty_data,

           begin of ty_output,
             customer       type char10,                "Customer
             sales_order    type char10,                "Order Number
             material       type char18,                "Material
             purchase_order type char20,                "Purchase Order
             order_date     type char20,                "Order Date
             price_each     type p length 8 decimals 2, "Price Each
             quantity       type p length 5 decimals 0, "Quantity
             title_text     type char4,                 "Title
             first_name     type char40,                "First Name
             last_name      type char40,                "Last Name
             street1        type char50,                "Street 1
             street2        type char40,                "Street 2
             city           type char40,                "City
             state          type char3,                 "State
             postal_code    type char10,                "Postal Code
             country        type char3,                 "Country
             phone          type char30,                "Phone
             freight        type p length 8 decimals 2, "Freight
             email          type char200,               "Email
             gift_message   type char1,                 "Gift Message Y/N
           end of ty_output,

           begin of ty_download,
             data type string,
           end of ty_download,

           ty_tab_download type standard table of ty_download,
           ty_tab_output   type standard table of ty_output,
           ty_tab_display  type standard table of ty_output.

    data: t_data        type standard table of ty_data,
          wa_data       type ty_data,
          t_output      type standard table of ty_output,
          wa_output     type ty_output,
          t_download    type standard table of ty_download,
          wa_download   type ty_download,
          l_format     type string,
          l_field_count type i.

    methods:
      constructor
        raising lcl_exception,

      get_data
        raising lcl_exception,

      display
        changing ch_output type ty_tab_display
          raising lcl_exception,

      write_file
        importing im_output type ty_tab_output
          raising lcl_exception,

      format_data
        importing im_data type data
        exporting ex_data type data,

      write_unix_file
        changing ch_table type ty_tab_download
          raising lcl_exception,

      write_pc_file
        changing ch_table type ty_tab_download
          raising lcl_exception.

endclass.                    "lcl_extract DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_extract IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_extract implementation.

  method constructor.
  endmethod.                    "constructor

  method execute.
    if p_alv = abap_true.
* Output to ALV
      get_data( ).
      display( changing ch_output = t_output ).
    else.
* Write file
      get_data( ).
      write_file( exporting im_output = t_output ).
    endif.
  endmethod.                    "execute

  method get_data.
    data: l_freight  type p length 8 decimals 2,
          l_discount type p length 8 decimals 2,
          l_date     type char10,
          l_time     type char10,
          l_title    type ad_titletx,
          l_name     type tdobname,
          l_gift     type char1,
          t_lines    type standard table of tline.

    clear: t_data[],
           t_output[],
           l_name.

* Primary select
    ##too_many_itab_fields
    select h~auart      "Order Type
           h~kunnr      "Customer
           h~vbeln      "Sales Order
           h~knumv      "Document Condition
           l~posnr      "Order Item
           l~matnr      "Material
           h~bstnk      "Purchase Order
           h~erdat      "Order Date
           h~erzet      "Order Time
           l~kzwi3      "Subtotal 3 from pricing procedure for condition
           l~kwmeng     "Quantity
           p~adrnr      "Address Key
           a~title      "Title Key
           a~name1      "First Name
           a~street     "Street 1
           a~str_suppl1 "Street 2
           a~city1      "City
           a~region     "State
           a~post_code1 "Postal Code
           a~country    "Country
           a~tel_number "Phone
           e~smtp_addr  "Email
      into table t_data
      from vbak as h               "Sales Document: Header Data
      join vbap as l               "Sales Document: Item Data
        on h~vbeln = l~vbeln
      join vbpa as p               "Sales Document: Partner
        on p~vbeln = h~vbeln
       and p~parvw = 'RE'
      join adrc as a               "Addresses
        on p~adrnr = a~addrnumber
      join adr6 as e               "E-Mail Addresses
        on p~adrnr = e~addrnumber
* We have an index on VBAK-AUDAT - this select should be less costly
      where h~audat in s_audat.

* Reduce the result set on user selections and transfer to output table
    clear wa_data.
    loop at t_data into wa_data.
      if wa_data-order_time in s_erzet and
         wa_data-order_type in s_auart and
         wa_data-customer   in s_kunnr.

* Assume no gift message
        l_gift = 'N'.

* Get freight
        clear: l_freight,
               l_discount.

        select single kwert from konv into l_freight
          where knumv = wa_data-doc_condition
            and kposn = wa_data-order_item
            and kschl = 'ZSHF'.
        select single kwert from konv into l_discount
         where knumv = wa_data-doc_condition
           and kposn = wa_data-order_item
           and kschl = 'ZSHD'.

* Populate wa_output from wa_data
        clear wa_output.
        wa_output-customer       = wa_data-customer.
        shift wa_output-customer left deleting leading '0'.
        wa_output-sales_order    = wa_data-sales_order.
        shift wa_output-sales_order left deleting leading '0'.
        wa_output-material       = wa_data-material.
        shift wa_output-material left deleting leading '0'.
        wa_output-purchase_order = wa_data-purchase_order.
        write wa_data-order_date to l_date.
        write wa_data-order_time to l_time.
        concatenate l_date
                    l_time
               into wa_output-order_date separated by space.
        if wa_data-quantity is initial.
          wa_output-price_each   = wa_data-price_each.
        else.
          wa_output-price_each   = wa_data-price_each / wa_data-quantity.
        endif.
        wa_output-quantity       = wa_data-quantity.

* Get title text
        select single title_medi from tsad3t
                 into wa_output-title_text
                where title = wa_data-title_key
                  and langu = 'E'.

* If Sold To is a company, do not split wa_data-name
        if wa_data-title_key <> '0003'.
          split wa_data-name
             at space
           into wa_output-first_name
                wa_output-last_name.
        else.
          wa_output-first_name   = wa_data-name.
        endif.

        wa_output-street1        = wa_data-street1.
        wa_output-street2        = wa_data-street2.
        wa_output-city           = wa_data-city.
        wa_output-state          = wa_data-state.
        wa_output-postal_code    = wa_data-postal_code.
        wa_output-country        = wa_data-country.
        wa_output-phone          = wa_data-phone.
        wa_output-freight        = l_freight + l_discount.
        wa_output-email          = wa_data-email.

* Get name for function READ_TEXT
        l_name = wa_data-sales_order.

* Check for gift message at header
        call function 'READ_TEXT'
          exporting
            id                      = '1000'
            language                = 'E'
            name                    = l_name
            object                  = 'VBBK'
            archive_handle          = 0
          tables
            lines                   = t_lines
          exceptions
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7.
        if sy-subrc = 0.
          l_gift = 'Y'.
        endif.

* Get name for function READ_TEXT
        concatenate wa_data-sales_order wa_data-order_item into l_name.

* Check for gift message at line
        call function 'READ_TEXT'
          exporting
            id                      = '1000'
            language                = 'E'
            name                    = l_name
            object                  = 'VBBP'
            archive_handle          = 0
          tables
            lines                   = t_lines
          exceptions
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7.
        if sy-subrc = 0.
          l_gift = 'Y'.
        endif.
        wa_output-gift_message = l_gift.

* Add populated line to output table
        append wa_output to t_output.
      endif.
    endloop.
  endmethod.                    "get_data

  method write_file.
    field-symbols: <f> type any.

* Determine the number of fields in the output, for later use.
    clear l_field_count.
    do.
      assign component sy-index of structure wa_output to <f>.
      if sy-subrc <> 0. exit. endif.
      l_field_count = l_field_count + 1.
    enddo.

    loop at t_output into wa_output.
      clear: l_field_count,
             wa_download.
      do.
        assign component sy-index of structure wa_output to <f>.
        if sy-subrc <> 0. exit. endif.
        clear: l_format.
        format_data( exporting im_data = <f>
                     importing ex_data = l_format ).
        assign l_format to <f>.
        if sy-index = l_field_count.
          concatenate wa_download-data <f> into wa_download-data.
        else.
          concatenate wa_download-data <f> c_delimiter into wa_download-data.
        endif.
      enddo.
      append wa_download to t_download.
      clear wa_download.
    endloop.

    data: l_result type match_result.
    clear: l_result.
    find first occurrence of '/' in p_file results l_result.
    if l_result is initial.
      write_pc_file( changing ch_table = t_download ).
    else.
      write_unix_file( changing ch_table = t_download ).
    endif.
    write: 'File written successfuly to directory: ',
          / p_file.
  endmethod.                    "write_file

  method format_data.
    data: l_descr type ref to cl_abap_typedescr,
          l_cdate type char10,
          l_cpack type char20.
    l_descr = cl_abap_typedescr=>describe_by_data( im_data ).
    if l_descr->type_kind = l_descr->typekind_date.
      clear l_cdate.
      if im_data <> '00000000'.
        write im_data to l_cdate mm/dd/yyyy.
      endif.
      ex_data = l_cdate.
    elseif l_descr->type_kind = l_descr->typekind_packed.
      write im_data to l_cpack decimals 2.
      condense l_cpack.
      ex_data = l_cpack.
    else.
      ex_data = im_data.
    endif.
  endmethod.                    "format_data

  method write_pc_file.
    data: l_filename  type  string,
          l_format    type char10,
          l_size      type i.
    if p_file is initial. return. endif.
    l_filename = p_file.
    l_format   = 'ASC'.

    cl_gui_frontend_services=>gui_download(
      exporting
        bin_filesize            = l_size
        filename                = l_filename
        filetype                = l_format
      changing
        data_tab                = ch_table
      exceptions
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        others                  = 22 ).
    if sy-subrc <> 0.
      raise exception type lcl_exception
        exporting
          im_message = 'File Download Failed.'.
    endif.
  endmethod.                    "write_pc_file

  method write_unix_file.
    data: l_out type string,
          l_tmp type string.
    field-symbols: <tab>  type standard table,
                   <l_wa> type any,
                   <fld>  type any.

    assign ch_table to <tab>.

    open dataset p_file in text mode for output encoding default.

    if sy-subrc <> 0.
      raise exception type lcl_exception
        exporting
          im_message = 'File Download Failed.'.
    endif.
    loop at <tab> assigning <l_wa>.
      clear l_out.
      do.
        assign component sy-index of structure <l_wa> to <fld>.
        if sy-subrc <> 0. exit. endif.
        clear l_tmp.
        l_tmp = <fld>.
        concatenate l_out l_tmp into l_out.
      enddo.
      transfer l_out to p_file.
      if sy-subrc <> 0.
        raise exception type lcl_exception
          exporting
            im_message = 'File Download Failed.'.
      endif.
    endloop.
    close dataset p_file.
  endmethod.                    "write_unix_file

  method display.
    data: l_functions type ref to cl_salv_functions_list,
          l_columns   type ref to cl_salv_columns_table,
          l_column    type ref to cl_salv_column,
          l_one_col   type ref to cl_salv_column_table,
          l_display   type ref to cl_salv_display_settings,
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
          l_table     type ref to cl_salv_table.
    field-symbols: <l_wa_col> type salv_s_column_ref.

* Create ALV grid
    clear l_text.
    try.
        cl_salv_table=>factory(
          importing
            r_salv_table = l_table
          changing
            t_table      = ch_output ).
      catch cx_salv_msg into l_oref.
        l_text = l_oref->get_text( ).
    endtry.

    l_functions = l_table->get_functions( ).
    l_functions->set_all( ).

    l_columns = l_table->get_columns( ).
    l_columns->set_optimize( if_salv_c_bool_sap=>true ).

* Set column headings
    clear l_text.
    try.
        l_column = l_columns->get_column('CUSTOMER').
        l_column->set_long_text('Customer').
        l_column->set_medium_text('Customer').
        l_column->set_short_text('Cust').

        l_column = l_columns->get_column('SALES_ORDER').
        l_column->set_long_text('Sales Order').
        l_column->set_medium_text('SO').
        l_column->set_short_text('PO').

        l_column = l_columns->get_column('MATERIAL').
        l_column->set_long_text('Material Number').
        l_column->set_medium_text('Material').
        l_column->set_short_text('Mat').

        l_column = l_columns->get_column('PURCHASE_ORDER').
        l_column->set_long_text('Purchase Order').
        l_column->set_medium_text('PO').
        l_column->set_short_text('PO').

        l_column = l_columns->get_column('ORDER_DATE').
        l_column->set_long_text('Order Date').
        l_column->set_medium_text('Ord Dt').
        l_column->set_short_text('ODt').

        l_column = l_columns->get_column('PRICE_EACH').
        l_column->set_long_text('Price Each').
        l_column->set_medium_text('Prc Ea').
        l_column->set_short_text('PrEa').

        l_column = l_columns->get_column('QUANTITY').
        l_column->set_long_text('Quantity').
        l_column->set_medium_text('Qty').
        l_column->set_short_text('Qty').

        l_column = l_columns->get_column('TITLE_TEXT').
        l_column->set_long_text('Title').
        l_column->set_medium_text('Title').
        l_column->set_short_text('Title').

        l_column = l_columns->get_column('FIRST_NAME').
        l_column->set_long_text('First Name').
        l_column->set_medium_text('First').
        l_column->set_short_text('First').

        l_column = l_columns->get_column('LAST_NAME').
        l_column->set_long_text('Last Name').
        l_column->set_medium_text('Last').
        l_column->set_short_text('Last').

        l_column = l_columns->get_column('STREET1').
        l_column->set_long_text('Street 1').
        l_column->set_medium_text('Strt 1').
        l_column->set_short_text('Str1').

        l_column = l_columns->get_column('STREET2').
        l_column->set_long_text('Street 2').
        l_column->set_medium_text('Strt 2').
        l_column->set_short_text('Str2').

        l_column = l_columns->get_column('CITY').
        l_column->set_long_text('City').
        l_column->set_medium_text('City').
        l_column->set_short_text('City').

        l_column = l_columns->get_column('STATE').
        l_column->set_long_text('State').
        l_column->set_medium_text('State').
        l_column->set_short_text('State').

        l_column = l_columns->get_column('POSTAL_CODE').
        l_column->set_long_text('ZIP/Postal Code').
        l_column->set_medium_text('ZIP').
        l_column->set_short_text('ZIP').

        l_column = l_columns->get_column('COUNTRY').
        l_column->set_long_text('Country').
        l_column->set_medium_text('Country').
        l_column->set_short_text('Cntry').

        l_column = l_columns->get_column('COUNTRY').
        l_column->set_long_text('Country').
        l_column->set_medium_text('Country').
        l_column->set_short_text('Cntry').

        l_column = l_columns->get_column('PHONE').
        l_column->set_long_text('Telephone').
        l_column->set_medium_text('Phone').
        l_column->set_short_text('Ph').

        l_column = l_columns->get_column('FREIGHT').
        l_column->set_long_text('Freight').
        l_column->set_medium_text('Frt').
        l_column->set_short_text('Frt').

        l_column = l_columns->get_column('EMAIL').
        l_column->set_long_text('Email Address').
        l_column->set_medium_text('Email').
        l_column->set_short_text('Email').

        l_column = l_columns->get_column('GIFT_MESSAGE').
        l_column->set_long_text('Gift Message Y/N').
        l_column->set_medium_text('Gift').
        l_column->set_short_text('Gift').

      catch cx_salv_not_found into l_oref.
        l_text = l_oref->get_text( ).
    endtry.

    l_display = l_table->get_display_settings( ).
    l_display->set_striped_pattern( if_salv_c_bool_sap=>true ).

    l_layout = l_table->get_layout( ).
    l_key-report = sy-repid.
    l_layout->set_key( l_key ).
    l_layout->set_default( abap_true ).
    l_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
    l_select = l_table->get_selections( ).
    l_select->set_selection_mode( if_salv_c_selection_mode=>multiple ).

    l_table->display( ).
  endmethod.                    "display

  method get_file_name.
    if sy-batch = abap_true. return. endif.
    if sy-binpt = abap_true. return. endif.
    data: l_filename_tab type filetable,
          l_wa_filename  type localfile,
          l_rc           type i.
* Call method to show file selection screen
    call method cl_gui_frontend_services=>file_open_dialog
      changing
        file_table              = l_filename_tab
        rc                      = l_rc
      exceptions
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        others                  = 5.

    if sy-subrc = 0.
* Return user selection
      read table l_filename_tab into l_wa_filename index 1.
      if sy-subrc = 0 and l_rc > 0.
        re_file = l_wa_filename.
      endif.
    endif.
  endmethod.                    "get_file_name

  method set_file_name.
    data: l_cdate     type char10,
          l_ctime     type char6.
    write sy-datum to l_cdate mm/dd/yyyy.
    replace all occurrences of '/' in l_cdate with '.'.
    write sy-uzeit to l_ctime.
    concatenate p_file 'ship.' l_cdate '.' l_ctime '.txt' into re_file.
  endmethod.                    "set_file_name

  method main.
    create object re_obj.
  endmethod.                    "main

endclass.                    "lcl_extract IMPLEMENTATION

*----------------------------------------------------------------------*
*       Initialization
*----------------------------------------------------------------------*
initialization.
* Order Date default
  clear s_audat.
  s_audat-sign   = 'I'.
  s_audat-option = 'EQ'.
  s_audat-low    = sy-datum.
  append s_audat.

* Order Time default
  clear s_erzet.
  s_erzet-sign   = 'I'.
  s_erzet-option = 'EQ'.
  s_erzet-low    = sy-uzeit.
  append s_erzet.

* Document Type default
  clear s_auart.
  s_auart-sign   = 'I'.
  s_auart-option = 'EQ'.
  s_auart-low    = 'ZPC'.
  append s_auart.

* Sold To default
  clear s_kunnr.
  s_kunnr-sign   = 'I'.
  s_kunnr-option = 'EQ'.
  s_kunnr-low    = '40100000'.
  append s_kunnr.

  p_file = lcl_extract=>set_file_name( ).

*----------------------------------------------------------------------*
*       At Selection Screen
*----------------------------------------------------------------------*
at selection-screen on value-request for p_file.
  p_file = lcl_extract=>get_file_name( ).

*----------------------------------------------------------------------*
*       Start of Selection
*----------------------------------------------------------------------*
start-of-selection.
  if sy-batch = abap_true.
    p_file = lcl_extract=>set_file_name( ).
  endif.

  try.
      o_extract = lcl_extract=>main( ).
    catch lcl_exception into o_exception.
      l_message = o_exception->get_message( ).
      message i398(00) with l_message space
                             space space.
      exit.
  endtry.

  try.
      o_extract->execute( ).
    catch lcl_exception into o_exception.
      l_message = o_exception->get_message( ).
      message i398(00) with l_message space
                             space space.
  endtry.

*----------------------------------------------------------------------*
*       End of Selection
*----------------------------------------------------------------------*
end-of-selection.
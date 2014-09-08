*----------------------------------------------------------------------*
* Report  ZSHORTAGE_MAINTENANCE
*
* Utility for managing/maintaining table ZSHORTAGE
*
* This program will be used to display records from table ZSHORTAGE
* along with ancillary data via an ALV grid. Users will able to
* maintain certain ZSHORTAGE fields directly from this grid.
*----------------------------------------------------------------------*
<<<<<<< HEAD
<<<<<<< HEAD
* Created:       kps 7/29/2014
* Minor changes: kps 9/8/2014
=======
* Created: kps 7/29/2014
>>>>>>> FETCH_HEAD
=======
* Created: kps 7/29/2014
>>>>>>> FETCH_HEAD
*
*----------------------------------------------------------------------*

report  zshortage_maintenance.

* Defer class definition
class lcl_shortage_maintenance definition deferred.

* Global data
data g_shortages type ref to lcl_shortage_maintenance.

tables zshortage_status.

types: begin of ty_shortage_status,
        mandt       type mandt,
        status      type zzshort_status,
        status_desc type zzshort_status_desc,
       end of ty_shortage_status.

data: gt_shortage_status  type table of ty_shortage_status,
      gt_status_dropdown  type vrm_values,
      gwa_status_dropdown like line of gt_status_dropdown.

* Global selections and parameter
data: g_short_date    type dats,
      g_warehouse     type lgnum,
      g_material      type matnr,
      g_wave          type lvs_refnr,
      g_wave_creator  type ernam,
      g_customer_code type char2.

select-options: s_date for g_short_date no-extension no intervals,
                s_wrhs for g_warehouse no-extension no intervals default '46' obligatory,
                s_mat  for g_material no-extension no intervals,
                s_wave for g_wave no-extension no intervals.

parameters p_status type char30 as listbox visible length 20 default 'OPEN'.

select-options: s_crtr for g_wave_creator no-extension no intervals,
                s_cc   for g_customer_code no-extension no intervals.

*----------------------------------------------------------------------*
*       CLASS lcl_shortage_maintenance DEFINITION
*----------------------------------------------------------------------*
class lcl_shortage_maintenance definition.
  public section.

    data: g_changed type boolean.

    class-methods: main.

    methods: update_table,
             refresh_alv,
             send_email, "only use after update
             commit_transaction. "helper for email class - call after sending email

  private section.

    types: begin of ty_shortage,
* ZSHORTAGE fields
             warehouse          type lgnum,
             trans_order        type tanum,
             trans_order_item   type tapos,
             short_date         type zzpick_date,
             short_time         type zzpick_time,
             puller             type zzemployee_id,
             material_no        type matnr,
             wave_number        type lvs_refnr,
             delivery           type vbeln_vl,
             delivery_item      type zzdelivery_item,
             short_quantity     type zzshort_quantity,
             open_quantity      type zzopen_quantity,
             status             type zzshort_status,
             last_changed_by    type zzlast_change,
             last_change_date   type zzdate_change,
             last_change_time   type zztime_change,
             notes              type zzshort_notes,
             prod_order_no      type aufnr,
* Additional fields
             ssi                type char3,
             plant              type char4,
             wave_creator       type char12,
             planned_ship_date  type dats,
             planned_ship_time  type tims,
             pack_size          type i,
             customer_code      type char2,
             sold_to            type kunag,
             work_center        type char8,
             order_qty          type i,
             prod_end_date      type dats,
             cartons_confirmed  type i,
             cartons_received   type i,
             available_qty      type i,
             actual_goods_issue type dats,
             cellstyles         type lvc_t_styl,
           end of ty_shortage.

    types: begin of ty_change_email,
             email_addresses   type zzemail,
             customer_code     type char2,
             warehouse         type lgnum,
             trans_order       type tanum,
             trans_order_item  type tapos,
             open_quantity     type zzopen_quantity,
             status            type zzshort_status,
             last_changed_by   type zzlast_change,
<<<<<<< HEAD
<<<<<<< HEAD
             planned_ship_date type char10,
             planned_ship_time type char8,
=======
             planned_ship_date type dats,
             planned_ship_time type tims,
>>>>>>> FETCH_HEAD
=======
             planned_ship_date type dats,
             planned_ship_time type tims,
>>>>>>> FETCH_HEAD
    end of ty_change_email.

* General
    data: t_shortages        type standard table of ty_shortage,
          wa_shortages       type ty_shortage,
          t_update           type standard table of zshortage,
          wa_update          type zshortage,
          t_fcat             type lvc_t_fcat, "field catalog
          wa_fcat            type lvc_s_fcat, "work area for field catalog
          o_grid             type ref to cl_gui_alv_grid,
          s_layout           type lvc_s_layo,
          o_container        type scrfname value 'ALV_GRID',
          o_custom_container type ref to cl_gui_custom_container,
          l_layout           type disvariant,
          t_status_dropdown  type lvc_t_drop, "table for status dropdowns
          wa_status_dropdown type lvc_s_drop, "work area for status dropdowns
          wa_status          type standard table of zshortage_status,
          ls_status          type lvc_s_drop, "structure for statuses
          l_modified_row     type int4,
          t_change_email     type standard table of ty_change_email,
          wa_change_email    type ty_change_email.

* Event handling
    data: ir_data_changed       type ref to cl_alv_changed_data_protocol,
          s_mod_cell            type lvc_s_modi.

* Exception handling
    data: o_exception type ref to cx_root,
          l_text      type string.

* Email
    constants c_error  type bapi_mtype value 'E'.

    data: o_email      type ref to zcl_send_email,
          l_subject    type so_obj_des,
          t_body       type soli_tab,
          wa_body      type soli,
          t_recipients type bcsy_smtpa.

    methods: get_data,
             prep_statuses,
             build_fcat,
             display_alv,
             handle_data_changed for event data_changed_finished
                                        of cl_gui_alv_grid
                                 importing et_good_cells.

endclass.                    "lcl_shortage_maintenance DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_shortage_maintenance IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_shortage_maintenance implementation.

  method main.
    create object g_shortages.
    g_shortages->g_changed = abap_false.
    g_shortages->get_data( ).
    g_shortages->build_fcat( ).
    g_shortages->prep_statuses( ).
    g_shortages->display_alv( ).
  endmethod.                    "main

  method get_data.
    if p_status is initial.
      ##too_many_itab_fields
      select warehouse
             trans_order
             trans_order_item
             short_date
             short_time
             puller
             material_no
             wave_number
             delivery
             delivery_item
             short_quantity
             open_quantity
             status
             last_changed_by
             last_change_date
             last_change_time
             notes
             prod_order_no
        from zshortage
      into corresponding fields of table t_shortages
       where short_date  in s_date
         and warehouse   in s_wrhs
         and material_no in s_mat
         and wave_number in s_wave.
    else.
      ##too_many_itab_fields
      select warehouse
             trans_order
             trans_order_item
             short_date
             short_time
             puller
             material_no
             wave_number
             delivery
             delivery_item
             short_quantity
             open_quantity
             status
             last_changed_by
             last_change_date
             last_change_time
             notes
             prod_order_no
        from zshortage
        into corresponding fields of table t_shortages
       where short_date  in s_date
         and warehouse   in s_wrhs
         and material_no in s_mat
         and wave_number in s_wave
         and status      =  p_status.
    endif.

    field-symbols: <fs_shortages> type ty_shortage.

    loop at t_shortages assigning <fs_shortages>.
* SSI
      select single lgbkz
               from mlgn
               into <fs_shortages>-ssi
              where lgnum = <fs_shortages>-warehouse
                and matnr = <fs_shortages>-material_no.
* Plant
      select single werks
               from ltap
               into <fs_shortages>-plant
              where lgnum = <fs_shortages>-warehouse
                and tanum = <fs_shortages>-trans_order.
* Wave Creator
      select single ernam
               from vbsk
               into <fs_shortages>-wave_creator
              where sammg = <fs_shortages>-wave_number.
* Planned Ship Date
      select single wadat
               from likp
               into <fs_shortages>-planned_ship_date
              where vbeln = <fs_shortages>-delivery.
* Planned Ship Time
      select single wauhr
               from likp
               into <fs_shortages>-planned_ship_time
              where vbeln = <fs_shortages>-delivery.
* Pack Size
      data: l_numerator   type i,
            l_denominator type i.

      select single umrez
               into l_numerator
               from marm
              where matnr = <fs_shortages>-material_no
                and meinh = 'CTN'.

      select single umren
               into l_denominator
               from marm
              where matnr = <fs_shortages>-material_no
                and meinh = 'CTN'.

      if l_denominator <> 0.
        <fs_shortages>-pack_size = l_numerator / l_denominator.
      else.
        <fs_shortages>-pack_size = 0.
      endif.
* Customer Code
      select single maktx
               from makt
               into <fs_shortages>-customer_code
              where matnr = <fs_shortages>-material_no.
      <fs_shortages>-customer_code = <fs_shortages>-customer_code(2).
* Sold To
      select single kunag
               from likp
               into <fs_shortages>-sold_to
              where vbeln = <fs_shortages>-delivery
                and lgnum = <fs_shortages>-warehouse.
* Work Center
      if <fs_shortages>-prod_order_no is not initial.
        select single work_center
                 from zlabel
                 into <fs_shortages>-work_center
                where material_no = <fs_shortages>-material_no
                  and plant       = <fs_shortages>-plant
                  and rec_program = 'ZCTN'.
      endif.
* Order Quantity
      if <fs_shortages>-prod_order_no is not initial.
        data l_qty type i.

        select single gamng
                 from afko
                 into l_qty
                where aufnr = <fs_shortages>-prod_order_no.
        if <fs_shortages>-pack_size <> 0.
          <fs_shortages>-order_qty = l_qty / <fs_shortages>-pack_size.
        else.
          <fs_shortages>-order_qty = 0.
        endif.
      endif.
* Production End Date
      if <fs_shortages>-prod_order_no is not initial.
        select single gltrp
                 from afko
                 into <fs_shortages>-prod_end_date
                where aufnr = <fs_shortages>-prod_order_no.
      endif.
* Number of Cartons Confirmed
      if <fs_shortages>-prod_order_no is not initial.
        select single prod_order_qty
          from zlabel
          into <fs_shortages>-cartons_confirmed
         where prod_order_no = <fs_shortages>-prod_order_no
           and ( status = 'P' or status = 'E' )
           and rec_program = 'ZCTN'.
      endif.
* Number of Cartons Received
      if <fs_shortages>-prod_order_no is not initial.
        select single prod_order_qty
          from zlabel
          into <fs_shortages>-cartons_received
         where prod_order_no = <fs_shortages>-prod_order_no
           and gr_date <> 0
           and rec_program = 'ZCTN'.
      endif.
* Available Quantity
      select sum( verme )
        from lqua
        into <fs_shortages>-available_qty
       where matnr = <fs_shortages>-material_no
         and lgnum = <fs_shortages>-warehouse
         and ( lgtyp = '001' or lgtyp = '002' ).
* Actual Goods Issue
<<<<<<< HEAD
<<<<<<< HEAD
      select single wadat_ist
=======
      select single wadat
>>>>>>> FETCH_HEAD
=======
      select single wadat
>>>>>>> FETCH_HEAD
        from likp
        into <fs_shortages>-actual_goods_issue
       where vbeln = <fs_shortages>-delivery .
* Cell Styles
      data: ls_listrow like line of t_shortages,
            ls_stylerow type lvc_s_styl,
            lt_styletab type lvc_t_styl.

      clear ls_stylerow.
      clear lt_styletab[].

      ls_listrow = <fs_shortages>.

      if ls_listrow-status(6) = 'CLOSED'.
        ls_stylerow-fieldname = 'OPEN_QUANTITY'.
        ls_stylerow-style = cl_gui_alv_grid=>mc_style_disabled.
        insert ls_stylerow into table lt_styletab.

        ls_stylerow-fieldname = 'STATUS'.
        ls_stylerow-style = cl_gui_alv_grid=>mc_style_disabled.
        insert ls_stylerow into table lt_styletab.

        ls_stylerow-fieldname = 'NOTES'.
        ls_stylerow-style = cl_gui_alv_grid=>mc_style_disabled.
        insert ls_stylerow into table lt_styletab.

        ls_stylerow-fieldname = 'PROD_ORDER_NO'.
        ls_stylerow-style = cl_gui_alv_grid=>mc_style_disabled.
        insert ls_stylerow into table lt_styletab.

        ls_listrow-cellstyles = lt_styletab.
        <fs_shortages>-cellstyles = ls_listrow-cellstyles.
      endif.
      modify t_shortages from <fs_shortages>.
    endloop.
  endmethod.                    "get_data

  method prep_statuses.
    clear: t_status_dropdown[],
           wa_status_dropdown,
           wa_status[].

    select mandt status status_desc
      from zshortage_status
into table wa_status.

    field-symbols <fs_status> type zshortage_status.

    loop at wa_status assigning <fs_status>.
      wa_status_dropdown-handle = 1.
      wa_status_dropdown-value = <fs_status>-status.
      append wa_status_dropdown to t_status_dropdown.
      clear wa_status_dropdown.
    endloop.
  endmethod.                    "prep_statuses

  method build_fcat.
    clear t_fcat[].

    clear wa_fcat.
    wa_fcat-col_pos   = 1.
    wa_fcat-fieldname = 'WAREHOUSE'.
    wa_fcat-scrtext_s = 'Whs'.
    wa_fcat-scrtext_m = 'Wrhs'.
    wa_fcat-scrtext_l = 'Warehouse'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 2.
    wa_fcat-fieldname = 'TRANS_ORDER'.
    wa_fcat-scrtext_s = 'TO'.
    wa_fcat-scrtext_m = 'Trns Ord'.
    wa_fcat-scrtext_l = 'Transfer Order'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 3.
    wa_fcat-fieldname = 'TRANS_ORDER_ITEM'.
    wa_fcat-scrtext_s = 'Itm'.
    wa_fcat-scrtext_m = 'TO Item'.
    wa_fcat-scrtext_l = 'Transfer Order Item'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 4.
    wa_fcat-fieldname = 'SHORT_DATE'.
    wa_fcat-scrtext_s = 'SDt'.
    wa_fcat-scrtext_m = 'Shrt Dt'.
    wa_fcat-scrtext_l = 'Short Date'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 5.
    wa_fcat-fieldname = 'SHORT_TIME'.
    wa_fcat-scrtext_s = 'STm'.
    wa_fcat-scrtext_m = 'Shrt Tm'.
    wa_fcat-scrtext_l = 'Short Time'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 6.
    wa_fcat-fieldname = 'PULLER'.
    wa_fcat-scrtext_s = 'P'.
    wa_fcat-scrtext_m = 'Plr'.
    wa_fcat-scrtext_l = 'Puller'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 7.
    wa_fcat-fieldname = 'MATERIAL_NO'.
    wa_fcat-scrtext_s = 'Mtl'.
    wa_fcat-scrtext_m = 'Matl'.
    wa_fcat-scrtext_l = 'Material'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 8.
    wa_fcat-fieldname = 'WAVE_NUMBER'.
    wa_fcat-scrtext_s = 'Grp'.
    wa_fcat-scrtext_m = 'Group'.
    wa_fcat-scrtext_l = 'Group'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 9.
    wa_fcat-fieldname = 'DELIVERY'.
    wa_fcat-scrtext_s = 'Del'.
    wa_fcat-scrtext_m = 'Delv'.
    wa_fcat-scrtext_l = 'Delivery'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 10.
    wa_fcat-fieldname = 'DELIVERY_ITEM'.
    wa_fcat-scrtext_s = 'Itm'.
    wa_fcat-scrtext_m = 'Delv Itm'.
    wa_fcat-scrtext_l = 'Delivery Item'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 11.
    wa_fcat-fieldname = 'SHORT_QUANTITY'.
    wa_fcat-scrtext_s = 'SQty'.
    wa_fcat-scrtext_m = 'Shrt Qty'.
    wa_fcat-scrtext_l = 'Short Quantity'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 12.
    wa_fcat-fieldname = 'OPEN_QUANTITY'.
    wa_fcat-scrtext_s = 'OQty'.
    wa_fcat-scrtext_m = 'Opn Qty'.
    wa_fcat-scrtext_l = 'Open Quantity'.
    wa_fcat-edit      = abap_true.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 13.
    wa_fcat-fieldname = 'STATUS'.
    wa_fcat-scrtext_s = 'Sts'.
    wa_fcat-scrtext_m = 'Status'.
    wa_fcat-scrtext_l = 'Status'.
    wa_fcat-edit      = abap_true.
    wa_fcat-drdn_hndl = '1'.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 14.
    wa_fcat-fieldname = 'LAST_CHANGED_BY'.
    wa_fcat-scrtext_s = 'Chg By'.
    wa_fcat-scrtext_m = 'Lst Chg By'.
    wa_fcat-scrtext_l = 'User ID of Last Change'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 15.
    wa_fcat-fieldname = 'LAST_CHANGE_DATE'.
    wa_fcat-scrtext_s = 'Chg Dt'.
    wa_fcat-scrtext_m = 'Lst Chg Dt'.
    wa_fcat-scrtext_l = 'Date of Last Change'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 16.
    wa_fcat-fieldname = 'LAST_CHANGE_TIME'.
    wa_fcat-scrtext_s = 'Chg Tm'.
    wa_fcat-scrtext_m = 'Lst Chg Tm'.
    wa_fcat-scrtext_l = 'Time of Last Change'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 17.
    wa_fcat-fieldname = 'NOTES'.
    wa_fcat-scrtext_s = 'Notes'.
    wa_fcat-scrtext_m = 'Notes'.
    wa_fcat-scrtext_l = 'Pick Shortage Notes'.
    wa_fcat-outputlen = '255'.
    wa_fcat-col_opt   = abap_true.
    wa_fcat-edit      = abap_true.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 18.
    wa_fcat-fieldname = 'PROD_ORDER_NO'.
    wa_fcat-scrtext_s = 'PO'.
    wa_fcat-scrtext_m = 'Prd Ord'.
    wa_fcat-scrtext_l = 'Production Order'.
    wa_fcat-edit      = abap_true.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 19.
    wa_fcat-fieldname = 'SSI'.
    wa_fcat-scrtext_s = 'SSI'.
    wa_fcat-scrtext_m = 'SSI'.
    wa_fcat-scrtext_l = 'Storage Section Indicators'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 20.
    wa_fcat-fieldname = 'PLANT'.
    wa_fcat-scrtext_s = 'Plt'.
    wa_fcat-scrtext_m = 'Plnt'.
    wa_fcat-scrtext_l = 'Plant'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 21.
    wa_fcat-fieldname = 'WAVE_CREATOR'.
    wa_fcat-scrtext_s = 'WC'.
    wa_fcat-scrtext_m = 'Wv Crt'.
    wa_fcat-scrtext_l = 'Wave Creator'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 22.
    wa_fcat-fieldname = 'PLANNED_SHIP_DATE'.
    wa_fcat-scrtext_s = 'PlShDt'.
    wa_fcat-scrtext_m = 'Pln Shp Dt'.
    wa_fcat-scrtext_l = 'Planned Ship Date'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 23.
    wa_fcat-fieldname = 'PLANNED_SHIP_TIME'.
    wa_fcat-scrtext_s = 'PlShTm'.
    wa_fcat-scrtext_m = 'Pln Shp Tm'.
    wa_fcat-scrtext_l = 'Planned Ship Time'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 24.
    wa_fcat-fieldname = 'PACK_SIZE'.
    wa_fcat-scrtext_s = 'Pack'.
    wa_fcat-scrtext_m = 'Pack'.
    wa_fcat-scrtext_l = 'Pack Size'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 25.
    wa_fcat-fieldname = 'CUSTOMER_CODE'.
    wa_fcat-scrtext_s = 'CC'.
    wa_fcat-scrtext_m = 'Cust Cd'.
    wa_fcat-scrtext_l = 'Customer Code'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 26.
    wa_fcat-fieldname = 'SOLD_TO'.
    wa_fcat-scrtext_s = 'SldTo'.
    wa_fcat-scrtext_m = 'Sold To'.
    wa_fcat-scrtext_l = 'Sold To'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 27.
    wa_fcat-fieldname = 'WORK_CENTER'.
    wa_fcat-scrtext_s = 'WkCtr'.
    wa_fcat-scrtext_m = 'Work Ctr'.
    wa_fcat-scrtext_l = 'Work Center'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 28.
    wa_fcat-fieldname = 'ORDER_QTY'.
    wa_fcat-scrtext_s = 'Qty'.
    wa_fcat-scrtext_m = 'Ord Qty'.
    wa_fcat-scrtext_l = 'Order Quantity'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 29.
    wa_fcat-fieldname = 'PROD_END_DATE'.
    wa_fcat-scrtext_s = 'PrEdDt'.
    wa_fcat-scrtext_m = 'Prd End Dt'.
    wa_fcat-scrtext_l = 'Production End Date'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 30.
    wa_fcat-fieldname = 'CARTONS_CONFIRMED'.
    wa_fcat-scrtext_s = 'CtCf'.
    wa_fcat-scrtext_m = 'Cts Cfmd'.
    wa_fcat-scrtext_l = 'Cartons Confirmed'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 31.
    wa_fcat-fieldname = 'CARTONS_RECEIVED'.
    wa_fcat-scrtext_s = 'CtRc'.
    wa_fcat-scrtext_m = 'Cts Rcvd'.
    wa_fcat-scrtext_l = 'Cartons Received'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 32.
    wa_fcat-fieldname = 'AVAILABLE_QTY'.
    wa_fcat-scrtext_s = 'AQty'.
    wa_fcat-scrtext_m = 'Avl Qty'.
    wa_fcat-scrtext_l = 'Available Quantiy'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.

    clear wa_fcat.
    wa_fcat-col_pos   = 33.
    wa_fcat-fieldname = 'ACTUAL_GOODS_ISSUE'.
    wa_fcat-scrtext_s = 'AGI'.
    wa_fcat-scrtext_m = 'Act GI'.
    wa_fcat-scrtext_l = 'Actual Goods Issue'.
    wa_fcat-edit      = abap_false.
    append wa_fcat to t_fcat.
  endmethod.                    "build_fcat

  method display_alv.
    create object o_custom_container
      exporting
        container_name = o_container.

    create object o_grid
      exporting
        i_parent = o_custom_container.

    set handler me->handle_data_changed for o_grid.

    o_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    o_grid->set_drop_down_table( it_drop_down = t_status_dropdown ).

* Selection mode
    s_layout-sel_mode   = '0'.

* Field name for styles table
    s_layout-stylefname = 'CELLSTYLES'.

* Optimize column width
    s_layout-cwidth_opt = abap_true.

    clear l_text.
    try.
        o_grid->set_table_for_first_display(
            exporting i_save = 'A'
                      is_layout = s_layout
             changing it_fieldcatalog = t_fcat
                      it_outtab = t_shortages ).
      catch cx_root into o_exception.
        l_text = o_exception->if_message~get_longtext( ).
    endtry.

    call screen 100.
  endmethod.                    "display_alv

  method update_table.
    field-symbols: <fs_shortages> type ty_shortage.

    clear t_update[].
    clear wa_update.
    loop at t_shortages assigning <fs_shortages>.
      move-corresponding <fs_shortages> to wa_update.
      append wa_update to t_update.
      clear wa_update.
    endloop.

    update zshortage from table t_update.
    commit work and wait.
    message 'Changes saved successfully.' type 'S'.

    send_email( ).
  endmethod.                    "update_table

  method handle_data_changed.
    g_changed = abap_true.

    clear wa_shortages.
    loop at et_good_cells into s_mod_cell.
      wa_shortages-last_changed_by  = sy-uname.
      wa_shortages-last_change_date = sy-datum.
      wa_shortages-last_change_time = sy-uzeit.

* Modify internal shortages table
      modify t_shortages from wa_shortages
                        index s_mod_cell-row_id
                 transporting last_changed_by
                              last_change_date
                              last_change_time.

* Collect data for status change email(s)
      clear: wa_shortages,
             wa_change_email.

      read table t_shortages
           index s_mod_cell-row_id
            into wa_shortages.

      select single email_addresses
               from zpckprint
               into wa_change_email
              where warehouse = wa_shortages-warehouse
<<<<<<< HEAD
<<<<<<< HEAD
                and status    = wa_shortages-status
                and ssi       = wa_shortages-ssi.
=======
                and status    = wa_shortages-status.
>>>>>>> FETCH_HEAD
=======
                and status    = wa_shortages-status.
>>>>>>> FETCH_HEAD

      if sy-subrc = 0.
        wa_change_email-warehouse         = wa_shortages-warehouse.
        wa_change_email-customer_code     = wa_shortages-customer_code.
        wa_change_email-trans_order       = wa_shortages-trans_order.
        wa_change_email-trans_order_item  = wa_shortages-trans_order_item.
        wa_change_email-open_quantity     = wa_shortages-open_quantity.
        wa_change_email-status            = wa_shortages-status.
        wa_change_email-last_changed_by   = wa_shortages-last_changed_by.
<<<<<<< HEAD
<<<<<<< HEAD

        write wa_shortages-planned_ship_date to wa_change_email-planned_ship_date.
        write wa_shortages-planned_ship_time to wa_change_email-planned_ship_time.
=======
        wa_change_email-planned_ship_date = wa_shortages-planned_ship_date.
        wa_change_email-planned_ship_time = wa_shortages-planned_ship_time.
>>>>>>> FETCH_HEAD
=======
        wa_change_email-planned_ship_date = wa_shortages-planned_ship_date.
        wa_change_email-planned_ship_time = wa_shortages-planned_ship_time.
>>>>>>> FETCH_HEAD

        append wa_change_email to t_change_email.
        sort t_change_email by warehouse
                               trans_order
                               trans_order_item
                               status.
        delete adjacent duplicates from t_change_email comparing all fields.
      endif.
    endloop.

    refresh_alv( ).
  endmethod.                    "handle_data_changed

  method refresh_alv.
    clear l_text.
    try.
        o_grid->refresh_table_display( ).
      catch cx_root into o_exception.
        l_text = o_exception->if_message~get_longtext( ).
    endtry.
  endmethod.                    "refresh_alv

  method send_email.
    data l_quantity type char5.

    field-symbols: <fs_email> type ty_change_email.

    loop at t_change_email assigning <fs_email>.
      clear: l_quantity,
             l_subject,
             t_body[],
             wa_body,
             t_recipients[].

* Email subject
      write <fs_email>-open_quantity to l_quantity decimals 0.
      concatenate 'Shortage Status Change:'
                  <fs_email>-customer_code
             into l_subject
        separated by space.
* Email body
      concatenate 'Status:'
                  <fs_email>-status
                  'CC:'
                  <fs_email>-customer_code
                  'Qty:'
                  l_quantity
             into wa_body
        separated by space.
      append wa_body to t_body.

      clear wa_body.
      concatenate 'A status change for Transfer Order:'
                  <fs_email>-trans_order
                  <fs_email>-trans_order_item
             into wa_body
        separated by space.
      append wa_body to t_body.

      clear wa_body.
      concatenate 'has been recorded by:'
                  <fs_email>-last_changed_by
             into wa_body
     separated by space.
      append wa_body to t_body.

      clear wa_body.
      wa_body = space.
      append wa_body to t_body.

      clear wa_body.
      concatenate 'This shortage has a current scheduled ship date and time of:'
                  <fs_email>-planned_ship_date
                  <fs_email>-planned_ship_time
             into wa_body
     separated by space.
      append wa_body to t_body.
* Email recipients
      split <fs_email>-email_addresses at ',' into table t_recipients.

      clear l_text.
      o_email = zcl_send_email=>main( ).
      try.
          o_email->send_the_email( im_subject       = l_subject
                                   im_body          = t_body
                                   im_recipient_tab = t_recipients ).
        catch cx_root into o_exception.
          l_text = o_exception->if_message~get_longtext( ).
      endtry.

      commit_transaction( ).
    endloop.
  endmethod.                    "send_email

  method commit_transaction.
    data: l_creturn type bapiret2.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait   = 'X'
      importing
        return = l_creturn.
    if l_creturn-type = c_error.
      "do nothing - we aren't actually confirming here
    endif.
  endmethod.                    "commit_transaction

endclass.                    "lcl_shortage_maintenance IMPLEMENTATION

*----------------------------------------------------------------------*
*   Initialization
*----------------------------------------------------------------------*
initialization.
  clear: gt_shortage_status[],
         gt_status_dropdown[],
         gwa_status_dropdown.

  select mandt status status_desc
    from zshortage_status
into table gt_shortage_status.

  field-symbols <fs_status> type ty_shortage_status.

  loop at gt_shortage_status assigning <fs_status>.
    "gwa_status_dropdown-key  = sy-tabix.
    gwa_status_dropdown = <fs_status>-status.
    append gwa_status_dropdown to gt_status_dropdown.
    clear gwa_status_dropdown.
  endloop.

  call function 'VRM_SET_VALUES'
    exporting
      id              = 'p_status'
      values          = gt_status_dropdown
    exceptions
      id_illegal_name = 1
      others          = 2.

*----------------------------------------------------------------------*
*   Start of Selection
*----------------------------------------------------------------------*
start-of-selection.
  lcl_shortage_maintenance=>main( ).

*----------------------------------------------------------------------*
*   Module  INITIALIZE_0100  OUTPUT
*----------------------------------------------------------------------*
module initialize_100 output.
  set pf-status '100'.
  set titlebar '100'.
endmodule.                 " initialize_100  OUTPUT

*----------------------------------------------------------------------*
*   Module  USER_COMMAND_100  INPUT
*----------------------------------------------------------------------*
module user_command_100 input.
  data: l_answer type string,
        ok_code  type sy-ucomm,
        save_ok  type sy-ucomm,
        output   type sy-ucomm.

  ok_code = sy-ucomm.
  save_ok = ok_code.
  clear ok_code.
  case save_ok.
    when '&F03'.
      if sy-dynnr = 100.
        if g_shortages->g_changed = abap_true.
          call function 'POPUP_TO_CONFIRM'
            exporting
              titlebar              = 'Data has been changed'
              text_question         = 'Data has been changed. Do you want to save?'
              text_button_1         = 'Yes'(001)
              text_button_2         = 'No'(002)
              default_button        = '1'
              display_cancel_button = abap_true
            importing
              answer                = l_answer
            exceptions
              text_not_found        = 1
              others                = 2.
          case l_answer.
            when '1'.
              g_shortages->update_table( ).
              message 'Changes saved.' type 'S'.
              leave to screen 0.
            when '2'.
              leave to screen 0.
            when 'A'.
              g_shortages->refresh_alv( ).
              message 'Operation canceled by user.' type 'S'.
          endcase.
        else.
          leave to screen 0.
        endif.
      else.
        leave program.
      endif.
    when '&F15' or '&F12'.
      leave program.
    when 'SAVE'.
      if sy-dynnr = 100.
        g_shortages->update_table( ).
        g_shortages->refresh_alv( ).
        message 'Changes saved.' type 'S'.
      endif.
    when others.
      output = save_ok.
  endcase.
<<<<<<< HEAD
<<<<<<< HEAD
endmodule.                 " user_command_100  INPUT
=======
endmodule.                 " user_command_100  INPUT
>>>>>>> FETCH_HEAD
=======
endmodule.                 " user_command_100  INPUT
>>>>>>> FETCH_HEAD

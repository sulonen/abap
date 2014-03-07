*&---------------------------------------------------------------------*
*&  Include           ZXRSRU01                                         *
*&---------------------------------------------------------------------*
* user exit for variable processing

* Begin kevins 01/16/2014
* Data for POS 5 Week Trend reporting
data: p_date         like sy-datum,
      l_index_date   like sy-datum,
      l_index_week   like scal-week,
      l_low_week     like scal-week,
      l_index        type integer.

types: begin of l_fiscal_week_ty,
      /bic/zcfiscwk type /bic/oizcfiscwk,
     end of l_fiscal_week_ty.

* Data for POS weekly reporting.
data: l_fiscal_weeks_itab  type standard table of l_fiscal_week_ty,
      l_var_range          like rrrangeexit,
      l_first_day          type sy-datum,
      l_last_day           type sy-datum,
      l_customer           type char30,
      l_fiscal_week(6)     type n,
      l_fiscal_week_low(6) type n,
      l_fiscal_month(6)    type n,
      l_date               type char10.

* End kevins 01/16/2014

case i_vnam.
* Begin kevins 2/28/2014
* POS 5-Week: POS 5 Week Sales Trend Reporting - 5 Week Range
  when 'ZPOS5WK'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_index_week,
             l_low_week.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.

        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Set initial value for low week.
        l_low_week = l_index_week.

* If the 5-week period crosses the beginning of the calendar year,
* subtract 1 from the year and calculate the week appropriately.
        if l_low_week+4(2) - 4 < 1.
          l_low_week(4) = l_low_week(4) - 1.
          l_low_week+4(2) = 52 + ( l_low_week+4(2) - 4 ).
        else.
* Otherwise, just subtract 5 weeks.
          l_low_week+4(2) = l_low_week+4(2) - 4.
        endif.

      else.

        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* If this is the first week of the year, subtract 1 year
* and change the week to 52.
        if l_index_week+4(2) - 1 < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52.
        else.
* Otherwise, just subtract a week.
          l_index_week = l_index_week - 1.
        endif.

* Set initial value for low week.
        l_low_week = l_index_week.
* If the 5-week period crosses the beginning of the calendar year,
* subtract 1 from the year and calculate the week appropriately.
        if l_low_week+4(2) - 4 < 1.
          l_low_week(4) = l_low_week(4) - 1.
          l_low_week+4(2) = 52 + ( l_low_week+4(2) - 4 ).
        else.
* Otherwise, just subtract 5 weeks.
          l_low_week+4(2) = l_low_week+4(2) - 4.
        endif.
      endif.

* Return the date range to the query.
      l_s_range-low    = l_low_week.
      l_s_range-high   = l_index_week.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'BT'.
      append l_s_range to e_t_range.
    endif.

* POS Week 5: POS 5 Week Sales Trend Reporting - Week 5
* Previous week for broadcast reporting (non-interactive).
  when 'ZPOSWK5'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_index_week.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.

        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

      else.

        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 1.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract 1 week.
          l_index_week = l_index_week - 1.
        endif.
      endif.

* Return the week to the query.
      l_s_range-low    = l_index_week.
      l_s_range-high   = l_index_week.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 4: POS 5 Week Sales Trend Reporting - Week 4
* Previous week for broadcast reporting (non-interactive).
  when 'ZPOSWK4'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_index_week.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 1.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract a week.
          l_index_week = l_index_week - 1.
        endif.
      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 2.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract 2 weeks.
          l_index_week = l_index_week - 2.
        endif.
      endif.

* Return the week to the query.
      l_s_range-low    = l_index_week.
      l_s_range-high   = l_index_week.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 3: POS 5 Week Sales Trend Reporting - Week 3
* Previous week for broadcast reporting (non-interactive).
  when 'ZPOSWK3'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_index_week.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 2.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract 2 weeks.
          l_index_week = l_index_week - 2.
        endif.
      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 3.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract 3 weeks.
          l_index_week = l_index_week - 3.
        endif.
      endif.
* Set index date.
      call function 'DATE_CREATE'
        exporting
          datum_ein = p_date
        importing
          datum_aus = l_index_date.

      call function 'DATE_GET_WEEK'
        exporting
          date         = l_index_date
        importing
          week         = l_index_week
        exceptions
          date_invalid = 1
          others       = 2.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
      l_index = l_index_week+4(2) - 3.
      if l_index < 1.
        l_index_week(4) = l_index_week(4) - 1.
        l_index_week+4(2) = 52 - abs( l_index ).
      else.
* Otherwise, just subtract 3 weeks.
        l_index_week = l_index_week - 3.
      endif.

* Return the week to the query.
      l_s_range-low    = l_index_week.
      l_s_range-high   = l_index_week.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 2: POS 5 Week Sales Trend Reporting - Week 2
* Previous week for broadcast reporting (non-interactive).
  when 'ZPOSWK2'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_index_week.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 3.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract 3 weeks.
          l_index_week = l_index_week - 3.
        endif.
      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 4.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract 4 weeks.
          l_index_week = l_index_week - 4.
        endif.
      endif.

* Return the week to the query.
      l_s_range-low    = l_index_week.
      l_s_range-high   = l_index_week.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 1: POS 5 Week Sales Trend Reporting - Week 1
* Previous week for broadcast reporting (non-interactive).
  when 'ZPOSWK1'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_index_week.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 4.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract 4 weeks.
          l_index_week = l_index_week - 4.
        endif.
      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 5.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract 5 weeks.
          l_index_week = l_index_week - 5.
        endif.
      endif.

* Return the week to the query.
      l_s_range-low    = l_index_week.
      l_s_range-high   = l_index_week.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 5 (LY): POS 5 Week Sales Trend Reporting - Week 5 (Last Year)
* Previous week for broadcast reporting (non-interactive).
  when 'ZPOSWK5LY'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_index_week.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.
* Subtract a year
        l_index_week(4) = l_index_week(4) - 1.
      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year
        l_index_week(4) = l_index_week(4) - 1.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 1.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract 1 week.
          l_index_week = l_index_week - 1.
        endif.
      endif.

* Return the week to the query.
      l_s_range-low    = l_index_week.
      l_s_range-high   = l_index_week.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 4 (LY): POS 5 Week Sales Trend Reporting - Week 4 (Last Year)
* Previous week for broadcast reporting (non-interactive).
  when 'ZPOSWK4LY'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_index_week.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year
        l_index_week(4) = l_index_week(4) - 1.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 1.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract a week.
          l_index_week = l_index_week - 1.
        endif.
      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year
        l_index_week(4) = l_index_week(4) - 1.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 2.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract 2 weeks.
          l_index_week = l_index_week - 2.
        endif.
      endif.

* Return the week to the query.
      l_s_range-low    = l_index_week.
      l_s_range-high   = l_index_week.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 3 (LY): POS 5 Week Sales Trend Reporting - Week 3 (Last Year)
* Previous week for broadcast reporting (non-interactive).
  when 'ZPOSWK3LY'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_index_week.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year
        l_index_week(4) = l_index_week(4) - 1.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 2.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract 2 weeks.
          l_index_week = l_index_week - 2.
        endif.
      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year
        l_index_week(4) = l_index_week(4) - 1.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 3.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract 3 weeks.
          l_index_week = l_index_week - 3.
        endif.
      endif.

* Return the week to the query.
      l_s_range-low    = l_index_week.
      l_s_range-high   = l_index_week.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 2 (LY): POS 5 Week Sales Trend Reporting - Week 2 (Last Year)
* Previous week for broadcast reporting (non-interactive).
  when 'ZPOSWK2LY'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_index_week.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year
        l_index_week(4) = l_index_week(4) - 1.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 3.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract 3 weeks.
          l_index_week = l_index_week - 3.
        endif.
      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year
        l_index_week(4) = l_index_week(4) - 1.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 4.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract 4 weeks.
          l_index_week = l_index_week - 4.
        endif.
      endif.

* Return the week to the query.
      l_s_range-low    = l_index_week.
      l_s_range-high   = l_index_week.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 1 (LY): POS 5 Week Sales Trend Reporting - Week 1 (Last Year)
* Previous week for broadcast reporting (non-interactive).
  when 'ZPOSWK1LY'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_index_week.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year
        l_index_week(4) = l_index_week(4) - 1.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 4.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract 4 weeks.
          l_index_week = l_index_week - 4.
        endif.
      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year
        l_index_week(4) = l_index_week(4) - 1.

* If this would be before the first week of the year,
* subtract 1 year, calculate the appropriate week, and
* adjust accordingly.
        l_index = l_index_week+4(2) - 5.
        if l_index < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52 - abs( l_index ).
        else.
* Otherwise, just subtract 5 weeks.
          l_index_week = l_index_week - 5.
        endif.
      endif.

* Return the week to the query.
      l_s_range-low    = l_index_week.
      l_s_range-high   = l_index_week.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Last Fiscal Week: POS Weekly Report
* Previous customer fiscal week.
  when 'ZPOSLFW'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_week.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.
      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* If this is the first week of the year, subtract 1 year
* and change the week to 52.
        if l_index_week+4(2) - 1 < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52.
        else.
* Otherwise, just subtract a week.
          l_index_week = l_index_week - 1.
        endif.
      endif.

* Get first day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      if sy-subrc = 0.
        l_customer = l_var_range-low.

* Get customer fiscal week.
* Note: /BIC/AZPOSCAL00 is the active ODS table for the
* POS Customer Calendar ODS (ZPOSCAL).
        select single /bic/zcfiscwk
          from /bic/azposcal00
          into l_fiscal_week
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Return the week to the query.
        l_s_range-low    = l_fiscal_week.
        l_s_range-high   = l_fiscal_week.
        l_s_range-sign   = 'I'.
        l_s_range-opt    = 'EQ'.
        append l_s_range to e_t_range.
      endif.
    endif.

* POS Fiscal Month to Date: POS Weekly Report
* Customer fiscal Month to Date (derived from calendar week).
  when 'ZPOSFM'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_fiscal_weeks_itab[],
             l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_week,
             l_fiscal_week_low.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.
      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* If this is the first week of the year, subtract 1 year
* and change the week to 52.
        if l_index_week+4(2) - 1 < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52.
        else.
* Otherwise, just subtract a week.
          l_index_week = l_index_week - 1.
        endif.
      endif.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      if sy-subrc = 0.
        l_customer = l_var_range-low.

* Get customer fiscal week.
* Note: /BIC/AZPOSCAL00 is the active ODS table for the
* POS Customer Calendar ODS (ZPOSCAL).
        select single /bic/zcfiscwk
          from /bic/azposcal00
          into l_fiscal_week
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Get customer fiscal month.
        select single /bic/zcfiscmo
          from /bic/azposcal00
          into l_fiscal_month
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Collect customer fiscal weeks.
        select /bic/zcfiscwk
          from /bic/azposcal00
          into table l_fiscal_weeks_itab
          where customer = l_customer and
                /bic/zcfiscmo = l_fiscal_month.

        sort l_fiscal_weeks_itab by /bic/zcfiscwk.

        read table l_fiscal_weeks_itab index 1 into l_fiscal_week_low.

* Return the week range to the query.
        l_s_range-low    = l_fiscal_week_low.
        l_s_range-high   = l_fiscal_week.
        l_s_range-sign   = 'I'.
        l_s_range-opt    = 'BT'.
        append l_s_range to e_t_range.
      endif.
    endif.

* POS Last Fiscal Week (Last Year): POS Weekly Report
* Previous customer fiscal week (last year).
  when 'ZPOSLFWLY'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_week.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year.
        l_index_week(4) = l_index_week(4) - 1.
      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year.
        l_index_week(4) = l_index_week(4) - 1.

* If this is the first week of the year, subtract 1 year
* and change the week to 52.
        if l_index_week+4(2) - 1 < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52.
        else.
* Otherwise, just subtract a week.
          l_index_week = l_index_week - 1.
        endif.
      endif.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      if sy-subrc = 0.
        l_customer = l_var_range-low.

* Get customer fiscal week.
* Note: /BIC/AZPOSCAL00 is the active ODS table for the
* POS Customer Calendar ODS (ZPOSCAL).
        select single /bic/zcfiscwk
          from /bic/azposcal00
          into l_fiscal_week
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Return the week to the query.
        l_s_range-low    = l_fiscal_week.
        l_s_range-high   = l_fiscal_week.
        l_s_range-sign   = 'I'.
        l_s_range-opt    = 'EQ'.
        append l_s_range to e_t_range.
      endif.
    endif.

* POS Fiscal Month to Date (Last Year): POS Weekly Report
* Fiscal month for broadcast reporting (derived from last calendar week).
  when 'ZPOSFMLY'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_fiscal_weeks_itab[],
             l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_week,
             l_fiscal_week_low.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year.
        l_index_week(4) = l_index_week(4) - 1.
      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year.
        l_index_week(4) = l_index_week(4) - 1.

* If this is the first week of the year, subtract 1 year
* and change the week to 52.
        if l_index_week+4(2) - 1 < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52.
        else.
* Otherwise, just subtract a week.
          l_index_week = l_index_week - 1.
        endif.
      endif.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      if sy-subrc = 0.
        l_customer = l_var_range-low.

* Get customer fiscal week.
* Note: /BIC/AZPOSCAL00 is the active ODS table for the
* POS Customer Calendar ODS (ZPOSCAL).
        select single /bic/zcfiscwk
          from /bic/azposcal00
          into l_fiscal_week
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Get customer fiscal month.
        select single /bic/zcfiscmo
          from /bic/azposcal00
          into l_fiscal_month
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Collect customer fiscal weeks.
        select /bic/zcfiscwk
          from /bic/azposcal00
          into table l_fiscal_weeks_itab
          where customer = l_customer and
                /bic/zcfiscmo = l_fiscal_month.

        sort l_fiscal_weeks_itab by /bic/zcfiscwk.

        read table l_fiscal_weeks_itab index 1 into l_fiscal_week_low.

* Return the week range to the query.
        l_s_range-low    = l_fiscal_week_low.
        l_s_range-high   = l_fiscal_week.
        l_s_range-sign   = 'I'.
        l_s_range-opt    = 'BT'.
        append l_s_range to e_t_range.
      endif.
    endif.

* POS Fiscal Week (Text): Text variable for POS reporting.
  when 'ZPOSFWTXT'.
* After popup
    if i_step = 2.
      clear: l_var_range,
             l_customer,
             l_index_date,
             l_index_week.

* Get customer
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      l_customer = l_var_range-low.

* Get fiscal week
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSLFW'.
      l_index_week = l_var_range-low.

* Get date from customer fiscal week.
* Note: /BIC/AZPOSCAL00 is the active ODS table for the
* POS Customer Calendar ODS (ZPOSCAL).
      select single calday
        from /bic/azposcal00
        into l_index_date
        where customer = l_customer and
              /bic/zcfiscwk = l_index_week.

* Format date.
      concatenate l_index_date+4(2) '/'
                  l_index_date+6(2) '/'
                  l_index_date(4) into l_date.

* Return the date to the query.
      l_s_range-low    = l_date.
      l_s_range-high   = l_date.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Fiscal Week (Text): Text variable for POS reporting.
  when 'ZPOSFWLYTXT'.
* After popup
    if i_step = 2.
      clear: l_var_range,
             l_customer,
             l_index_date,
             l_index_week.

* Get customer
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      l_customer = l_var_range-low.

* Get fiscal week
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSLFWLY'.
      l_index_week = l_var_range-low.

* Get date from customer fiscal week.
* Note: /BIC/AZPOSCAL00 is the active ODS table for the
* POS Customer Calendar ODS (ZPOSCAL).
      select single calday
        from /bic/azposcal00
        into l_index_date
        where customer = l_customer and
              /bic/zcfiscwk = l_index_week.

* Format date.
      concatenate l_index_date+4(2) '/'
                  l_index_date+6(2) '/'
                  l_index_date(4) into l_date.

* Return the date to the query.
      l_s_range-low    = l_date.
      l_s_range-high   = l_date.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Fiscal Month (Text): Text variable for POS reporting.
  when 'ZPOSCUSTFMTXT'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_month,
             l_date.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.
      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* If this is the first week of the year, subtract 1 year
* and change the week to 52.
        if l_index_week+4(2) - 1 < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52.
        else.
* Otherwise, just subtract a week.
          l_index_week = l_index_week - 1.
        endif.
      endif.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      if sy-subrc = 0.
        l_customer = l_var_range-low.

* Get customer fiscal month.
        select single /bic/zcfiscmo
          from /bic/azposcal00
          into l_fiscal_month
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Format month
        concatenate l_fiscal_month+4(2) '/'
                    l_fiscal_month(4)
                    into l_date.

* Return the month to the query.
        l_s_range-low    = l_date.
        l_s_range-high   = l_date.
        l_s_range-sign   = 'I'.
        l_s_range-opt    = 'EQ'.
        append l_s_range to e_t_range.
      endif.
    endif.

* POS Fiscal Month Last Year (Text): Text variable for POS reporting.
  when 'ZPOSCUSTFMLYTXT'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_month,
             l_date.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year.
        l_index_week(4) = l_index_week(4) - 1.
      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year.
        l_index_week(4) = l_index_week(4) - 1.

* If this is the first week of the year, subtract 1 year
* and change the week to 52.
        if l_index_week+4(2) - 1 < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52.
        else.
* Otherwise, just subtract a week.
          l_index_week = l_index_week - 1.
        endif.
      endif.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      if sy-subrc = 0.
        l_customer = l_var_range-low.

* Get customer fiscal month.
        select single /bic/zcfiscmo
          from /bic/azposcal00
          into l_fiscal_month
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Format month
        concatenate l_fiscal_month+4(2) '/'
                    l_fiscal_month(4)
                    into l_date.

* Return the month to the query.
        l_s_range-low    = l_date.
        l_s_range-high   = l_date.
        l_s_range-sign   = 'I'.
        l_s_range-opt    = 'EQ'.
        append l_s_range to e_t_range.
      endif.
    endif.

* POS (Customer) Fiscal Week (Text - Week Number): Text variable for POS reporting.
  when 'ZPOSCUSTFWTXT'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_week,
             l_date.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.
      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = p_date
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year.
*      l_index_week(4) = l_index_week(4) - 1.

* If this is the first week of the year, subtract 1 year
* and change the week to 52.
        if l_index_week+4(2) - 1 < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52.
        else.
* Otherwise, just subtract a week.
          l_index_week = l_index_week - 1.
        endif.
      endif.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      if sy-subrc = 0.
        l_customer = l_var_range-low.

* Get customer fiscal week.
        select single /bic/zcfiscwk
          from /bic/azposcal00
          into l_fiscal_week
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Format month
        concatenate l_fiscal_week+4(2) '/'
                    l_fiscal_week(4)
                    into l_date.

* Return the week range to the query.
        l_s_range-low    = l_date.
        l_s_range-high   = l_date.
        l_s_range-sign   = 'I'.
        l_s_range-opt    = 'EQ'.
        append l_s_range to e_t_range.
      endif.
    endif.

* POS (Customer) Fiscal Week (Text - Week Number): Text variable for POS reporting.
  when 'ZPOSCUSTFWLYTXT'.
* After popup
    if i_step = 2.
      clear: p_date,
             l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_week,
             l_date.

* If user provided a date, use that date. Otherwise, use sy-datum.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSDATE'.
      if sy-subrc = 0.
        p_date = l_var_range-low.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = sy-datum
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year.
        l_index_week(4) = l_index_week(4) - 1.

      else.
        p_date = sy-datum.
* Set index date.
        call function 'DATE_CREATE'
          exporting
            datum_ein = sy-datum
          importing
            datum_aus = l_index_date.

        call function 'DATE_GET_WEEK'
          exporting
            date         = l_index_date
          importing
            week         = l_index_week
          exceptions
            date_invalid = 1
            others       = 2.

* Subtract a year.
        l_index_week(4) = l_index_week(4) - 1.

* If this is the first week of the year, subtract 1 year
* and change the week to 52.
        if l_index_week+4(2) - 1 < 1.
          l_index_week(4) = l_index_week(4) - 1.
          l_index_week+4(2) = 52.
        else.
* Otherwise, just subtract a week.
          l_index_week = l_index_week - 1.
        endif.
      endif.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      if sy-subrc = 0.
        l_customer = l_var_range-low.

* Get customer fiscal week.
        select single /bic/zcfiscwk
          from /bic/azposcal00
          into l_fiscal_week
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Format month
        concatenate l_fiscal_week+4(2) '/'
                    l_fiscal_week(4)
                    into l_date.

* Return the week range to the query.
        l_s_range-low    = l_date.
        l_s_range-high   = l_date.
        l_s_range-sign   = 'I'.
        l_s_range-opt    = 'EQ'.
        append l_s_range to e_t_range.
      endif.
    endif.

* POS Week 5 (Text): Text variable for POS reporting.
  when 'ZPOSWK5TXT'.
* After popup
    if i_step = 2.
      clear: l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_week,
             l_date.

* Get customer
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      l_customer = l_var_range-low.

* Get calendar week
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSWK5'.
      l_index_week = l_var_range-low.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer fiscal week.
* Note: /BIC/AZPOSCAL00 is the active ODS table for the
* POS Customer Calendar ODS (ZPOSCAL).
        select single /bic/zcfiscwk
          from /bic/azposcal00
          into l_fiscal_week
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Get date from customer fiscal week.
      select single calday
        from /bic/azposcal00
        into l_index_date
        where customer = l_customer and
              /bic/zcfiscwk = l_fiscal_week.

* Format date.
      concatenate l_index_date+4(2) '/'
                  l_index_date+6(2) '/'
                  l_index_date(4) into l_date.

* Return the date to the query.
      l_s_range-low    = l_date.
      l_s_range-high   = l_date.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 4 (Text): Text variable for POS reporting.
  when 'ZPOSWK4TXT'.
* After popup
    if i_step = 2.
      clear: l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_week,
             l_date.

* Get customer
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      l_customer = l_var_range-low.

* Get calendar week
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSWK4'.
      l_index_week = l_var_range-low.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer fiscal week.
* Note: /BIC/AZPOSCAL00 is the active ODS table for the
* POS Customer Calendar ODS (ZPOSCAL).
        select single /bic/zcfiscwk
          from /bic/azposcal00
          into l_fiscal_week
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Get date from customer fiscal week.
      select single calday
        from /bic/azposcal00
        into l_index_date
        where customer = l_customer and
              /bic/zcfiscwk = l_fiscal_week.

* Format date.
      concatenate l_index_date+4(2) '/'
                  l_index_date+6(2) '/'
                  l_index_date(4) into l_date.

* Return the date to the query.
      l_s_range-low    = l_date.
      l_s_range-high   = l_date.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 3 (Text): Text variable for POS reporting.
  when 'ZPOSWK3TXT'.
* After popup
    if i_step = 2.
      clear: l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_week,
             l_date.

* Get customer
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      l_customer = l_var_range-low.

* Get calendar week
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSWK3'.
      l_index_week = l_var_range-low.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer fiscal week.
* Note: /BIC/AZPOSCAL00 is the active ODS table for the
* POS Customer Calendar ODS (ZPOSCAL).
        select single /bic/zcfiscwk
          from /bic/azposcal00
          into l_fiscal_week
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Get date from customer fiscal week.
      select single calday
        from /bic/azposcal00
        into l_index_date
        where customer = l_customer and
              /bic/zcfiscwk = l_fiscal_week.

* Format date.
      concatenate l_index_date+4(2) '/'
                  l_index_date+6(2) '/'
                  l_index_date(4) into l_date.

* Return the date to the query.
      l_s_range-low    = l_date.
      l_s_range-high   = l_date.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 2 (Text): Text variable for POS reporting.
  when 'ZPOSWK2TXT'.
* After popup
    if i_step = 2.
      clear: l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_week,
             l_date.

* Get customer
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      l_customer = l_var_range-low.

* Get calendar week
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSWK2'.
      l_index_week = l_var_range-low.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer fiscal week.
* Note: /BIC/AZPOSCAL00 is the active ODS table for the
* POS Customer Calendar ODS (ZPOSCAL).
        select single /bic/zcfiscwk
          from /bic/azposcal00
          into l_fiscal_week
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Get date from customer fiscal week.
      select single calday
        from /bic/azposcal00
        into l_index_date
        where customer = l_customer and
              /bic/zcfiscwk = l_fiscal_week.

* Format date.
      concatenate l_index_date+4(2) '/'
                  l_index_date+6(2) '/'
                  l_index_date(4) into l_date.

* Return the date to the query.
      l_s_range-low    = l_date.
      l_s_range-high   = l_date.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 1 (Text): Text variable for POS reporting.
  when 'ZPOSWK1TXT'.
* After popup
    if i_step = 2.
      clear: l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_week,
             l_date.

* Get customer
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      l_customer = l_var_range-low.

* Get calendar week
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSWK1'.
      l_index_week = l_var_range-low.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer fiscal week.
* Note: /BIC/AZPOSCAL00 is the active ODS table for the
* POS Customer Calendar ODS (ZPOSCAL).
        select single /bic/zcfiscwk
          from /bic/azposcal00
          into l_fiscal_week
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Get date from customer fiscal week.
      select single calday
        from /bic/azposcal00
        into l_index_date
        where customer = l_customer and
              /bic/zcfiscwk = l_fiscal_week.

* Format date.
      concatenate l_index_date+4(2) '/'
                  l_index_date+6(2) '/'
                  l_index_date(4) into l_date.

* Return the date to the query.
      l_s_range-low    = l_date.
      l_s_range-high   = l_date.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 5 LY (Text): Text variable for POS reporting.
  when 'ZPOSWK5LYTXT'.
* After popup
    if i_step = 2.
      clear: l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_week,
             l_date.

* Get customer
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      l_customer = l_var_range-low.

* Get calendar week
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSWK5LY'.
      l_index_week = l_var_range-low.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer fiscal week.
* Note: /BIC/AZPOSCAL00 is the active ODS table for the
* POS Customer Calendar ODS (ZPOSCAL).
        select single /bic/zcfiscwk
          from /bic/azposcal00
          into l_fiscal_week
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Get date from customer fiscal week.
      select single calday
        from /bic/azposcal00
        into l_index_date
        where customer = l_customer and
              /bic/zcfiscwk = l_fiscal_week.

* Format date.
      concatenate l_index_date+4(2) '/'
                  l_index_date+6(2) '/'
                  l_index_date(4) into l_date.

* Return the date to the query.
      l_s_range-low    = l_date.
      l_s_range-high   = l_date.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 4 LY (Text): Text variable for POS reporting.
  when 'ZPOSWK4LYTXT'.
* After popup
    if i_step = 2.
      clear: l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_week,
             l_date.

* Get customer
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      l_customer = l_var_range-low.

* Get calendar week
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSWK4LY'.
      l_index_week = l_var_range-low.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer fiscal week.
* Note: /BIC/AZPOSCAL00 is the active ODS table for the
* POS Customer Calendar ODS (ZPOSCAL).
        select single /bic/zcfiscwk
          from /bic/azposcal00
          into l_fiscal_week
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Get date from customer fiscal week.
      select single calday
        from /bic/azposcal00
        into l_index_date
        where customer = l_customer and
              /bic/zcfiscwk = l_fiscal_week.

* Format date.
      concatenate l_index_date+4(2) '/'
                  l_index_date+6(2) '/'
                  l_index_date(4) into l_date.

* Return the date to the query.
      l_s_range-low    = l_date.
      l_s_range-high   = l_date.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 3 LY (Text): Text variable for POS reporting.
  when 'ZPOSWK3LYTXT'.
* After popup
    if i_step = 2.
      clear: l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_week,
             l_date.

* Get customer
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      l_customer = l_var_range-low.

* Get calendar week
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSWK3LY'.
      l_index_week = l_var_range-low.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer fiscal week.
* Note: /BIC/AZPOSCAL00 is the active ODS table for the
* POS Customer Calendar ODS (ZPOSCAL).
        select single /bic/zcfiscwk
          from /bic/azposcal00
          into l_fiscal_week
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Get date from customer fiscal week.
      select single calday
        from /bic/azposcal00
        into l_index_date
        where customer = l_customer and
              /bic/zcfiscwk = l_fiscal_week.

* Format date.
      concatenate l_index_date+4(2) '/'
                  l_index_date+6(2) '/'
                  l_index_date(4) into l_date.

* Return the date to the query.
      l_s_range-low    = l_date.
      l_s_range-high   = l_date.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 2 LY (Text): Text variable for POS reporting.
  when 'ZPOSWK2LYTXT'.
* After popup
    if i_step = 2.
      clear: l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_week,
             l_date.

* Get customer
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      l_customer = l_var_range-low.

* Get calendar week
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSWK2LY'.
      l_index_week = l_var_range-low.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer fiscal week.
* Note: /BIC/AZPOSCAL00 is the active ODS table for the
* POS Customer Calendar ODS (ZPOSCAL).
        select single /bic/zcfiscwk
          from /bic/azposcal00
          into l_fiscal_week
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Get date from customer fiscal week.
      select single calday
        from /bic/azposcal00
        into l_index_date
        where customer = l_customer and
              /bic/zcfiscwk = l_fiscal_week.

* Format date.
      concatenate l_index_date+4(2) '/'
                  l_index_date+6(2) '/'
                  l_index_date(4) into l_date.

* Return the date to the query.
      l_s_range-low    = l_date.
      l_s_range-high   = l_date.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* POS Week 1 LY (Text): Text variable for POS reporting.
  when 'ZPOSWK1LYTXT'.
* After popup
    if i_step = 2.
      clear: l_var_range,
             l_customer,
             l_index_date,
             l_index_week,
             l_first_day,
             l_last_day,
             l_fiscal_week,
             l_date.

* Get customer
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSCUST'.
      l_customer = l_var_range-low.

* Get calendar week
      clear l_var_range.
      read table i_t_var_range into l_var_range with key vnam = 'ZPOSWK1LY'.
      l_index_week = l_var_range-low.

* First day of the week.
      call function 'WEEK_GET_FIRST_DAY'
        exporting
          week         = l_index_week
        importing
          date         = l_first_day
        exceptions
          week_invalid = 1
          others       = 2.
* Last day of the week.
      l_last_day = l_first_day + 6.

* Get customer fiscal week.
* Note: /BIC/AZPOSCAL00 is the active ODS table for the
* POS Customer Calendar ODS (ZPOSCAL).
        select single /bic/zcfiscwk
          from /bic/azposcal00
          into l_fiscal_week
          where customer = l_customer and
                calday <= l_last_day and
                calday >= l_first_day.

* Get date from customer fiscal week.
      select single calday
        from /bic/azposcal00
        into l_index_date
        where customer = l_customer and
              /bic/zcfiscwk = l_fiscal_week.

* Format date.
      concatenate l_index_date+4(2) '/'
                  l_index_date+6(2) '/'
                  l_index_date(4) into l_date.

* Return the date to the query.
      l_s_range-low    = l_date.
      l_s_range-high   = l_date.
      l_s_range-sign   = 'I'.
      l_s_range-opt    = 'EQ'.
      append l_s_range to e_t_range.
    endif.

* End kevins 2/28/2014

endcase.


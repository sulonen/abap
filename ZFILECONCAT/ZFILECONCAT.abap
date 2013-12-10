*&---------------------------------------------------------------------*
*& Report  ZFILECONCAT
*&
*& Utility to concatenate mutliple EDI-collected files in FTP directory
*& on application server so that they can be loaded as a single file
*& by a BW InfoPackage.
*&
*&---------------------------------------------------------------------*
*& Created: kevins 12/2/2013
*&
*&---------------------------------------------------------------------*

report zfileconcat.

parameters: p_regx   type char30    lower case,
            p_dir    type btcxpgpar lower case,
            p_output type char30    lower case,
            p_arch   type btcxpgpar lower case.

*----------------------------------------------------------------------*
*       CLASS lcl_fileconcat DEFINITION
*----------------------------------------------------------------------*
class lcl_fileconcat definition create private.
  public section.

    types: begin of file_entry,
              create_date type d,
              filename type string,
           end of file_entry.

    data: wa_log    type btcxpm,
          log       type table of btcxpm,
          wa_file   type file_entry,
          file_list type table of file_entry.

    class-methods main importing im_regx      type char30
                                 im_directory type btcxpgpar
                                 im_output    type char30
                                 im_archive   type btcxpgpar
                                 returning value(re_obj) type ref to lcl_fileconcat.

    methods: get_list,
             concatenate_files,
             cleanup.

  private section.
    data: directory    type btcxpgpar,          " location of EDI files on app server
          archive      type btcxpgpar,          " location of archive files on app server
          regx         type string,             " regular expression for file ID
          output       type string,             " filename for generated file
          dset_in      type string,             " dataset for input files
          dset_out     type string,             " dataset for output file
          dset_archive type string,             " dataset for archive file
          line         type char512.            " line of text

    methods: constructor importing im_regx      type char30
                                   im_directory type btcxpgpar
                                   im_output    type char30
                                   im_archive   type btcxpgpar.

endclass.                    "lcl_fileconcat DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_fileconcat IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_fileconcat implementation.

  method constructor.
    clear: directory, regx, output, archive.

    me->directory = im_directory.
    me->regx      = im_regx.
    me->output    = im_output.
    me->archive   = im_archive.

    concatenate directory output into dset_out.
    concatenate archive sy-datum '.csv' into dset_archive.
  endmethod.                    "constructor

  method get_list.
* use external command ("ZLS" via SM69) to get file list
    call function 'SXPG_COMMAND_EXECUTE'
      exporting
        commandname                   = 'ZLS'
        additional_parameters         = directory
        operatingsystem               = sy-opsys
*       targetsystem                  = sy-host
*       destination                   =
        stdout                        = 'X'
        stderr                        = 'X'
*       terminationwait               = 'X'
*       trace                         =
*       dialog                        =
*       importing
*       status                        =
*       exitcode                      =
      tables
        exec_protocol                 = log
      exceptions
        no_permission                 = 1
        command_not_found             = 2
        parameters_too_long           = 3
        security_risk                 = 4
        wrong_check_call_interface    = 5
        program_start_error           = 6
        program_termination_error     = 7
        x_error                       = 8
        parameter_expected            = 9
        too_many_parameters           = 10
        illegal_command               = 11
        wrong_asynchronous_parameters = 12
        cant_enq_tbtco_entry          = 13
        jobcount_generation_error     = 14
        others                        = 15.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.

    clear file_list[].

    loop at log into wa_log from 2.
      clear wa_file.
      wa_file-create_date = wa_log-message+32(6).
      wa_file-filename = wa_log-message+45(83).

      find first occurrence of regex regx in wa_file-filename.
      if sy-subrc = 0.
        append wa_file to file_list.
      endif.
    endloop.
  endmethod.                    "get_list

  method concatenate_files.
* start with fresh InfoSet and Archive files
    delete dataset dset_out.
    open dataset dset_out for appending in text mode encoding default.
    open dataset dset_archive for appending in text mode encoding default.

    clear wa_file.
    loop at file_list into wa_file.
      clear dset_in.
      concatenate directory wa_file-filename into dset_in.
      open dataset dset_in for input in text mode encoding default.
      do.
        read dataset dset_in into line.
        if sy-subrc = 0.
* write line to both InfoSet and Archive files
          transfer line to dset_out.
          transfer line to dset_archive.
        else.
          exit.
        endif.
      enddo.
      close dataset dset_in.
    endloop.

    close dataset dset_out.
  endmethod.                    "concatenate_files

  method cleanup.
* delete EDI files
    clear: dset_in, wa_file.
    loop at file_list into wa_file.
      concatenate directory wa_file-filename into dset_in.
      delete dataset dset_in.
    endloop.
  endmethod.                    "cleanup

  method main.
    create object re_obj
      exporting
        im_directory = im_directory
        im_regx      = im_regx
        im_output    = im_output
        im_archive   = im_archive.
  endmethod.                    "main

endclass.                    "lcl_fileconcat IMPLEMENTATION

start-of-selection.
  data files type ref to lcl_fileconcat.
  files = lcl_fileconcat=>main( im_directory = p_dir
                                im_regx      = p_regx
                                im_output    = p_output
                                im_archive   = p_arch ).
  files->get_list( ).
  files->concatenate_files( ).
  files->cleanup( ).
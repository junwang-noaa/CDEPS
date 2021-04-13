module dshr_configread_mod

!!-------------------------------------------------------------------------------
!! read data model configuration
!! A "data stream" is a sequence of input files where each file
!! contains the same set of data fields and all the data fields
!! are on the same grid.
!! The sequence of input data files provides an uninterupted time
!! series of data.
!!-------------------------------------------------------------------------------

  use esmf             , only : ESMF_VM, ESMF_VMGetCurrent, ESMF_VMBroadCast
  use esmf             , only : ESMF_SUCCESS, ESMF_ConfigCreate, ESMF_ConfigLoadFile
  use esmf             , only : ESMF_ConfigGetLen, ESMF_ConfigGetAttribute
  use shr_sys_mod      , only : shr_sys_abort
  use dshr_methods_mod , only : chkerr

  public shr_configread

  subroutine shr_configread(conf_filename, streamdat, logunit,   &
                            pio_subsystem, io_type, io_format, rc)

    !!---------------------------------------------------------------------
    !! The configuration file is a text file that can have following entries
    !! file_id: "stream"
    !! file_version: 1.0
    !! stream_info: 1
    !! taxmode: 
    !! tInterpAlgo:
    !! readMode:
    !! mapalgo:
    !! dtlimit:
    !! yearFirst:
    !! yearLast:
    !! yearAlign:
    !! stream_vectors:
    !! stream_mesh_file:
    !! stream_lev_dimname:
    !! stream_data_files:
    !! stream_data_variables:
    !! stream_offset:
    !!---------------------------------------------------------------------
    
    ! input/output variables
    character(len=*), optional  , intent(in)             :: conf_filename
    type(shr_stream_streamType) , intent(inout), pointer :: streamdat(:)
    integer                     , intent(in)             :: logunit
    type(iosystem_desc_t)       , intent(in), pointer    :: pio_subsystem
    integer                     , intent(in)             :: io_type
    integer                     , intent(in)             :: io_format
    integer                     , intent(out)            :: rc

    ! local variables
    type(ESMF_VM)            :: vm
    type(ESMF_Config)        :: cf
    integer                  :: i, n, nstrms
    character(2)             :: mystrm
    character(*),parameter   :: subName = '(shr_configread)'
    character(len=ESMF_MAXSTR), allocatable :: strm_tmpstrings(:)
    character(*) , parameter :: u_FILE_u = __FILE__

    ! ---------------------------------------------------------------------

    rc = ESMF_SUCCESS

    nstrms = 0 
    
    ! allocate streamdat instance on all tasks
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    
    ! set ESMF config
    cf =  ESMF_ConfigCreate(rc=RC)
    call ESMF_ConfigLoadFile(config=CF ,filename=trim(conf_filename), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! get number of streams 
    nstrms = ESMF_ConfigGetLen(config=CF, label='stream_info:', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! allocate an array of shr_stream_streamtype objects on just isroot_task
    if( nstrms > 0 ) then
      allocate(streamdat(nstrms))
    else
      call shr_sys_abort("no stream_info in config file "//trim(conf_filename))
    endif

    ! fill in non-default values for the streamdat attributes
    do i=1, nstrms
      if( nstrms == 1 ) then
        mystrm=''
      else
        write(mystram,'("I2")') i
      endif 
      call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%taxmode,label="taxmode"//mystrm, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    
      call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%mapalgo,label="mapalgo"//mystrm, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%tInterpAlgo,label="tInterpAlgo"//mystrm, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%readMode,label="readMode"//mystrm, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      if( ESMF_ConfigGetLen(config=CF, label="yearFirst"//mystrm, rc=rc) > 0 ) then
        call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%yearFirst,label="yearFirst"//mystrm, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      else
        call shr_sys_abort("yearFirst must be provided")
      endif

      if( ESMF_ConfigGetLen(config=CF, label="yearLast"//mystrm, rc=rc) > 0 ) then
        call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%yearLast,label="yearLast"//mystrm, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      else
        call shr_sys_abort("yearLast must be provided")
      endif

      if( ESMF_ConfigGetLen(config=CF, label="yearAlign"//mystrm, rc=rc) > 0 ) then
        call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%yearAlign,label="yearAlign"//mystrm, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      else
        call shr_sys_abort("yearAlign must be provided")
      endif

      call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%dtlimit,label="dtlimit"//mystrm, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%offset,label="stream_offset"//mystrm, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      if( ESMF_ConfigGetLen(config=CF, label="stream_mesh_file"//mystrm, rc=rc) > 0 ) then
        call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%meshfile,label="stream_mesh_file"//mystrm, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      else
        call shr_sys_abort("stream_mesh_file must be provided")
      endif

      if( ESMF_ConfigGetLen(config=CF, label="stream_vectors"//mystrm, rc=rc) > 0 ) then
        call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%stream_vectors,label="stream_vectors"//mystrm, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      else
        call shr_sys_abort("stream_vectors must be provided")
      endif

      if( ESMF_ConfigGetLen(config=CF, label="stream_lev_dimname"//mystrm, rc=rc) > 0 ) then
        call ESMF_ConfigGetAttribute(CF,value=streamdat(i)%lev_dimname,label="stream_lev_dimname"//mystrm, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      else
        call shr_sys_abort("stream_lev_dimname must be provided")
      endif

      ! Get a list of stream file names
      streamdat(i)%nfiles = ESMF_ConfigGetLen(config=CF, label="stream_data_files"//mystrm, rc=rc)
      if( streamdat(i)%nfiles > 0) then
        allocate(streamdat(i)%file( streamdat(i)%nfiles))
        allocate(strm_tmpstrings(streamdat(i)%nfiles))
        call ESMF_ConfigGetAttribute(CF,valueList=strm_tmpstrings, label="stream_data_files"//mystrm, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        do n=1,streamdat(i)%nfiles
          streamdat(i)%file(n)%name = trim(strm_tmpstrings)
        enddo
        deallocate(strm_tmpstrings)
      else
        call shr_sys_abort("stream data files must be provided")
      endif

      ! Get name of stream variables in file and model
      streamdat(i)%nvars = ESMF_ConfigGetLen(config=CF, label="stream_data_variables"//mystrm, rc=rc) 
      if( streamdat(i)%nvars > 0) then
        allocate(streamdat(i)%varlist(streamdat(i)%nvars))
        allocate(strm_tmpstrings(streamdat(i)%nvars))
        call ESMF_ConfigGetAttribute(CF,valueList=strm_tmpstrings,label="stream_data_variables"//mystrm, rc=rc)
        do n=1, streamdat(i)%nvars
          streamdat(i)%varlist(n)%nameinfile = strm_tmpstrings(n)(1:index(tmpstr, " ")) 
          streamdat(i)%varlist(n)%nameinmodel = strm_tmpstrings(n)index(trim(tmpstr), " ", .true.)+1:)
        enddo
      else
        call shr_sys_abort("stream data variables must be provided")
      endif

      ! Initialize stream pio
      streamdat(i)%pio_subsystem => pio_subsystem
      streamdat(i)%pio_iotype = io_type
      streamdat(i)%pio_ioformat = io_format
      call shr_stream_getCalendar(streamdat(i), 1, streamdat(i)%calendar)   

      ! Error check
      if (trim(streamdat(i)%taxmode) == shr_stream_taxis_extend .and. streamdat(i)%dtlimit < 1.e10) then
        call shr_sys_abort(trim(subName)//" ERROR: if taxmode value is extend set dtlimit to 1.e30")
      end if

    enddo ! end loop nstrm

    ! Set logunit
    streamdat(:)%logunit = logunit

    ! initialize flag that stream has been set
    streamdat(:)%init = .true.

  end subroutine shr_configread

end module dshr_configread_mod

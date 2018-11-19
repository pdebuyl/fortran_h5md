
  subroutine h5md_element_create_time_is(this, loc, name, data, mode, step, time, chunks, step_offset, time_offset)
    integer, parameter :: rank = 1
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer, intent(in) :: data
    integer, intent(in) :: mode
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time
    integer, intent(in), optional :: chunks(rank)
    integer, intent(in), optional :: step_offset
    double precision, intent(in), optional :: time_offset

    integer(HSIZE_T) :: dims(rank), maxdims(rank), chunk_dims(rank)
    integer(HID_T) :: s, plist, object_id

    call h5gcreate_f(loc, name, this% id, this% error)
    call h5md_check_valid(this% id, 'invalid id in create_time')


    dims(rank) = 0
    maxdims(rank) = H5S_UNLIMITED_F

    
    if (present(chunks)) then
      chunk_dims = chunks
    else
      chunk_dims = chunking(dims, maxdims)
    end if

    call h5screate_simple_f(rank, dims, s, this% error, maxdims)

    call h5pcreate_f(H5P_DATASET_CREATE_F, plist, this% error)
    call h5pset_chunk_f(plist, rank, chunk_dims, this% error)
    call h5dcreate_f(this% id, 'value', H5T_NATIVE_INTEGER, s, this% v, this% error, plist)
    call h5pclose_f(plist, this% error)
    call h5sclose_f(s, this% error)

    if (iand(mode, H5MD_LINEAR) == H5MD_LINEAR) then
      if (.not. present(step)) error stop 'no step in h5md_element_create_time'
      if (iand(mode, H5MD_STORE_TIME) == H5MD_STORE_TIME) then
          if (.not. present(time)) error stop 'no step in h5md_element_create_time'
      end if
    end if

    call h5md_populate_step_time(this, mode, step, time)

    if (present(step_offset)) then
      call h5oopen_f(this%id, 'step', object_id, this%error)
      call h5md_write_attribute(object_id, 'offset', step_offset)
      call h5oclose_f(object_id, this%error)
    end if

    if (present(time_offset)) then
      call h5oopen_f(this%id, 'time', object_id, this%error)
      call h5md_write_attribute(object_id, 'offset', time_offset)
      call h5oclose_f(object_id, this%error)
    end if

  end subroutine h5md_element_create_time_is


  subroutine h5md_element_create_time_i1(this, loc, name, data, mode, step, time, chunks, step_offset, time_offset)
    integer, parameter :: rank = 2
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer, intent(in) :: data(:)
    integer, intent(in) :: mode
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time
    integer, intent(in), optional :: chunks(rank)
    integer, intent(in), optional :: step_offset
    double precision, intent(in), optional :: time_offset

    integer(HSIZE_T) :: dims(rank), maxdims(rank), chunk_dims(rank)
    integer(HID_T) :: s, plist, object_id

    call h5gcreate_f(loc, name, this% id, this% error)
    call h5md_check_valid(this% id, 'invalid id in create_time')


    dims(1:rank-1) = shape(data)
    dims(rank) = 0
    maxdims(1:rank-1) = shape(data)
    maxdims(rank) = H5S_UNLIMITED_F

    
    if (present(chunks)) then
      chunk_dims = chunks
    else
      chunk_dims = chunking(dims, maxdims)
    end if

    call h5screate_simple_f(rank, dims, s, this% error, maxdims)

    call h5pcreate_f(H5P_DATASET_CREATE_F, plist, this% error)
    call h5pset_chunk_f(plist, rank, chunk_dims, this% error)
    call h5dcreate_f(this% id, 'value', H5T_NATIVE_INTEGER, s, this% v, this% error, plist)
    call h5pclose_f(plist, this% error)
    call h5sclose_f(s, this% error)

    if (iand(mode, H5MD_LINEAR) == H5MD_LINEAR) then
      if (.not. present(step)) error stop 'no step in h5md_element_create_time'
      if (iand(mode, H5MD_STORE_TIME) == H5MD_STORE_TIME) then
          if (.not. present(time)) error stop 'no step in h5md_element_create_time'
      end if
    end if

    call h5md_populate_step_time(this, mode, step, time)

    if (present(step_offset)) then
      call h5oopen_f(this%id, 'step', object_id, this%error)
      call h5md_write_attribute(object_id, 'offset', step_offset)
      call h5oclose_f(object_id, this%error)
    end if

    if (present(time_offset)) then
      call h5oopen_f(this%id, 'time', object_id, this%error)
      call h5md_write_attribute(object_id, 'offset', time_offset)
      call h5oclose_f(object_id, this%error)
    end if

  end subroutine h5md_element_create_time_i1


  subroutine h5md_element_create_time_i2(this, loc, name, data, mode, step, time, chunks, step_offset, time_offset)
    integer, parameter :: rank = 3
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer, intent(in) :: data(:,:)
    integer, intent(in) :: mode
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time
    integer, intent(in), optional :: chunks(rank)
    integer, intent(in), optional :: step_offset
    double precision, intent(in), optional :: time_offset

    integer(HSIZE_T) :: dims(rank), maxdims(rank), chunk_dims(rank)
    integer(HID_T) :: s, plist, object_id

    call h5gcreate_f(loc, name, this% id, this% error)
    call h5md_check_valid(this% id, 'invalid id in create_time')


    dims(1:rank-1) = shape(data)
    dims(rank) = 0
    maxdims(1:rank-1) = shape(data)
    maxdims(rank) = H5S_UNLIMITED_F

    
    if (present(chunks)) then
      chunk_dims = chunks
    else
      chunk_dims = chunking(dims, maxdims)
    end if

    call h5screate_simple_f(rank, dims, s, this% error, maxdims)

    call h5pcreate_f(H5P_DATASET_CREATE_F, plist, this% error)
    call h5pset_chunk_f(plist, rank, chunk_dims, this% error)
    call h5dcreate_f(this% id, 'value', H5T_NATIVE_INTEGER, s, this% v, this% error, plist)
    call h5pclose_f(plist, this% error)
    call h5sclose_f(s, this% error)

    if (iand(mode, H5MD_LINEAR) == H5MD_LINEAR) then
      if (.not. present(step)) error stop 'no step in h5md_element_create_time'
      if (iand(mode, H5MD_STORE_TIME) == H5MD_STORE_TIME) then
          if (.not. present(time)) error stop 'no step in h5md_element_create_time'
      end if
    end if

    call h5md_populate_step_time(this, mode, step, time)

    if (present(step_offset)) then
      call h5oopen_f(this%id, 'step', object_id, this%error)
      call h5md_write_attribute(object_id, 'offset', step_offset)
      call h5oclose_f(object_id, this%error)
    end if

    if (present(time_offset)) then
      call h5oopen_f(this%id, 'time', object_id, this%error)
      call h5md_write_attribute(object_id, 'offset', time_offset)
      call h5oclose_f(object_id, this%error)
    end if

  end subroutine h5md_element_create_time_i2


  subroutine h5md_element_create_time_i3(this, loc, name, data, mode, step, time, chunks, step_offset, time_offset)
    integer, parameter :: rank = 4
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer, intent(in) :: data(:,:,:)
    integer, intent(in) :: mode
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time
    integer, intent(in), optional :: chunks(rank)
    integer, intent(in), optional :: step_offset
    double precision, intent(in), optional :: time_offset

    integer(HSIZE_T) :: dims(rank), maxdims(rank), chunk_dims(rank)
    integer(HID_T) :: s, plist, object_id

    call h5gcreate_f(loc, name, this% id, this% error)
    call h5md_check_valid(this% id, 'invalid id in create_time')


    dims(1:rank-1) = shape(data)
    dims(rank) = 0
    maxdims(1:rank-1) = shape(data)
    maxdims(rank) = H5S_UNLIMITED_F

    
    if (present(chunks)) then
      chunk_dims = chunks
    else
      chunk_dims = chunking(dims, maxdims)
    end if

    call h5screate_simple_f(rank, dims, s, this% error, maxdims)

    call h5pcreate_f(H5P_DATASET_CREATE_F, plist, this% error)
    call h5pset_chunk_f(plist, rank, chunk_dims, this% error)
    call h5dcreate_f(this% id, 'value', H5T_NATIVE_INTEGER, s, this% v, this% error, plist)
    call h5pclose_f(plist, this% error)
    call h5sclose_f(s, this% error)

    if (iand(mode, H5MD_LINEAR) == H5MD_LINEAR) then
      if (.not. present(step)) error stop 'no step in h5md_element_create_time'
      if (iand(mode, H5MD_STORE_TIME) == H5MD_STORE_TIME) then
          if (.not. present(time)) error stop 'no step in h5md_element_create_time'
      end if
    end if

    call h5md_populate_step_time(this, mode, step, time)

    if (present(step_offset)) then
      call h5oopen_f(this%id, 'step', object_id, this%error)
      call h5md_write_attribute(object_id, 'offset', step_offset)
      call h5oclose_f(object_id, this%error)
    end if

    if (present(time_offset)) then
      call h5oopen_f(this%id, 'time', object_id, this%error)
      call h5md_write_attribute(object_id, 'offset', time_offset)
      call h5oclose_f(object_id, this%error)
    end if

  end subroutine h5md_element_create_time_i3


  subroutine h5md_element_create_time_ds(this, loc, name, data, mode, step, time, chunks, step_offset, time_offset)
    integer, parameter :: rank = 1
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: data
    integer, intent(in) :: mode
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time
    integer, intent(in), optional :: chunks(rank)
    integer, intent(in), optional :: step_offset
    double precision, intent(in), optional :: time_offset

    integer(HSIZE_T) :: dims(rank), maxdims(rank), chunk_dims(rank)
    integer(HID_T) :: s, plist, object_id

    call h5gcreate_f(loc, name, this% id, this% error)
    call h5md_check_valid(this% id, 'invalid id in create_time')


    dims(rank) = 0
    maxdims(rank) = H5S_UNLIMITED_F

    
    if (present(chunks)) then
      chunk_dims = chunks
    else
      chunk_dims = chunking(dims, maxdims)
    end if

    call h5screate_simple_f(rank, dims, s, this% error, maxdims)

    call h5pcreate_f(H5P_DATASET_CREATE_F, plist, this% error)
    call h5pset_chunk_f(plist, rank, chunk_dims, this% error)
    call h5dcreate_f(this% id, 'value', H5T_NATIVE_DOUBLE, s, this% v, this% error, plist)
    call h5pclose_f(plist, this% error)
    call h5sclose_f(s, this% error)

    if (iand(mode, H5MD_LINEAR) == H5MD_LINEAR) then
      if (.not. present(step)) error stop 'no step in h5md_element_create_time'
      if (iand(mode, H5MD_STORE_TIME) == H5MD_STORE_TIME) then
          if (.not. present(time)) error stop 'no step in h5md_element_create_time'
      end if
    end if

    call h5md_populate_step_time(this, mode, step, time)

    if (present(step_offset)) then
      call h5oopen_f(this%id, 'step', object_id, this%error)
      call h5md_write_attribute(object_id, 'offset', step_offset)
      call h5oclose_f(object_id, this%error)
    end if

    if (present(time_offset)) then
      call h5oopen_f(this%id, 'time', object_id, this%error)
      call h5md_write_attribute(object_id, 'offset', time_offset)
      call h5oclose_f(object_id, this%error)
    end if

  end subroutine h5md_element_create_time_ds


  subroutine h5md_element_create_time_d1(this, loc, name, data, mode, step, time, chunks, step_offset, time_offset)
    integer, parameter :: rank = 2
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: data(:)
    integer, intent(in) :: mode
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time
    integer, intent(in), optional :: chunks(rank)
    integer, intent(in), optional :: step_offset
    double precision, intent(in), optional :: time_offset

    integer(HSIZE_T) :: dims(rank), maxdims(rank), chunk_dims(rank)
    integer(HID_T) :: s, plist, object_id

    call h5gcreate_f(loc, name, this% id, this% error)
    call h5md_check_valid(this% id, 'invalid id in create_time')


    dims(1:rank-1) = shape(data)
    dims(rank) = 0
    maxdims(1:rank-1) = shape(data)
    maxdims(rank) = H5S_UNLIMITED_F

    
    if (present(chunks)) then
      chunk_dims = chunks
    else
      chunk_dims = chunking(dims, maxdims)
    end if

    call h5screate_simple_f(rank, dims, s, this% error, maxdims)

    call h5pcreate_f(H5P_DATASET_CREATE_F, plist, this% error)
    call h5pset_chunk_f(plist, rank, chunk_dims, this% error)
    call h5dcreate_f(this% id, 'value', H5T_NATIVE_DOUBLE, s, this% v, this% error, plist)
    call h5pclose_f(plist, this% error)
    call h5sclose_f(s, this% error)

    if (iand(mode, H5MD_LINEAR) == H5MD_LINEAR) then
      if (.not. present(step)) error stop 'no step in h5md_element_create_time'
      if (iand(mode, H5MD_STORE_TIME) == H5MD_STORE_TIME) then
          if (.not. present(time)) error stop 'no step in h5md_element_create_time'
      end if
    end if

    call h5md_populate_step_time(this, mode, step, time)

    if (present(step_offset)) then
      call h5oopen_f(this%id, 'step', object_id, this%error)
      call h5md_write_attribute(object_id, 'offset', step_offset)
      call h5oclose_f(object_id, this%error)
    end if

    if (present(time_offset)) then
      call h5oopen_f(this%id, 'time', object_id, this%error)
      call h5md_write_attribute(object_id, 'offset', time_offset)
      call h5oclose_f(object_id, this%error)
    end if

  end subroutine h5md_element_create_time_d1


  subroutine h5md_element_create_time_d2(this, loc, name, data, mode, step, time, chunks, step_offset, time_offset)
    integer, parameter :: rank = 3
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: data(:,:)
    integer, intent(in) :: mode
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time
    integer, intent(in), optional :: chunks(rank)
    integer, intent(in), optional :: step_offset
    double precision, intent(in), optional :: time_offset

    integer(HSIZE_T) :: dims(rank), maxdims(rank), chunk_dims(rank)
    integer(HID_T) :: s, plist, object_id

    call h5gcreate_f(loc, name, this% id, this% error)
    call h5md_check_valid(this% id, 'invalid id in create_time')


    dims(1:rank-1) = shape(data)
    dims(rank) = 0
    maxdims(1:rank-1) = shape(data)
    maxdims(rank) = H5S_UNLIMITED_F

    
    if (present(chunks)) then
      chunk_dims = chunks
    else
      chunk_dims = chunking(dims, maxdims)
    end if

    call h5screate_simple_f(rank, dims, s, this% error, maxdims)

    call h5pcreate_f(H5P_DATASET_CREATE_F, plist, this% error)
    call h5pset_chunk_f(plist, rank, chunk_dims, this% error)
    call h5dcreate_f(this% id, 'value', H5T_NATIVE_DOUBLE, s, this% v, this% error, plist)
    call h5pclose_f(plist, this% error)
    call h5sclose_f(s, this% error)

    if (iand(mode, H5MD_LINEAR) == H5MD_LINEAR) then
      if (.not. present(step)) error stop 'no step in h5md_element_create_time'
      if (iand(mode, H5MD_STORE_TIME) == H5MD_STORE_TIME) then
          if (.not. present(time)) error stop 'no step in h5md_element_create_time'
      end if
    end if

    call h5md_populate_step_time(this, mode, step, time)

    if (present(step_offset)) then
      call h5oopen_f(this%id, 'step', object_id, this%error)
      call h5md_write_attribute(object_id, 'offset', step_offset)
      call h5oclose_f(object_id, this%error)
    end if

    if (present(time_offset)) then
      call h5oopen_f(this%id, 'time', object_id, this%error)
      call h5md_write_attribute(object_id, 'offset', time_offset)
      call h5oclose_f(object_id, this%error)
    end if

  end subroutine h5md_element_create_time_d2


  subroutine h5md_element_create_time_d3(this, loc, name, data, mode, step, time, chunks, step_offset, time_offset)
    integer, parameter :: rank = 4
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: data(:,:,:)
    integer, intent(in) :: mode
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time
    integer, intent(in), optional :: chunks(rank)
    integer, intent(in), optional :: step_offset
    double precision, intent(in), optional :: time_offset

    integer(HSIZE_T) :: dims(rank), maxdims(rank), chunk_dims(rank)
    integer(HID_T) :: s, plist, object_id

    call h5gcreate_f(loc, name, this% id, this% error)
    call h5md_check_valid(this% id, 'invalid id in create_time')


    dims(1:rank-1) = shape(data)
    dims(rank) = 0
    maxdims(1:rank-1) = shape(data)
    maxdims(rank) = H5S_UNLIMITED_F

    
    if (present(chunks)) then
      chunk_dims = chunks
    else
      chunk_dims = chunking(dims, maxdims)
    end if

    call h5screate_simple_f(rank, dims, s, this% error, maxdims)

    call h5pcreate_f(H5P_DATASET_CREATE_F, plist, this% error)
    call h5pset_chunk_f(plist, rank, chunk_dims, this% error)
    call h5dcreate_f(this% id, 'value', H5T_NATIVE_DOUBLE, s, this% v, this% error, plist)
    call h5pclose_f(plist, this% error)
    call h5sclose_f(s, this% error)

    if (iand(mode, H5MD_LINEAR) == H5MD_LINEAR) then
      if (.not. present(step)) error stop 'no step in h5md_element_create_time'
      if (iand(mode, H5MD_STORE_TIME) == H5MD_STORE_TIME) then
          if (.not. present(time)) error stop 'no step in h5md_element_create_time'
      end if
    end if

    call h5md_populate_step_time(this, mode, step, time)

    if (present(step_offset)) then
      call h5oopen_f(this%id, 'step', object_id, this%error)
      call h5md_write_attribute(object_id, 'offset', step_offset)
      call h5oclose_f(object_id, this%error)
    end if

    if (present(time_offset)) then
      call h5oopen_f(this%id, 'time', object_id, this%error)
      call h5md_write_attribute(object_id, 'offset', time_offset)
      call h5oclose_f(object_id, this%error)
    end if

  end subroutine h5md_element_create_time_d3


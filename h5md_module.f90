! This file is part of fortran_h5md
! Copyright 2015 Pierre de Buyl
! License: BSD

module h5md_module
  use hdf5
  implicit none

  private

  public :: h5md_file_t, h5md_element_t
  public :: h5md_check_valid, h5md_check_exists
  public :: h5md_write_attribute
  public :: h5md_read_attribute
  public :: h5md_write_dataset
  public :: H5MD_FIXED, H5MD_TIME, H5MD_LINEAR, H5MD_STORE_TIME

  integer, parameter :: H5MD_FIXED = 1
  integer, parameter :: H5MD_TIME = 2
  integer, parameter :: H5MD_LINEAR = 4
  integer, parameter :: H5MD_STORE_TIME = 8

  type h5md_file_t
     integer(HID_T) :: id
     integer(HID_T) :: particles
     integer(HID_T) :: observables
     integer(HID_T) :: connectivity
     integer(HID_T) :: parameters
     integer :: error
   contains
     procedure :: create => h5md_file_create
     procedure :: open => h5md_file_open
     procedure :: close => h5md_file_close
  end type h5md_file_t

  type h5md_element_t
     integer :: type
     integer(HID_T) :: id
     integer(HID_T) :: v
     integer(HID_T) :: s
     integer(HID_T) :: t
     logical :: has_time
     integer :: error
     integer :: Nmax
   contains
     generic, public :: create_fixed => h5md_element_create_fixed_d2, h5md_element_create_fixed_d1, &
          h5md_element_create_fixed_i1, h5md_element_create_fixed_i2, h5md_element_create_fixed_d3, &
          h5md_element_create_fixed_d4
     procedure, private :: h5md_element_create_fixed_d4
     procedure, private :: h5md_element_create_fixed_d3
     procedure, private :: h5md_element_create_fixed_d2
     procedure, private :: h5md_element_create_fixed_d1
     procedure, private :: h5md_element_create_fixed_i1
     procedure, private :: h5md_element_create_fixed_i2
     generic, public :: read_fixed => h5md_element_read_fixed_d2, h5md_element_read_fixed_i1
     procedure, private :: h5md_element_read_fixed_d2
     procedure, private :: h5md_element_read_fixed_i1
     procedure :: open_time => h5md_element_open_time
     generic, public :: create_time => &
          h5md_element_create_time_ds, h5md_element_create_time_d1, &
          h5md_element_create_time_d2, h5md_element_create_time_d3, &
          h5md_element_create_time_is, h5md_element_create_time_i1, &
          h5md_element_create_time_i2, h5md_element_create_time_i3
     procedure, private :: h5md_element_create_time_ds
     procedure, private :: h5md_element_create_time_d1
     procedure, private :: h5md_element_create_time_d2
     procedure, private :: h5md_element_create_time_d3
     procedure, private :: h5md_element_create_time_is
     procedure, private :: h5md_element_create_time_i1
     procedure, private :: h5md_element_create_time_i2
     procedure, private :: h5md_element_create_time_i3
     generic, public :: append => h5md_element_append_d2, h5md_element_append_d1, &
          h5md_element_append_ds, h5md_element_append_i1, h5md_element_append_i2, &
          h5md_element_append_i3, h5md_element_append_d3
     procedure, private :: h5md_element_append_d2
     procedure, private :: h5md_element_append_d1
     procedure, private :: h5md_element_append_ds
     procedure, private :: h5md_element_append_i1
     procedure, private :: h5md_element_append_i2
     procedure, private :: h5md_element_append_i3
     procedure, private :: h5md_element_append_d3
     generic, public :: append_buffer => h5md_element_append_buffer_s, h5md_element_append_buffer_d1
     procedure, private :: h5md_element_append_buffer_s
     procedure, private :: h5md_element_append_buffer_d1
     procedure :: close => h5md_element_close
  end type h5md_element_t

  interface h5md_write_attribute
     module procedure h5md_write_attribute_cs
     module procedure h5md_write_attribute_c1
     module procedure h5md_write_attribute_is
     module procedure h5md_write_attribute_i1
     module procedure h5md_write_attribute_ds
  end interface h5md_write_attribute

  interface h5md_read_attribute
     module procedure h5md_read_attribute_ds
  end interface h5md_read_attribute

  interface h5md_write_dataset
     module procedure h5md_write_dataset_ds
     module procedure h5md_write_dataset_d2
     module procedure h5md_write_dataset_i2
     module procedure h5md_write_dataset_i1
     module procedure h5md_write_dataset_d3
     module procedure h5md_write_dataset_d4
     module procedure h5md_write_dataset_is
  end interface h5md_write_dataset

contains

  subroutine h5md_file_create(this, filename, creator, creator_version, author, author_email)
    class(h5md_file_t), intent(out) :: this
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: creator, creator_version, author
    character(len=*), intent(in), optional :: author_email

    integer(HID_T) :: g1, g2

    call h5fcreate_f(filename, H5F_ACC_TRUNC_F, this% id, this% error)

    call h5gcreate_f(this% id, 'h5md', g1, this% error)
    call h5md_write_attribute(g1, 'version', [1, 1])
    call h5gcreate_f(g1, 'author', g2, this% error)
    call h5md_write_attribute(g2, 'name', author)
    if (present(author_email)) then
       call h5md_write_attribute(g2, 'email', author_email)
    end if
    call h5gclose_f(g2, this% error)
    call h5gcreate_f(g1, 'creator', g2, this% error)
    call h5md_write_attribute(g2, 'name', creator)
    call h5md_write_attribute(g2, 'version', creator_version)
    call h5gclose_f(g2, this% error)
    call h5gclose_f(g1, this% error)

    call h5gcreate_f(this% id, 'particles', this% particles, this% error)
    call h5gcreate_f(this% id, 'observables', this%observables, this% error)

  end subroutine h5md_file_create

  subroutine h5md_file_open(this, filename, flags)
    class(h5md_file_t), intent(out) :: this
    character(len=*), intent(in) :: filename
    integer, intent(in) :: flags

    integer(HID_T) :: g1
    integer(HID_T) :: att, space
    logical :: flag, link_exists
    integer :: rank, version(2)
    integer(HSIZE_T) :: dims(1), maxdims(1)

    call h5fopen_f(filename, flags, this% id, this% error)
    call check_error(this% error, 'error opening the file '//filename)

    call h5oexists_by_name_f(this% id, 'h5md', link_exists, this% error)
    if (.not. link_exists) stop 'h5md group not found'
    call h5gopen_f(this% id, 'h5md', g1, this% error)
    call check_error(this% error, '/h5md group not found in H5MD file')

    call h5aopen_f(g1, 'version', att, this% error)
    call check_error(this% error, 'h5md version not found in H5MD file')
    call h5aget_space_f(att, space, this% error)
    call h5sis_simple_f(space, flag, this% error)
    if (.not. flag) then
       write(*,*) 'h5md version not a simple dataspace'
       stop
    end if
    call h5sget_simple_extent_ndims_f(space, rank, this% error)
    if (rank /= 1) then
       write(*,*) 'h5md version not of rank 1'
       stop
    end if
    call h5sget_simple_extent_dims_f(space, dims, maxdims, this% error)
    if (dims(1) /= 2) then
       write(*,*) 'h5md version not of length 2'
       stop
    end if
    call h5aread_f(att, H5T_NATIVE_INTEGER, version, dims, this% error)
    call h5aclose_f(att, this% error)
    call h5sclose_f(space, this% error)
    call h5gclose_f(g1, this% error)

    if (version(1) /= 1) then
       stop 'unsupported H5MD version'
    end if
    if ( (version(2) /= 0) .and. (version(2) /= 1) ) then
       stop 'unsupported H5MD version'
    end if

    call h5lexists_f(this% id, 'particles', link_exists, this% error)

    if (.not. link_exists) then
       write(*,*) 'particles group not found'
    else
       call h5gopen_f(this% id, 'particles', this% particles, this% error)
       call check_error(this% error, '/particles group opening error')
    end if

  end subroutine h5md_file_open

  subroutine h5md_file_close(this)
    class(h5md_file_t), intent(inout) :: this

    logical :: valid

    call h5iis_valid_f(this% particles, valid, this% error)
    if (valid) then
       call h5gclose_f(this% particles, this% error)
    end if
    call h5fclose_f(this% id, this% error)
    call check_error(this% error, 'file close error')

  end subroutine h5md_file_close

  subroutine h5md_element_close(this)
    class(h5md_element_t), intent(inout) :: this

    logical :: valid

    if (this% type == H5MD_FIXED) then
       return
    else
       call h5iis_valid_f(this% id, valid, this% error)
       if (valid) then
          call h5gclose_f(this% id, this% error)
       end if
       call h5iis_valid_f(this% s, valid, this% error)
       if (valid) then
          call h5dclose_f(this% s, this% error)
       end if
       call h5iis_valid_f(this% t, valid, this% error)
       if (valid) then
          call h5dclose_f(this% t, this% error)
       end if
       call h5iis_valid_f(this% v, valid, this% error)
       if (valid) then
          call h5dclose_f(this% v, this% error)
       end if
    end if

  end subroutine h5md_element_close

  function chunking(dims, maxdims) result(c)
    integer(HSIZE_T), intent(in) :: dims(:)
    integer(HSIZE_T), intent(in) :: maxdims(:)
    integer(HSIZE_T) :: c(size(dims))

    integer :: rank, i

    rank = size(dims)
    if (rank>1) then
       c(1:rank-1) = dims(1:rank-1)
       do i = 1, rank-1
          c(i) = min(c(i), 512_HSIZE_T)
       end do
       c(rank) = max(1_HSIZE_T, 512_HSIZE_T*1024_HSIZE_T/product(c(1:rank-1)))
    else
       c(rank) = 2048
    end if

  end function chunking

  subroutine h5md_populate_step_time(elem, mode, step, time)
    type(h5md_element_t), intent(inout) :: elem
    integer, intent(in) :: mode
    integer, intent(in) :: step
    double precision, intent(in) :: time

    integer(HID_T) :: s, plist
    integer(HSIZE_T) :: dims(1), maxdims(1), chunk_dims(1)

    if (iand(mode, H5MD_TIME) == H5MD_TIME) then
       elem%type = H5MD_TIME
       dims(1) = 0
       maxdims(1) = H5S_UNLIMITED_F
       chunk_dims(1) = 2048
       call h5screate_simple_f(1, dims, s, elem%error, maxdims)
       call h5pcreate_f(H5P_DATASET_CREATE_F, plist, elem%error)
       call h5pset_chunk_f(plist, 1, chunk_dims, elem%error)
       call h5dcreate_f(elem%id, 'step', H5T_NATIVE_INTEGER, s, elem%s, elem%error, plist)
       if (iand(mode, H5MD_STORE_TIME) == H5MD_STORE_TIME) then
          call h5dcreate_f(elem%id, 'time', H5T_NATIVE_DOUBLE, s, elem%t, elem%error, plist)
          elem%has_time = .true.
       else
          elem%has_time = .false.
       end if
       call h5pclose_f(plist, elem%error)
       call h5sclose_f(s, elem%error)
    else if (iand(mode,H5MD_LINEAR) == H5MD_LINEAR) then
       elem%type = H5MD_LINEAR
       call h5screate_f(H5S_SCALAR_F, s, elem%error)
       call h5dcreate_f(elem%id, 'step', H5T_NATIVE_INTEGER, s, elem%s, elem%error)
       call h5dwrite_f(elem%s, H5T_NATIVE_INTEGER, step, dims, elem%error, H5S_ALL_F, s)
       call h5dclose_F(elem%s, elem%error)
       elem%has_time = (iand(mode,H5MD_STORE_TIME)==H5MD_STORE_TIME)
       if (elem%has_time) then
          call h5dcreate_f(elem%id, 'time', H5T_NATIVE_DOUBLE, s, elem%t, elem%error)
          call h5dwrite_f(elem%t, H5T_NATIVE_DOUBLE, time, dims, elem%error, H5S_ALL_F, s)
          call h5dclose_F(elem%t, elem%error)
       end if
       call h5sclose_f(s, elem%error)
    end if

  end subroutine h5md_populate_step_time

  include 'h5md_element_create_time.f90'

  subroutine h5md_element_append_d2(this, data, step, time)
    class(h5md_element_t), intent(inout) :: this
    double precision, intent(in) :: data(:,:)
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time

    integer, parameter :: rank=3
    integer :: r
    integer(HID_T) :: s, mem_s
    integer(HSIZE_T) :: dims(rank), maxdims(rank), start(rank), select_count(rank)

    if (this% type == H5MD_FIXED) return

    dims(1:rank-1) = shape(data)
    call h5screate_simple_f(rank-1, dims, mem_s, this% error)

    call h5md_extend(this% v, r, dims, maxdims)
    call check_true((r == rank), 'invalid rank for v in append')
    call h5dget_space_f(this% v, s, this% error)
    start = 0
    start(3) = dims(3)-1
    select_count = dims
    select_count(3) = 1
    call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
    call h5dwrite_f(this% v, H5T_NATIVE_DOUBLE, data, select_count, this% error, mem_s, s)
    call h5sclose_f(s, this% error)
    call h5sclose_f(mem_s, this% error)

    if (this% type == H5MD_TIME) then
       dims(1) = 1
       call h5screate_simple_f(1, dims, mem_s, this% error)
       call h5md_extend(this% s, r, dims, maxdims)
       call h5dget_space_f(this% s, s, this% error)
       start(1) = dims(1)-1
       select_count = 1
       call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
       call h5dwrite_f(this% s, H5T_NATIVE_INTEGER, step, select_count, this% error, mem_s, s)
       call h5sclose_f(s, this% error)

       if (present(time) .and. this% has_time) then
          call h5md_extend(this% t, r, dims, maxdims)
          call h5dget_space_f(this% t, s, this% error)
          start(1) = dims(1)-1
          select_count = 1
          call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
          call h5dwrite_f(this% t, H5T_NATIVE_DOUBLE, time, select_count, this% error, mem_s, s)
          call h5sclose_f(s, this% error)
       end if
       call h5sclose_f(mem_s, this% error)
    end if

  end subroutine h5md_element_append_d2

  subroutine h5md_element_append_d1(this, data, step, time)
    class(h5md_element_t), intent(inout) :: this
    double precision, intent(in) :: data(:)
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time

    integer, parameter :: rank=2
    integer :: r
    integer(HID_T) :: s, mem_s
    integer(HSIZE_T) :: dims(rank), maxdims(rank), start(rank), select_count(rank)

    if (this% type == H5MD_FIXED) return

    dims(1:rank-1) = shape(data)
    call h5screate_simple_f(rank-1, dims, mem_s, this% error)

    call h5md_extend(this% v, r, dims, maxdims)
    call check_true((r == rank), 'invalid rank for v in append')
    call h5dget_space_f(this% v, s, this% error)
    start = 0
    start(rank) = dims(rank)-1
    select_count = dims
    select_count(rank) = 1
    call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
    call h5dwrite_f(this% v, H5T_NATIVE_DOUBLE, data, select_count, this% error, mem_s, s)
    call h5sclose_f(s, this% error)
    call h5sclose_f(mem_s, this% error)

    if (this% type == H5MD_TIME) then
       dims(1) = 1
       call h5screate_simple_f(1, dims, mem_s, this% error)
       call h5md_extend(this% s, r, dims, maxdims)
       call h5dget_space_f(this% s, s, this% error)
       start(1) = dims(1)-1
       select_count = 1
       call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
       call h5dwrite_f(this% s, H5T_NATIVE_INTEGER, step, select_count, this% error, mem_s, s)
       call h5sclose_f(s, this% error)

       if (present(time) .and. this% has_time) then
          call h5md_extend(this% t, r, dims, maxdims)
          call h5dget_space_f(this% t, s, this% error)
          start(1) = dims(1)-1
          select_count = 1
          call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
          call h5dwrite_f(this% t, H5T_NATIVE_DOUBLE, time, select_count, this% error, mem_s, s)
          call h5sclose_f(s, this% error)
       end if
       call h5sclose_f(mem_s, this% error)
    end if

  end subroutine h5md_element_append_d1

  subroutine h5md_element_append_ds(this, data, step, time)
    class(h5md_element_t), intent(inout) :: this
    double precision, intent(in) :: data
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time

    integer, parameter :: rank=1
    integer :: r
    integer(HID_T) :: s, mem_s
    integer(HSIZE_T) :: dims(rank), maxdims(rank), start(rank), select_count(rank)

    if (this% type == H5MD_FIXED) return

    dims(1) = 1
    call h5screate_simple_f(1, dims, mem_s, this% error)

    call h5md_extend(this% v, r, dims, maxdims)
    call check_true((r == rank), 'invalid rank for v in append')
    call h5dget_space_f(this% v, s, this% error)
    start = 0
    start(rank) = dims(rank)-1
    select_count = dims
    select_count(rank) = 1
    call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
    call h5dwrite_f(this% v, H5T_NATIVE_DOUBLE, data, select_count, this% error, mem_s, s)
    call h5sclose_f(s, this% error)
    call h5sclose_f(mem_s, this% error)

    if (this% type == H5MD_TIME) then
       dims(1) = 1
       call h5screate_simple_f(1, dims, mem_s, this% error)
       call h5md_extend(this% s, r, dims, maxdims)
       call h5dget_space_f(this% s, s, this% error)
       start(1) = dims(1)-1
       select_count = 1
       call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
       call h5dwrite_f(this% s, H5T_NATIVE_INTEGER, step, select_count, this% error, mem_s, s)
       call h5sclose_f(s, this% error)

       if (present(time) .and. this% has_time) then
          call h5md_extend(this% t, r, dims, maxdims)
          call h5dget_space_f(this% t, s, this% error)
          start(1) = dims(1)-1
          select_count = 1
          call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
          call h5dwrite_f(this% t, H5T_NATIVE_DOUBLE, time, select_count, this% error, mem_s, s)
          call h5sclose_f(s, this% error)
       end if
       call h5sclose_f(mem_s, this% error)
    end if

  end subroutine h5md_element_append_ds

  subroutine h5md_element_append_d3(this, data, step, time)
    class(h5md_element_t), intent(inout) :: this
    double precision, intent(in) :: data(:,:,:)
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time

    integer, parameter :: rank=4
    integer :: r
    integer(HID_T) :: s, mem_s
    integer(HSIZE_T) :: dims(rank), maxdims(rank), start(rank), select_count(rank)

    if (this% type == H5MD_FIXED) return

    dims(1:rank-1) = shape(data)
    call h5screate_simple_f(rank-1, dims, mem_s, this% error)

    call h5md_extend(this% v, r, dims, maxdims)
    call check_true((r == rank), 'invalid rank for v in append')
    call h5dget_space_f(this% v, s, this% error)
    start = 0
    start(rank) = dims(rank)-1
    select_count = dims
    select_count(rank) = 1
    call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
    call h5dwrite_f(this% v, H5T_NATIVE_DOUBLE, data, select_count, this% error, mem_s, s)
    call h5sclose_f(s, this% error)
    call h5sclose_f(mem_s, this% error)

    if (this% type == H5MD_TIME) then
       dims(1) = 1
       call h5screate_simple_f(1, dims, mem_s, this% error)
       call h5md_extend(this% s, r, dims, maxdims)
       call h5dget_space_f(this% s, s, this% error)
       start(1) = dims(1)-1
       select_count = 1
       call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
       call h5dwrite_f(this% s, H5T_NATIVE_INTEGER, step, select_count, this% error, mem_s, s)
       call h5sclose_f(s, this% error)

       if (present(time) .and. this% has_time) then
          call h5md_extend(this% t, r, dims, maxdims)
          call h5dget_space_f(this% t, s, this% error)
          start(1) = dims(1)-1
          select_count = 1
          call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
          call h5dwrite_f(this% t, H5T_NATIVE_DOUBLE, time, select_count, this% error, mem_s, s)
          call h5sclose_f(s, this% error)
       end if
       call h5sclose_f(mem_s, this% error)
    end if

  end subroutine h5md_element_append_d3

  subroutine h5md_element_append_i1(this, data, step, time)
    class(h5md_element_t), intent(inout) :: this
    integer, intent(in) :: data(:)
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time

    integer, parameter :: rank=2
    integer :: r
    integer(HID_T) :: s, mem_s
    integer(HSIZE_T) :: dims(rank), maxdims(rank), start(rank), select_count(rank)

    if (this% type == H5MD_FIXED) return

    dims(1:rank-1) = shape(data)
    call h5screate_simple_f(rank-1, dims, mem_s, this% error)

    call h5md_extend(this% v, r, dims, maxdims)
    call check_true((r == rank), 'invalid rank for v in append')
    call h5dget_space_f(this% v, s, this% error)
    start = 0
    start(rank) = dims(rank)-1
    select_count = dims
    select_count(rank) = 1
    call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
    call h5dwrite_f(this% v, H5T_NATIVE_INTEGER, data, select_count, this% error, mem_s, s)
    call h5sclose_f(s, this% error)
    call h5sclose_f(mem_s, this% error)

    if (this% type == H5MD_TIME) then
       dims(1) = 1
       call h5screate_simple_f(1, dims, mem_s, this% error)
       call h5md_extend(this% s, r, dims, maxdims)
       call h5dget_space_f(this% s, s, this% error)
       start(1) = dims(1)-1
       select_count = 1
       call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
       call h5dwrite_f(this% s, H5T_NATIVE_INTEGER, step, select_count, this% error, mem_s, s)
       call h5sclose_f(s, this% error)

       if (present(time) .and. this% has_time) then
          call h5md_extend(this% t, r, dims, maxdims)
          call h5dget_space_f(this% t, s, this% error)
          start(1) = dims(1)-1
          select_count = 1
          call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
          call h5dwrite_f(this% t, H5T_NATIVE_DOUBLE, time, select_count, this% error, mem_s, s)
          call h5sclose_f(s, this% error)
       end if
       call h5sclose_f(mem_s, this% error)
    end if

  end subroutine h5md_element_append_i1

  subroutine h5md_element_append_i2(this, data, step, time)
    class(h5md_element_t), intent(inout) :: this
    integer, intent(in) :: data(:,:)
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time

    integer, parameter :: rank=3
    integer :: r
    integer(HID_T) :: s, mem_s
    integer(HSIZE_T) :: dims(rank), maxdims(rank), start(rank), select_count(rank)

    if (this% type == H5MD_FIXED) return

    dims(1:rank-1) = shape(data)
    call h5screate_simple_f(rank-1, dims, mem_s, this% error)

    call h5md_extend(this% v, r, dims, maxdims)
    call check_true((r == rank), 'invalid rank for v in append')
    call h5dget_space_f(this% v, s, this% error)
    start = 0
    start(3) = dims(3)-1
    select_count = dims
    select_count(3) = 1
    call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
    call h5dwrite_f(this% v, H5T_NATIVE_INTEGER, data, select_count, this% error, mem_s, s)
    call h5sclose_f(s, this% error)
    call h5sclose_f(mem_s, this% error)

    if (this% type == H5MD_TIME) then
       dims(1) = 1
       call h5screate_simple_f(1, dims, mem_s, this% error)
       call h5md_extend(this% s, r, dims, maxdims)
       call h5dget_space_f(this% s, s, this% error)
       start(1) = dims(1)-1
       select_count = 1
       call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
       call h5dwrite_f(this% s, H5T_NATIVE_INTEGER, step, select_count, this% error, mem_s, s)
       call h5sclose_f(s, this% error)

       if (present(time) .and. this% has_time) then
          call h5md_extend(this% t, r, dims, maxdims)
          call h5dget_space_f(this% t, s, this% error)
          start(1) = dims(1)-1
          select_count = 1
          call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
          call h5dwrite_f(this% t, H5T_NATIVE_DOUBLE, time, select_count, this% error, mem_s, s)
          call h5sclose_f(s, this% error)
       end if
       call h5sclose_f(mem_s, this% error)
    end if

  end subroutine h5md_element_append_i2

  subroutine h5md_element_append_i3(this, data, step, time)
    class(h5md_element_t), intent(inout) :: this
    integer, intent(in) :: data(:,:,:)
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time

    integer, parameter :: rank=4
    integer :: r
    integer(HID_T) :: s, mem_s
    integer(HSIZE_T) :: dims(rank), maxdims(rank), start(rank), select_count(rank)

    if (this% type == H5MD_FIXED) return

    dims(1:rank-1) = shape(data)
    call h5screate_simple_f(rank-1, dims, mem_s, this% error)

    call h5md_extend(this% v, r, dims, maxdims)
    call check_true((r == rank), 'invalid rank for v in append')
    call h5dget_space_f(this% v, s, this% error)
    start = 0
    start(rank) = dims(rank)-1
    select_count = dims
    select_count(rank) = 1
    call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
    call h5dwrite_f(this% v, H5T_NATIVE_INTEGER, data, select_count, this% error, mem_s, s)
    call h5sclose_f(s, this% error)
    call h5sclose_f(mem_s, this% error)

    if (this% type == H5MD_TIME) then
       dims(1) = 1
       call h5screate_simple_f(1, dims, mem_s, this% error)
       call h5md_extend(this% s, r, dims, maxdims)
       call h5dget_space_f(this% s, s, this% error)
       start(1) = dims(1)-1
       select_count = 1
       call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
       call h5dwrite_f(this% s, H5T_NATIVE_INTEGER, step, select_count, this% error, mem_s, s)
       call h5sclose_f(s, this% error)

       if (present(time) .and. this% has_time) then
          call h5md_extend(this% t, r, dims, maxdims)
          call h5dget_space_f(this% t, s, this% error)
          start(1) = dims(1)-1
          select_count = 1
          call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
          call h5dwrite_f(this% t, H5T_NATIVE_DOUBLE, time, select_count, this% error, mem_s, s)
          call h5sclose_f(s, this% error)
       end if
       call h5sclose_f(mem_s, this% error)
    end if

  end subroutine h5md_element_append_i3

  subroutine h5md_element_open_time(this, loc, name)
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name

    integer(HID_T) :: space
    integer(HSIZE_T), allocatable :: dims(:), maxdims(:)
    logical :: link_exists
    integer :: classtype, rank

    call h5lexists_f(loc, name, link_exists, this% error)
    call check_true(link_exists, 'element '//name//' not found')

    call h5gopen_f(loc, name, this% id, this% error)
    call h5dopen_f(this% id, 'value', this% v, this% error)
    call h5dopen_f(this% id, 'step', this% s, this% error)
    call h5dget_space_f(this% s, space, this% error)
    call h5sget_simple_extent_type_f(space, classtype, this% error)
    if (classtype == H5S_SCALAR_F) then
       this% type = H5MD_LINEAR
    else
       this% type = H5MD_TIME
    end if
    call h5sclose_f(space, this% error)

    call h5lexists_f(this% id, 'time', link_exists, this% error)
    if (link_exists) then
       call h5dopen_f(this% id, 'time', this% t, this% error)
       call h5dget_space_f(this% s, space, this% error)
       call h5sget_simple_extent_type_f(space, classtype, this% error)
       if (this% type == H5MD_LINEAR) then
          if (classtype /= H5S_SCALAR_F) then
             stop 'inconsistent step and time datasets'
          end if
       end if
       call h5sclose_f(space, this% error)
    end if

    call h5dget_space_f(this% v, space, this% error)
    call h5sget_simple_extent_type_f(space, classtype, this% error)
    if (classtype /= H5S_SIMPLE_F) then
       stop 'non simple dataspace'
    end if

    call h5sget_simple_extent_ndims_f(space, rank, this% error)
    allocate(dims(rank))
    allocate(maxdims(rank))
    call h5sget_simple_extent_dims_f(space, dims, maxdims, this% error)

    if (rank >= 2) then
       this%Nmax = int(dims(2))
    else
       this%Nmax = 0
    end if

    call h5sclose_f(space, this% error)

    deallocate(dims)
    deallocate(maxdims)

  end subroutine h5md_element_open_time

  subroutine h5md_element_create_fixed_d1(this, loc, name, data)
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: data(:)

    integer(HSIZE_T) :: dims(1)
    integer :: s

    dims = shape(data)

    call h5screate_simple_f(1, dims, s, this% error)
    call h5dcreate_f(loc, name, H5T_NATIVE_DOUBLE, s, this% id, this% error)
    call h5dwrite_f(this% id, H5T_NATIVE_DOUBLE, data, dims, this% error, H5S_ALL_F, s)

    call h5dclose_f(this% id, this% error)
    call h5sclose_f(s, this% error)

  end subroutine h5md_element_create_fixed_d1

  subroutine h5md_element_create_fixed_d2(this, loc, name, data)
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: data(:,:)

    integer(HSIZE_T) :: dims(2)
    integer :: s

    dims = shape(data)

    call h5screate_simple_f(2, dims, s, this% error)
    call h5dcreate_f(loc, name, H5T_NATIVE_DOUBLE, s, this% id, this% error)
    call h5dwrite_f(this% id, H5T_NATIVE_DOUBLE, data, dims, this% error, H5S_ALL_F, s)

    call h5dclose_f(this% id, this% error)
    call h5sclose_f(s, this% error)

  end subroutine h5md_element_create_fixed_d2

  subroutine h5md_element_create_fixed_d3(this, loc, name, data)
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: data(:,:,:)

    integer(HSIZE_T) :: dims(3)
    integer :: s

    dims = shape(data)

    call h5screate_simple_f(3, dims, s, this% error)
    call h5dcreate_f(loc, name, H5T_NATIVE_DOUBLE, s, this% id, this% error)
    call h5dwrite_f(this% id, H5T_NATIVE_DOUBLE, data, dims, this% error, H5S_ALL_F, s)

    call h5dclose_f(this% id, this% error)
    call h5sclose_f(s, this% error)

  end subroutine h5md_element_create_fixed_d3

  subroutine h5md_element_create_fixed_d4(this, loc, name, data)
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: data(:,:,:,:)

    integer(HSIZE_T) :: dims(4)
    integer :: s

    dims = shape(data)

    call h5screate_simple_f(4, dims, s, this% error)
    call h5dcreate_f(loc, name, H5T_NATIVE_DOUBLE, s, this% id, this% error)
    call h5dwrite_f(this% id, H5T_NATIVE_DOUBLE, data, dims, this% error, H5S_ALL_F, s)

    call h5dclose_f(this% id, this% error)
    call h5sclose_f(s, this% error)

  end subroutine h5md_element_create_fixed_d4

  subroutine h5md_element_create_fixed_i1(this, loc, name, data)
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer, intent(in) :: data(:)

    integer(HSIZE_T) :: dims(1)
    integer :: s

    dims = shape(data)

    call h5screate_simple_f(1, dims, s, this% error)
    call h5dcreate_f(loc, name, H5T_NATIVE_INTEGER, s, this% id, this% error)
    call h5dwrite_f(this% id, H5T_NATIVE_INTEGER, data, dims, this% error, H5S_ALL_F, s)

    call h5dclose_f(this% id, this% error)
    call h5sclose_f(s, this% error)

  end subroutine h5md_element_create_fixed_i1

  subroutine h5md_element_create_fixed_i2(this, loc, name, data)
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer, intent(in) :: data(:,:)

    integer, parameter :: rank=2
    integer(HSIZE_T) :: dims(rank)
    integer :: s

    dims = shape(data)

    call h5screate_simple_f(rank, dims, s, this% error)
    call h5dcreate_f(loc, name, H5T_NATIVE_INTEGER, s, this% id, this% error)
    call h5dwrite_f(this% id, H5T_NATIVE_INTEGER, data, dims, this% error, H5S_ALL_F, s)

    call h5dclose_f(this% id, this% error)
    call h5sclose_f(s, this% error)

  end subroutine h5md_element_create_fixed_i2

  subroutine h5md_element_read_fixed_d2(this, loc, name, data)
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(out), allocatable :: data(:,:)

    integer(HSIZE_T) :: dims(2), maxdims(2)
    integer(HID_T) :: s
    integer :: rank
    logical :: flag

    call h5dopen_f(loc, name, this% id, this% error)
    call h5dget_space_f(this% id, s, this% error)
    call h5sis_simple_f(s, flag, this% error)
    call check_true(flag, 'non-simple dataset in read_fixed')
    call h5sget_simple_extent_ndims_f(s, rank, this% error)
    call check_true((rank == 2), 'rank /= 2 in read_fixed')
    call h5sget_simple_extent_dims_f(s, dims, maxdims, this% error)

    allocate(data(dims(1), dims(2)))
    call h5dread_f(this% id, H5T_NATIVE_DOUBLE, data, dims, this% error, &
         mem_space_id=H5S_ALL_F, file_space_id=s)
    call h5dclose_f(this% id, this% error)
    call h5sclose_f(s, this% error)

  end subroutine h5md_element_read_fixed_d2

  subroutine h5md_element_read_fixed_i1(this, loc, name, data)
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer, intent(out), allocatable :: data(:)

    integer, parameter :: set_rank = 1
    integer(HSIZE_T) :: dims(set_rank), maxdims(set_rank)
    integer(HID_T) :: s
    integer :: rank
    logical :: flag

    call h5dopen_f(loc, name, this% id, this% error)
    call h5dget_space_f(this% id, s, this% error)
    call h5sis_simple_f(s, flag, this% error)
    call check_true(flag, 'non-simple dataset in read_fixed')
    call h5sget_simple_extent_ndims_f(s, rank, this% error)
    call check_true((rank == set_rank), 'rank /= set_rank in read_fixed')
    call h5sget_simple_extent_dims_f(s, dims, maxdims, this% error)

    allocate(data(dims(1)))
    call h5dread_f(this% id, H5T_NATIVE_INTEGER, data, dims, this% error, H5S_ALL_F, s)
    call h5dclose_f(this% id, this% error)
    call h5sclose_f(s, this% error)

  end subroutine h5md_element_read_fixed_i1

  subroutine check_error(e, msg)
    integer, intent(in) :: e
    character(len=*), intent(in) :: msg

    if (e /= 0) then
       write(*,*) 'error code ', e
       write(*,*) msg
       stop
    end if

  end subroutine check_error

  subroutine check_true(flag, msg)
    logical, intent(in) :: flag
    character(len=*), intent(in) :: msg

    if (.not. flag) then
       write(*,*) msg
       stop
    end if

  end subroutine check_true

  subroutine h5md_check_valid(id, msg, strict)
    integer(HID_T), intent(in) :: id
    character(len=*), intent(in) :: msg
    logical, intent(in), optional :: strict

    logical :: valid
    integer :: error

    call h5iis_valid_f(id, valid, error)
    if (.not. valid) then
       write(*, *) msg
       if (present(strict)) then
          if (strict) stop
       else
          stop
       end if
    end if

  end subroutine h5md_check_valid

  subroutine h5md_check_exists(loc, name, msg)
    integer(HID_T), intent(in) :: loc
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: msg

    logical :: exists
    integer :: error


    call h5lexists_f(loc, name, exists, error)

    if (.not. exists) then
       write(*, *) msg
       stop
    end if

  end subroutine h5md_check_exists

  subroutine h5md_write_attribute_cs(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value

    integer(HID_T) :: a, s, t
    integer :: error
    integer(HSIZE_T) :: dims(1)
    integer(SIZE_T) :: length

    call h5screate_f(H5S_SCALAR_F, s, error)
    dims(1) = len(value)
    length = len(value)
    call h5tcopy_f(H5T_NATIVE_CHARACTER, t, error)
    call h5tset_size_f(t, length, error)
    call h5tset_strpad_f(t, H5T_STR_NULLTERM_F, error)
    call h5acreate_f(loc, name, t, s, a, error)
    call h5awrite_f(a, t, value, dims, error)
    call h5aclose_f(a, error)
    call h5tclose_f(t, error)
    call h5sclose_f(s, error)

  end subroutine h5md_write_attribute_cs

  subroutine h5md_write_attribute_c1(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value(:)

    integer(HID_T) :: a, s, t
    integer :: error
    integer(HSIZE_T) :: dims(1)
    integer(SIZE_T) :: length

    dims(1) = size(value)
    length = len(value(1))
    call h5screate_simple_f(1, dims, s, error)
    call h5tcopy_f(H5T_NATIVE_CHARACTER, t, error)
    call h5tset_size_f(t, length, error)
    call h5tset_strpad_f(t, H5T_STR_NULLTERM_F, error)
    call h5acreate_f(loc, name, t, s, a, error)
    call h5awrite_f(a, t, value, dims, error)
    call h5aclose_f(a, error)
    call h5tclose_f(t, error)
    call h5sclose_f(s, error)

  end subroutine h5md_write_attribute_c1

  subroutine h5md_write_attribute_is(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer, intent(in) :: value

    integer(HID_T) :: a, s
    integer :: error
    integer(HSIZE_T) :: dims(1)

    call h5screate_f(H5S_SCALAR_F, s, error)
    call h5acreate_f(loc, name, H5T_NATIVE_INTEGER, s, a, error)
    call h5awrite_f(a, H5T_NATIVE_INTEGER, value, dims, error)
    call h5aclose_f(a, error)
    call h5sclose_f(s, error)

  end subroutine h5md_write_attribute_is

  subroutine h5md_write_attribute_i1(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer, intent(in) :: value(:)

    integer(HID_T) :: a, s, t
    integer :: error
    integer(HSIZE_T) :: dims(1)

    dims(1) = size(value, dim=1)
    call h5screate_simple_f(1, dims, s, error)
    t = H5T_NATIVE_INTEGER
    call h5acreate_f(loc, name, t, s, a, error)
    call h5awrite_f(a, t, value, dims, error)
    call h5aclose_f(a, error)
    call h5sclose_f(s, error)

  end subroutine h5md_write_attribute_i1

  subroutine h5md_write_attribute_ds(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: value

    integer(HID_T) :: a, s
    integer :: error
    integer(HSIZE_T) :: dims(1)

    call h5screate_f(H5S_SCALAR_F, s, error)
    call h5acreate_f(loc, name, H5T_NATIVE_DOUBLE, s, a, error)
    call h5awrite_f(a, H5T_NATIVE_DOUBLE, value, dims, error)
    call h5aclose_f(a, error)
    call h5sclose_f(s, error)

  end subroutine h5md_write_attribute_ds

  subroutine h5md_read_attribute_ds(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(out) :: value

    integer(HID_T) :: a, s
    integer :: error, rank
    integer(HSIZE_T) :: dims(1)
    logical :: flag

    call h5aopen_f(loc, name, a, error)
    call h5aget_space_f(a, s, error)
    call h5sis_simple_f(s, flag, error)
    if (.not. flag) then
       write(*,*) 'attribute does not have a simple dataspace in h5md_read_attribute'
       stop
    end if
    call h5sget_simple_extent_ndims_f(s, rank, error)
    if (rank /= 0) error stop 'non-zero rank in h5md_read_attribute'
    call h5aread_f(a, H5T_NATIVE_DOUBLE, value, dims, error)
    call h5sclose_f(s, error)
    call h5aclose_f(a, error)

  end subroutine h5md_read_attribute_ds

  subroutine h5md_extend(dset, rank, dims, maxdims, ext_size)
    integer(HID_T), intent(inout) :: dset
    integer, intent(out) :: rank
    integer(HSIZE_T), intent(out) :: dims(:), maxdims(:)
    integer(HSIZE_T), intent(in), optional :: ext_size

    integer(HID_T) :: s
    integer :: error
    integer(HSIZE_T) :: ext

    if (present(ext_size)) then
       ext = ext_size
    else
       ext = 1
    end if

    call h5dget_space_f(dset, s, error)
    call check_error(error, 'get_space error in h5md_extend')
    call h5sget_simple_extent_ndims_f(s, rank, error)
    call check_error(error, 'get_simple_extent_ndims error')
    call h5sget_simple_extent_dims_f(s, dims, maxdims, error)
    dims(rank) = dims(rank) + ext
    call h5sclose_f(s, error)
    call check_error(error, 'h5sclose error')
    call h5dset_extent_f(dset, dims, error)
    call check_error(error, 'set_extent error')

  end subroutine h5md_extend

  subroutine h5md_element_append_buffer_s(this, data, step, time, force_size)
    class(h5md_element_t), intent(inout) :: this
    double precision, intent(in) :: data(:)
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time
    integer, intent(in), optional :: force_size

    integer, parameter :: rank=1
    integer :: r
    integer(HID_T) :: s, mem_s
    integer(HSIZE_T) :: dims(rank), maxdims(rank), start(rank), select_count(rank)
    integer(HSIZE_T) :: buffer_size

    if (this% type == H5MD_FIXED) return

    dims = shape(data)
    if (present(force_size)) then
       if (force_size>0) then
          buffer_size = force_size
          dims(rank) = buffer_size
       else
          buffer_size = dims(rank)
       end if
    else
       buffer_size = dims(rank)
    end if
    call h5screate_simple_f(rank, dims, mem_s, this% error)

    call h5md_extend(this% v, r, dims, maxdims, buffer_size)
    call check_true((r == rank), 'invalid rank for v in append')
    call h5dget_space_f(this% v, s, this% error)
    start = 0
    start(rank) = dims(rank)-buffer_size
    select_count = dims
    select_count(rank) = buffer_size
    call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
    call h5dwrite_f(this% v, H5T_NATIVE_DOUBLE, data, select_count, this% error, mem_s, s)
    call h5sclose_f(s, this% error)
    call h5sclose_f(mem_s, this% error)

  end subroutine h5md_element_append_buffer_s

  subroutine h5md_element_append_buffer_d1(this, data, step, time, force_size)
    class(h5md_element_t), intent(inout) :: this
    double precision, intent(in) :: data(:,:)
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time
    integer, intent(in), optional :: force_size

    integer, parameter :: rank=2
    integer :: r
    integer(HID_T) :: s, mem_s
    integer(HSIZE_T) :: dims(rank), maxdims(rank), start(rank), select_count(rank)
    integer(HSIZE_T) :: buffer_size

    if (this% type == H5MD_FIXED) return

    dims = shape(data)
    if (present(force_size)) then
       if (force_size>0) then
          buffer_size = force_size
          dims(rank) = buffer_size
       else
          buffer_size = dims(rank)
       end if
    else
       buffer_size = dims(rank)
    end if
    call h5screate_simple_f(rank, dims, mem_s, this% error)

    call h5md_extend(this% v, r, dims, maxdims, buffer_size)
    call check_true((r == rank), 'invalid rank for v in append')
    call h5dget_space_f(this% v, s, this% error)
    start = 0
    start(rank) = dims(rank)-buffer_size
    select_count = dims
    select_count(rank) = buffer_size
    call h5sselect_hyperslab_f(s, H5S_SELECT_SET_F, start, select_count, this% error)
    call h5dwrite_f(this% v, H5T_NATIVE_DOUBLE, data, select_count, this% error, mem_s, s)
    call h5sclose_f(s, this% error)
    call h5sclose_f(mem_s, this% error)

  end subroutine h5md_element_append_buffer_d1

  subroutine h5md_write_dataset_ds(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: value

    integer(HID_T) :: d, s, t
    integer :: error
    integer(HSIZE_T) :: dims(1)

    t = H5T_NATIVE_DOUBLE
    dims(1) = 1

    call h5screate_f(H5S_SCALAR_F, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    call h5dwrite_f(d, t, value, dims, error)
    call h5dclose_f(d, error)
    call h5sclose_f(s, error)

  end subroutine h5md_write_dataset_ds

  subroutine h5md_write_dataset_d2(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: value(:,:)

    integer(HID_T) :: d, s, t
    integer :: error
    integer, parameter :: rank=2
    integer(HSIZE_T) :: dims(rank)

    t = H5T_NATIVE_DOUBLE
    dims = shape(value)

    call h5screate_simple_f(rank, dims, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    call h5dwrite_f(d, t, value, dims, error)
    call h5dclose_f(d, error)
    call h5sclose_f(s, error)

  end subroutine h5md_write_dataset_d2

  subroutine h5md_write_dataset_d4(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: value(:,:,:,:)

    integer(HID_T) :: d, s, t
    integer :: error
    integer, parameter :: rank=4
    integer(HSIZE_T) :: dims(rank)

    t = H5T_NATIVE_DOUBLE
    dims = shape(value)

    call h5screate_simple_f(rank, dims, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    call h5dwrite_f(d, t, value, dims, error)
    call h5dclose_f(d, error)
    call h5sclose_f(s, error)

  end subroutine h5md_write_dataset_d4

  subroutine h5md_write_dataset_d3(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: value(:,:,:)

    integer(HID_T) :: d, s, t
    integer :: error
    integer, parameter :: rank=3
    integer(HSIZE_T) :: dims(rank)

    t = H5T_NATIVE_DOUBLE
    dims = shape(value)

    call h5screate_simple_f(rank, dims, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    call h5dwrite_f(d, t, value, dims, error)
    call h5dclose_f(d, error)
    call h5sclose_f(s, error)

  end subroutine h5md_write_dataset_d3

  subroutine h5md_write_dataset_i2(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer, intent(in) :: value(:,:)

    integer(HID_T) :: d, s, t
    integer :: error
    integer, parameter :: rank=2
    integer(HSIZE_T) :: dims(rank)

    t = H5T_NATIVE_INTEGER
    dims = shape(value)

    call h5screate_simple_f(rank, dims, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    call h5dwrite_f(d, t, value, dims, error)
    call h5dclose_f(d, error)
    call h5sclose_f(s, error)

  end subroutine h5md_write_dataset_i2

  subroutine h5md_write_dataset_i1(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer, intent(in) :: value(:)

    integer(HID_T) :: d, s, t
    integer :: error
    integer, parameter :: rank=1
    integer(HSIZE_T) :: dims(rank)

    t = H5T_NATIVE_INTEGER
    dims = shape(value)

    call h5screate_simple_f(rank, dims, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    call h5dwrite_f(d, t, value, dims, error)
    call h5dclose_f(d, error)
    call h5sclose_f(s, error)

  end subroutine h5md_write_dataset_i1

  subroutine h5md_write_dataset_is(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer, intent(in) :: value

    integer(HID_T) :: d, s, t
    integer :: error
    integer(HSIZE_T) :: dims(1)

    t = H5T_NATIVE_INTEGER
    dims(1) = 1

    call h5screate_f(H5S_SCALAR_F, s, error)
    call h5dcreate_f(loc, name, t, s, d, error)
    call h5dwrite_f(d, t, value, dims, error)
    call h5dclose_f(d, error)
    call h5sclose_f(s, error)

  end subroutine h5md_write_dataset_is

end module h5md_module

module h5md_module
  use hdf5
  implicit none

  private

  public :: h5md_file_t, h5md_element_t
  public :: h5md_check_valid, h5md_check_exists
  public :: h5md_write_attribute
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
     generic, public :: create_fixed => h5md_element_create_fixed_d2, h5md_element_create_fixed_d1
     procedure, private :: h5md_element_create_fixed_d2, h5md_element_create_fixed_d1
     generic, public :: read_fixed => h5md_element_read_fixed_d2
     procedure, private :: h5md_element_read_fixed_d2
     procedure :: open_time => h5md_element_open_time
     generic, public :: create_time => h5md_element_create_time_d2, h5md_element_create_time_ds
     procedure, private :: h5md_element_create_time_d2, h5md_element_create_time_ds
     generic, public :: append => h5md_element_append_d2, h5md_element_append_ds
     procedure, private :: h5md_element_append_d2, h5md_element_append_ds
     procedure :: close => h5md_element_close
  end type h5md_element_t

  interface h5md_write_attribute
     module procedure h5md_write_attribute_cs
     module procedure h5md_write_attribute_c1
     module procedure h5md_write_attribute_is
     module procedure h5md_write_attribute_i1
  end interface h5md_write_attribute

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

  subroutine h5md_element_create_time_d2(this, loc, name, data, mode, step, time)
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: data(:,:)
    integer, intent(in) :: mode
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time

    integer, parameter :: rank = 3
    integer(HSIZE_T) :: dims(rank), maxdims(rank), chunk_dims(rank)
    integer(HID_T) :: s, plist

    call h5gcreate_f(loc, name, this% id, this% error)
    call h5md_check_valid(this% id, 'invalid id in create_time')

    dims(1) = size(data, 1)
    maxdims(1) = dims(1)
    chunk_dims(1) = dims(1)
    dims(2) = size(data, 2)
    maxdims(2) = dims(2)
    chunk_dims(2) = min(dims(2), 128)
    dims(3) = 0
    maxdims(3) = H5S_UNLIMITED_F
    chunk_dims(3) = 1

    call h5screate_simple_f(rank, dims, s, this% error, maxdims)

    call h5pcreate_f(H5P_DATASET_CREATE_F, plist, this% error)
    call h5pset_chunk_f(plist, rank, chunk_dims, this% error)
    call h5dcreate_f(this% id, 'value', H5T_NATIVE_DOUBLE, s, this% v, this% error, plist)
    call h5pclose_f(plist, this% error)
    call h5sclose_f(s, this% error)

    if (iand(mode, H5MD_TIME) == H5MD_TIME) then
       this% type = H5MD_TIME
       dims(1) = 0
       maxdims(1) = H5S_UNLIMITED_F
       chunk_dims(1) = 8
       call h5screate_simple_f(1, dims, s, this% error, maxdims)
       call h5pcreate_f(H5P_DATASET_CREATE_F, plist, this% error)
       call h5pset_chunk_f(plist, 1, chunk_dims, this% error)
       call h5dcreate_f(this% id, 'step', H5T_NATIVE_INTEGER, s, this% s, this% error, plist)
       if (iand(mode, H5MD_STORE_TIME) == H5MD_STORE_TIME) then
          call h5dcreate_f(this% id, 'time', H5T_NATIVE_DOUBLE, s, this% t, this% error, plist)
          this% has_time = .true.
       else
          this% has_time = .false.
       end if
       call h5pclose_f(plist, this% error)
       call h5sclose_f(s, this% error)
    else if (mode == H5MD_LINEAR) then
       this% type = H5MD_LINEAR
       if (.not. present(step)) stop 'step required for H5MD_LINEAR'
       call h5screate_f(H5S_SCALAR_F, s, this% error)
       call h5dcreate_f(this% id, 'step', H5T_NATIVE_INTEGER, s, this% s, this% error)
       call h5dwrite_f(this% s, H5T_NATIVE_INTEGER, step, dims, this% error, H5S_ALL_F, s)
       call h5dclose_F(this% s, this% error)
       this% has_time = present(time)
       if (this% has_time) then
          call h5dcreate_f(this% id, 'time', H5T_NATIVE_INTEGER, s, this% t, this% error)
          call h5dwrite_f(this% t, H5T_NATIVE_DOUBLE, time, dims, this% error, H5S_ALL_F, s)
          call h5dclose_F(this% t, this% error)
       end if
       call h5sclose_f(s, this% error)
    end if
    call h5gclose_f(this% id, this% error)

  end subroutine h5md_element_create_time_d2

  subroutine h5md_element_create_time_ds(this, loc, name, data, mode, step, time)
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    double precision, intent(in) :: data
    integer, intent(in) :: mode
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time

    integer, parameter :: rank = 1
    integer(HSIZE_T) :: dims(rank), maxdims(rank), chunk_dims(rank)
    integer(HID_T) :: s, plist

    call h5gcreate_f(loc, name, this% id, this% error)
    call h5md_check_valid(this% id, 'invalid id in create_time')

    dims(rank) = 0
    maxdims(rank) = H5S_UNLIMITED_F
    chunk_dims(rank) = 8

    call h5screate_simple_f(rank, dims, s, this% error, maxdims)

    call h5pcreate_f(H5P_DATASET_CREATE_F, plist, this% error)
    call h5pset_chunk_f(plist, rank, chunk_dims, this% error)
    call h5dcreate_f(this% id, 'value', H5T_NATIVE_DOUBLE, s, this% v, this% error, plist)
    call h5pclose_f(plist, this% error)
    call h5sclose_f(s, this% error)

    if (iand(mode, H5MD_TIME) == H5MD_TIME) then
       this% type = H5MD_TIME
       dims(1) = 0
       maxdims(1) = H5S_UNLIMITED_F
       chunk_dims(1) = 8
       call h5screate_simple_f(1, dims, s, this% error, maxdims)
       call h5pcreate_f(H5P_DATASET_CREATE_F, plist, this% error)
       call h5pset_chunk_f(plist, 1, chunk_dims, this% error)
       call h5dcreate_f(this% id, 'step', H5T_NATIVE_INTEGER, s, this% s, this% error, plist)
       if (iand(mode, H5MD_STORE_TIME) == H5MD_STORE_TIME) then
          call h5dcreate_f(this% id, 'time', H5T_NATIVE_DOUBLE, s, this% t, this% error, plist)
          this% has_time = .true.
       else
          this% has_time = .false.
       end if
       call h5pclose_f(plist, this% error)
       call h5sclose_f(s, this% error)
    else if (mode == H5MD_LINEAR) then
       this% type = H5MD_LINEAR
       if (.not. present(step)) stop 'step required for H5MD_LINEAR'
       call h5screate_f(H5S_SCALAR_F, s, this% error)
       call h5dcreate_f(this% id, 'step', H5T_NATIVE_INTEGER, s, this% s, this% error)
       call h5dwrite_f(this% s, H5T_NATIVE_INTEGER, step, dims, this% error, H5S_ALL_F, s)
       call h5dclose_F(this% s, this% error)
       this% has_time = present(time)
       if (this% has_time) then
          call h5dcreate_f(this% id, 'time', H5T_NATIVE_INTEGER, s, this% t, this% error)
          call h5dwrite_f(this% t, H5T_NATIVE_DOUBLE, time, dims, this% error, H5S_ALL_F, s)
          call h5dclose_F(this% t, this% error)
       end if
       call h5sclose_f(s, this% error)
    end if
    call h5gclose_f(this% id, this% error)

  end subroutine h5md_element_create_time_ds

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

    this% Nmax = dims(2)

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
    call h5dread_f(this% id, H5T_NATIVE_DOUBLE, data, dims, this% error, H5S_ALL_F, s)
    call h5dclose_f(this% id, this% error)
    call h5sclose_f(s, this% error)

  end subroutine h5md_element_read_fixed_d2

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

    call h5screate_f(H5S_SCALAR_F, s, error)
    dims(1) = len(value)
    call h5tcopy_f(H5T_NATIVE_CHARACTER, t, error)
    call h5tset_size_f(t, dims(1), error)
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
    integer(HSIZE_T) :: dims(1), length

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

  subroutine h5md_extend(dset, rank, dims, maxdims)
    integer(HID_T), intent(inout) :: dset
    integer, intent(out) :: rank
    integer(HSIZE_T), intent(out) :: dims(:), maxdims(:)

    integer(HID_T) :: s
    integer :: error

    call h5dget_space_f(dset, s, error)
    call check_error(error, 'get_space error in h5md_extend')
    call h5sget_simple_extent_ndims_f(s, rank, error)
    call check_error(error, 'get_simple_extent_ndims error')
    call h5sget_simple_extent_dims_f(s, dims, maxdims, error)
    dims(rank) = dims(rank) + 1
    call h5sclose_f(s, error)
    call check_error(error, 'h5sclose error')
    call h5dset_extent_f(dset, dims, error)
    call check_error(error, 'set_extent error')

  end subroutine h5md_extend

end module h5md_module

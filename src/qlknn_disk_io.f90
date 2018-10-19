! file: disk_io.f
module qlknn_disk_io
    implicit none
contains
    subroutine load_net(filename)
        character(len=*), intent(in) :: filename
        open(10,file=filename)
    end subroutine load_net
end module qlknn_disk_io

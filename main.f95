module procedures
    implicit none
    type live
        logical :: aliveq
        integer :: surroundN
     end type
    contains
    subroutine show_field(field)
        type(live),dimension(:,:) :: field
        integer :: x,y,i,j
        x=size(field,1)
        y=size(field,2)

        do j=1,y
            do i=1,x
                if (field(i,j)%aliveq .eqv. .true.) then
                    write(*,"(1x,A,1x$)") '@'
                else
                    write(*,"(1x,A,1x$)") char(250)
                end if
            end do
            write(*,*)
        end do

!        do j=1,y
!            do i=1,x
!                write(*,"(1x,I1,1x$)") field(i,j)%surroundN
!            end do
!            write(*,*)
!        end do
    end subroutine

    subroutine update_count(field)
        integer :: i,j
        integer :: k,l
        integer :: x,y
        integer :: next_x,next_y
        type(live), dimension(:,:),intent(inout) :: field
        x=size(field,1)
        y=size(field,2)
        field(:,:)%surroundN=0
        do j=1,y
            do i=1,x

                do k=j-1,j+1
                    do l=i-1,i+1
                        if (mod(k,y)<1) then
                                next_y=y+mod(k,y)
                            else
                                next_y=mod(k,y)
                        end if

                        if (mod(l,x)<1) then
                            next_x=x+mod(l,x)
                        else
                            next_x=mod(l,x)
                        end if

                        if ((k/=j .or. l/=i) .and. ( field(next_x,next_y)%aliveq .eqv. .true. )) then
                            field(i,j)%surroundN=field(i,j)%surroundN+1

                        end if

                    end do
                end do

            end do
        end do
    end subroutine

    subroutine update_alive(field)
        integer :: i,j
        integer :: x,y
        type(live), dimension(:,:),intent(inout) :: field
        x=size(field,1)
        y=size(field,2)
        do j=1,y
            do i=1,x
                if ((field(i,j)%aliveq .eqv. .false.) .and. (field(i,j)%surroundN == 3)) then
                    field(i,j)%aliveq = .true.
                end if
                if ((field(i,j)%aliveq .eqv. .true.) .and. (field(i,j)%surroundN<2 .or. field(i,j)%surroundN>3)) then
                    field(i,j)%aliveq = .false.
                else if ((field(i,j)%aliveq .eqv. .true.) .and. (field(i,j)%surroundN==2 .or. field(i,j)%surroundN==3)) then
                    field(i,j)%aliveq = .true.
                end if
            end do
        end do
    end subroutine
end module
program main
  use procedures
  implicit none
  type(live), dimension(30,30) :: field
  integer :: k,i,j
  real num
  logical :: flag
  integer :: x,y

  x=size(field,1)
  y=size(field,2)

  field(:,:) %aliveq=.false.
  field(10:12,10)%aliveq=.true.
  field(12,9)%aliveq=.true.
  field(11,8)%aliveq=.true.
!  do j=1,y
!    do i=1,x
!        call random_number(num)
!        if (num<=0.2) then
!            field(i,j)%aliveq=.true.
!        end if
!    end do
!  end do

  do k=1,1000
    call update_count(field)
    call show_field(field)
    call update_alive(field)
    write(*,*) '---------------------------------------------------------'
    !call sleep(1)

    !all dead check
!---------------------------------------------
!    flag=.false.
!    do j=1,y
!        do i=1,x
!            if ((field(i,j)%aliveq .eqv. .true.) .or. flag) then
!                flag=.true.
!                exit
!            end if
!        end do
!        if (flag .eqv. .true.) then
!        exit
!        end if
!    end do
!    if (flag .eqv. .false.) then
!        write(*,*) "Everyone is dead"
!        exit
!    end if
!---------------------------------------------
    !DISABLE SCREEN CLEANER IF NEEDED >>>>>
    call system('CLS')
    !<<<<<<
  end do

end

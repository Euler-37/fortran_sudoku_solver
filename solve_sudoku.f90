module sudoku_solver
    implicit none
    integer::sudoku(9,9)
contains
    subroutine read_sudoku(filename)
        character(len=*),intent(in)::filename
        integer::i
        open(10,file=filename,action='read')
        do i=1,9
            read(10,*)sudoku(i,:)
        end do
        close(10)
        call write_sudoku()
    end subroutine read_sudoku

    subroutine write_sudoku()
        integer::i
        write(*,"(A)")"+------+------+------+"
        do i=1,9
            write(*,"('|',3(3I2,'|'))")sudoku(i,:)
            if(mod(i,3)==0)write(*,"(A)")"+------+------+------+"
        end do
    end subroutine write_sudoku

    subroutine search_next(row,col)
        integer,intent(in)::col,row
        if(col<9)then
            call solve_sudoku(row,col+1)
        else
            call solve_sudoku(row+1,1)
        end if
    end subroutine search_next


    subroutine solve_sudoku(row,col)
        integer,intent(in)::col,row
        integer::num
        logical::checkbox
        logical::checkrow
        logical::checkcol
        integer::r,c
        if(row>9)then
            call write_sudoku()
            return
        end if
        if(sudoku(row,col)/=0)then
            call search_next(row,col)
        else
            ! 1 2 3|4 5 6|7 8 9
            !   1     4     7
            r=((row-1)/3)*3+1
            c=((col-1)/3)*3+1
            do num=1,9
                checkrow=all(sudoku(row,:)/=num)
                checkcol=all(sudoku(:,col)/=num)
                checkbox=all(sudoku(r:r+2,c:c+2)/=num)
                if(checkrow .and. checkcol.and. checkbox)then
                    sudoku(row,col)=num
                    call search_next(row,col)
                end if
            end do
            sudoku(row,col)=0
        end if
    end subroutine solve_sudoku

end module sudoku_solver

program main
    use sudoku_solver
    implicit none
    call read_sudoku('1.txt')
    call solve_sudoku(1,1)
end program main

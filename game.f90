program main
    implicit none
    integer :: poker(54)
    real(8) :: expection(4,13)
    real(8) :: potention1(4,13)
    real(8) :: potention2(4,13)
    integer :: pokerShow
    integer :: nleft
    integer :: i, j
    integer :: nn1, nn2, jj
    real(8) :: optimus
    nleft = 54
    do i = 1, 4
        do j = 1, 13
            poker(13*(i-1)+j) = 10*j + i
        end do
    end do
    poker(53) = 888
    poker(54) = 999
    do while(nleft .ne. 0)
        do i = 1, 4
            do j = 1, 13
                nn1 = 0
                nn2 = 0
                do jj = 1, 13
                    if ((poker(13*(i-1)+jj) .ne. 0) .and. (jj .ne. j)) then
                        nn1 = nn1 + 1
                    end if
                end do
                if (poker(53) .ne. 0) then
                    nn2 = nn2 + 1
                end if
                if (poker(54) .ne. 0) then
                    nn2 = nn2 + 1
                end if
                if (10*j+i .eq. 130+i) then
                    do jj = 1, 4
                        if (poker(13*(jj-1)+11) .ne. 0) then
                            nn1 = nn1 + 1
                        end if
                        if (poker(13*(jj-1)+12) .ne. 0) then
                            nn1 = nn1 + 1
                        end if
                        if (poker(13*(jj-1)+13) .ne. 0) then
                            nn2 = nn2 + 1
                        end if
                        if (poker(13*(jj-1)+1) .ne. 0) then
                            nn1 = nn1 + 1
                        end if
                    end do
                else if (10*j+i .eq. 20+i) then
                    do jj = 1, 4
                        if (poker(13*(jj-1)+1) .ne. 0) then
                            nn1 = nn1 + 1
                        end if
                        if (poker(13*(jj-1)+2) .ne. 0) then
                            nn2 = nn2 + 1
                        end if
                        if (poker(13*(jj-1)+3) .ne. 0) then
                            nn1 = nn1 + 1
                        end if
                        if (poker(13*(jj-1)+4) .ne. 0) then
                            nn1 = nn1 + 1
                        end if
                    end do
                else if (10*j+i .eq. 10+i) then
                    do jj = 1, 4
                        if (poker(13*(jj-1)+12) .ne. 0) then
                            nn1 = nn1 + 1
                        end if
                        if (poker(13*(jj-1)+13) .ne. 0) then
                            nn1 = nn1 + 1
                        end if
                        if (poker(13*(jj-1)+1) .ne. 0) then
                            nn2 = nn2 + 1
                        end if
                        if (poker(13*(jj-1)+2) .ne. 0) then
                            nn1 = nn1 + 1
                        end if
                        if (poker(13*(jj-1)+3) .ne. 0) then
                            nn1 = nn1 + 1
                        end if
                    end do
                else
                    do jj = 1, 4
                        if (poker(13*(jj-1)+j-2) .ne. 0) then
                            nn1 = nn1 + 1
                        end if
                        if (poker(13*(jj-1)+j-1) .ne. 0) then
                            nn1 = nn1 + 1
                        end if
                        if (poker(13*(jj-1)+j) .ne. 0) then
                            nn2 = nn2 + 1
                        end if
                        if (poker(13*(jj-1)+j+1) .ne. 0) then
                            nn1 = nn1 + 1
                        end if
                        if (poker(13*(jj-1)+j+2) .ne. 0) then
                            nn1 = nn1 + 1
                        end if
                    end do
                end if
                expection(i,j) = 1.d0*(nn1*1.d0/nleft) + 2.d0*(nn2*1.d0/nleft)
                potention1(i,j) = nn1*1.d0/nleft
                potention2(i,j) = nn2*1.d0/nleft
            end do
        end do
        optimus = minval(expection)
        do i = 1, 4
            do j = 1, 13
                if (expection(i,j) .eq. optimus) then
                    write(*,"(A,I1,2X,A,I2,2X,A,F8.6)") "最佳花色：", i, "最佳点数：", j, "数学期望：", expection(i,j)
                    write(*,"(A,F8.6,2X,A,F8.6)") "喝一杯概率：", potention1(i,j), "喝两杯概率：", potention2(i,j)
                end if
            end do
        end do
        write(*,*) "输入本次开牌花色（点数x10+花色，红桃为1，方块为2，黑桃为3，草花为4）："
        read(*,*) pokerShow
        do i = 1, 54
            if (poker(i) .eq. pokerShow) then
                poker(i) = 0
                nleft = nleft - 1
            end if
        end do
    end do
    pause
end program main
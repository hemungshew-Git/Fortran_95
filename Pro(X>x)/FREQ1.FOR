        INTEGER A(2,3),B(6)
        
        DO 1 I=1,2
        READ(*,*)(A(I,J),J=1,3)
1       CONTINUE
        
        k=1

        DO 7 I=1,2
        DO 8 J=1,3 
        
        b(k)=a(i,j)

        k=k+1

8       CONTINUE
7       CONTINUE

        WRITE(*,*) (b(k), k=1,6)

*****************************************************************


        STOP
        END


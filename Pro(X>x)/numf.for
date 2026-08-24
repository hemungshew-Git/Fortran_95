c     To write a 2-D ARRAY into single 1-D array

      Dimension a(10,10), b(100)

      open(unit= 4, file='num.dat', status='old')
      open(unit= 5, file='num.out', status='new')

      do 1 i=1,4

      read(4,*) (a(i,j),j=1,4)
1     continue
c-------------------------------------------------------------------
      write(5,*) 'now write the matrix row-wise'

      do 2 i=1,4
      do 3 j=1,4

C     a1=a(i,j)

      write(5,100) a(i,j)

100   format(f7.2 )


3     continue
2     continue
c-------------------------------------------------------
      write(5,*) 'write the matrix column wise'

      do 4 i=1,4
      do 5 j=1,4

c     a2=a(j,i)

      write(5,101) a(j,i)

101   format(10x,f7.2)

5     continue
4     continue
c------------------------------------------------------------
       
      k=1

      do 6 i = 1,4
      do 7 j = 1,4

      b(k)=a(i,j)

      k=k+1

7     continue
6     continue

      write(5,*) 'now the 1-D matrix b(k) = a(i,j)'

      write(5,102)(b(k),k=1,16)

102   format(2x,f7.2)
c------------------------------------------------------------------

      write(5,*) 'now the 1-D matrix by using formula '


      do 9 i = 1,4
      do 10 j = 1,4

      b(k)= b( j*(i-1)+j  )

c     below in the formula: 4 is the total number of column in 2-D array

C      b(k)= b(4*(i-1)+j )


      write(5,103)b(k)

103   format(2x,f7.2)
10    continue
9     continue

c--------------------------------------------------------
      write(5,*)'now 2-D Array from the 1D Array b(k)'

      do 11 i=1,4
      do 12 j=1,4

      b(k) = b(j*(i-1)+j )

      a(i,j)= b(k)

12    continue
11    continue

      do 13 i=1,4

      write(5,104) (a(i,j), j=1,4)

104   format(2x,16f7.2)
13    continue

      end

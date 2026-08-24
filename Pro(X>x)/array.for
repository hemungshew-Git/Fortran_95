        dimension sum(3), a(3), b(3)
        real sum1,a1,b1,a,b,sum
        data a /2.0, 3.0, 4.0 /
        data b /1.0, 1.0, 1.1 /
 
        
c       read(*,*) (a(i),i=1,3), (b(i), i=1,3)

        do 2 i=1,3

        a1=a(i)
        b1=b(i)
        
        sum1=a1+b1

        sum(i)=sum1

2       continue

        do 3 i=1,3

        write(*,*)'********************************'
        write(*,*) sum(i)
        write(*,*)'********************************'
        
3       continue

        write(*,*)'********************************'
        
        write(*,*) (sum(i),i=1,3)

        write(*,*)'********************************'

        end

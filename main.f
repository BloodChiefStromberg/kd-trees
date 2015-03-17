c       labels that begin with 4 are 4 4mat statements
c		ASSUMPTIONS: 
c       No row or query can contain more than 50 characters
c       You may run a maximum of 1000 queries over 1000 rows
c		NOTE: The value 300000  for the string space 
c       came from (50*1000)(50*1000)+1000
c				      ^rows  ^queries  ^spaces


        PROGRAM main
        CALL readTable
        END

        SUBROUTINE readTable
c       I set the max row size to 50. Use substitute to change if necessary
        COMMON names, colors, locations, next, previous, stringSpace,
     1  spaceEnd, rows
        INTEGER rows,I,names(1001),colors(1001),locations(1001),
     1  next(1001),previous(1001),spaceEnd
        CHARACTER stringSpace(300000), currentRow(50)

c       this is the index where the string space ends
        spaceEnd = 1

c       not sure if this is necessary
        CALL cleanString(currentRow, 50)
        CALL cleanCommon

        READ(*,*), rows
        IF (rows .GT. 1000) GOTO 100
        IF (rows .LT. 1) GOTO 100


c       Create the head node
        names(1) = -1
        colors(1) = -1
        locations(1) = -1
        next(1) = 2
        previous(1) = rows + 1

        DO 10, I = 2, rows + 1
            READ 41, currentRow
 41         FORMAT(1000A)
            CALL addRow(currentRow, I)
            CALL cleanString(currentRow, 50)
 10     CONTINUE 

        CALL displayTable(rows)
        WRITE(*,*) ' '
c        CALL swap(2, 3, names)
        CALL sort(rows, names)
        WRITE(*,*) 'SORTED BY NAMES'
        CALL displayTable(rows)
        WRITE(*,*) ' '
        WRITE(*,*) 'SORTED BY LOCATION'
        CALL sort(rows, locations)
        CALL displayTable(rows)
        CALL handleQueries
        GOTO 101
 100    WRITE(*,*) "Number must be between 1 and 1000, Exiting"
 101    END

        SUBROUTINE handleQueries
        COMMON names, colors, locations, next, previous, stringSpace,
     1  spaceEnd, rows, queryIndexs
        INTEGER queries,names(1001),colors(1001),locations(1001),
     1  next(1001),previous(1001),spaceEnd,queryIndexs(3000)
        CHARACTER stringSpace(300000), query(50)
        REAL misses

        READ(*,*) queries
        IF (queries .GT. 1000) GOTO 102
        IF (queries .LT. 1) GOTO 102

        DO 65 I = 1, queries
            READ 45, query
 45         FORMAT(1000A)
            queryIndexs(I) = spaceEnd
            CALL copy(query, 1, 50)
            CALL cleanString(query, 50)
 65     CONTINUE

        DO 73 I = 1, queries 
            misses = 0
            misses = misses + check(queryIndexs(I), names)
            misses = misses + check(queryIndexs(I), colors)
            misses = misses + check(queryIndexs(I), locations)
            IF (misses .NE. 3) GOTO 73
            PRINT 47, 'No match for '
            CALL printFromSpace(queryIndexs(I))
            WRITE(*,*) ' '
 47         FORMAT(20A$)
 73     CONTINUE
        GOTO 103
 102    WRITE(*,*) 'Number of queries must be between 1 and 1000'
 103    END


        SUBROUTINE printRow(num)
        COMMON names, colors, locations
        INTEGER names(1001),colors(1001),locations(1001),num
 44     FORMAT(1A$)
        CALL output(num, names)
        PRINT 44, ' '
        CALL output(num, colors)
        PRINT 44, ' '
        CALL output(num, locations)
        WRITE(*,*) ' '
        END

c		why won't integer work?
        REAL FUNCTION check(queryIndex, field)
        COMMON names, colors, locations, next, previous, stringSpace,
     1  spaceEnd, rows
        INTEGER names(1001),colors(1001),locations(1001),
     1  next(1001),previous(1001),spaceEnd,field(1001),queryIndex,rows
        CHARACTER stringSpace(300000)
        
        DO 90 I = 2, rows + 1
            IF (compare(queryIndex, field(I)) .NE. 0) GOTO 90
            CALL printRow(I)
            check = 0
            GOTO 99
 90     CONTINUE   
        check = 1
 99     END 

        SUBROUTINE printFromSpace(indx)
        COMMON names, colors, locations, next, previous, stringSpace,
     1  spaceEnd, rows
        INTEGER names(1001),colors(1001),locations(1001),
     1  next(1001),previous(1001),spaceEnd
        CHARACTER stringSpace(300000)

        DO 88 I = 0, 50
            IF (stringSpace(indx + I) .EQ. ' ') GOTO 87
 46         FORMAT(1A$)
            PRINT 46, stringSpace(indx + I)
 88     CONTINUE
 87     END 

        SUBROUTINE displayTable(rows)
        COMMON names, colors, locations, next, previous, stringSpace,
     1  spaceEnd
        INTEGER names(1001),colors(1001),locations(1001),
     1  next(1001),previous(1001),spaceEnd,rows,indx
        CHARACTER stringSpace(300000)
        
c       get the first node index
        indx = next(1)
        DO 66 I = 2, rows +1
            CALL printRow(I)
            indx = next(indx)
 66     CONTINUE
        END

        SUBROUTINE output(indx, field)
        COMMON names, colors, locations, next, previous, stringSpace,
     1  spaceEnd
        INTEGER names(1001),colors(1001),locations(1001),
     1  next(1001),previous(1001),spaceEnd,indx,field(1001)
        CHARACTER stringSpace(300000)
        DO 67 I = 0, 50
            IF (stringSpace(field(indx) + I) .EQ. ' ') GOTO 50
            PRINT 42, stringSpace(field(indx) + I)
 42         FORMAT(1A$)
 67     CONTINUE       
 50     END 

        REAL FUNCTION compare(ele1, ele2)
        COMMON names, colors, locations, next, previous, stringSpace,
     1  spaceEnd
        INTEGER names(1001),colors(1001),locations(1001),
     1  next(1001),previous(1001),spaceEnd,ele1,ele2
        CHARACTER stringSpace(300000), char1, char2
        DO 53 I = 0, 50
            char1 = stringSpace(ele1 + I)
            char2 = stringSpace(ele2 + I)
            IF (char1 .EQ. ' ') GOTO 55
            IF (char2 .EQ. ' ') GOTO 57
            IF (char1 .LT. char2) GOTO 54
            IF (char1 .GT. char2) GOTO 58
 53     CONTINUE 

 55     IF (char2 .NE. ' ') GOTO 54
        GOTO 59
 57     IF (char1 .NE. ' ') GOTO 58
        GOTO 59

 54     compare = 1
        GOTO 56       
 58     compare = -1
        GOTO 56
 59     compare = 0
        GOTO 56
 56     END

        SUBROUTINE testCompare
        COMMON names, colors, locations, next, previous, stringSpace,
     1  spaceEnd
        INTEGER names(1001),colors(1001),locations(1001),
     1  next(1001),previous(1001),spaceEnd
        CHARACTER stringSpace(300000)

        WRITE(*,*) 'TEST: ', compare(3,2)
        WRITE(*,*) ' '
        CALL output(2, names)
        WRITE(*,*) ' '
        CALL output(3, names)
        END
        
        SUBROUTINE sort(rows, field)
        COMMON names, colors, locations, next, previous, stringSpace,
     1  spaceEnd
        INTEGER rows,names(1001),colors(1001),locations(1001),
     1  next(1001),previous(1001),spaceEnd,prior,field(1001)
        CHARACTER stringSpace(300000)

        DO 70 I = 3, rows + 1
            DO 71 J = 0, I - 3
c                WRITE(*,*) 'J = ', J
c                WRITE(*,*) 'I - 3 = ', I - 3
                prior = I - J - 1
                IF (compare(field(I-J), field(prior)) .NE. 1) GOTO 71
c                WRITE(*,*) ' '
c                WRITE(*,*) 'Swapping names'
                CALL swap(I-J, prior, names)
c                CALL displayTable(rows)
                CALL swap(I-J, prior, colors)
c                WRITE(*,*) ' '
c                WRITE(*,*) "Swapping colors"
c                CALL displayTable(rows)
                CALL swap(I-J, prior, locations)
c               WRITE(*,*) ' '
c                WRITE(*,*) "Swapping locations"
c                CALL displayTable(rows)
 71         CONTINUE
 70     CONTINUE
        END           
            
        SUBROUTINE swap(indx1, indx2, field)
        COMMON names, colors, locations, next, previous, stringSpace,
     1  spaceEnd
        INTEGER names(1001),colors(1001),locations(1001),
     1  next(1001),previous(1001),spaceEnd,field(1001),temp
        CHARACTER stringSpace(300000)
        temp = field(indx1)
        field(indx1) = field(indx2)
        field(indx2) = temp
        END
        
        SUBROUTINE cleanString(array, length)
        INTEGER length
        CHARACTER array(length)
        DO 11 I = 1, length
            array(I) = ' '
 11     CONTINUE
        END

        SUBROUTINE cleanInts(array, length)
        INTEGER length
        INTEGER array(length)
        DO 14 I = 1, length
            array(I) = 0
 14     CONTINUE
        END

        SUBROUTINE cleanCommon
        COMMON names, colors, locations, next, previous, stringSpace 
        INTEGER names(1001),colors(1001),locations(1001),
     1  next(1001),previous(1001)
        CHARACTER stringSpace(300000)
        CALL cleanInts(names, 1001)
        CALL cleanInts(colors, 1001)
        CALL cleanInts(locations, 1001)
        CALL cleanInts(next, 1001)
        CALL cleanInts(previous, 1001)
        CALL cleanString(stringSpace, 300000)
        END

        SUBROUTINE addRow(currentRow, indx)
        COMMON names, colors, locations, next, previous, stringSpace,
     1  spaceEnd 
        INTEGER names(1001),colors(1001),locations(1001),
     1  next(1001), previous(1001), wordStart, spaceEnd, length
        CHARACTER stringSpace(300000), currentRow(50)
        wordStart = 1

c       find the name
        DO 12, I = 1, 50
            IF (currentRow(I) .EQ. ' ') GOTO 20
 12     CONTINUE
 20     names(indx) = spaceEnd
        length = I - wordStart
        CALL copy(currentRow, wordStart, length)
   
c       find the next non-whitespace char
        DO 17, I = I, 50
            IF (currentRow(I) .NE. ' ') GOTO 25
 17     CONTINUE
        
c       find the color
 25     wordStart = I
        DO 18, I = I, 50
            IF (currentRow(I) .EQ. ' ') GOTO 36
 18     CONTINUE
 36     colors(indx) = spaceEnd
        length = I - wordStart
        CALL copy(currentRow, wordStart, length)

c       find the next non-whitespace char
        DO 19, I = I, 50
            IF (currentRow(I) .NE. ' ') GOTO 27
 19     CONTINUE

c       find the location
 27     wordStart = I
        DO 30, I = I, 50
            IF (currentRow(I) .EQ. ' ') GOTO 26
 30     CONTINUE
 26     locations(indx) = spaceEnd
        length = I - wordStart
        CALL copy(currentRow, wordStart, length)

c       set the next and previous fields
        next(indx) = indx + 1
        previous(indx) = indx - 1
        END          

        SUBROUTINE copy(string, start, length)
        COMMON names, colors, locations, next, previous, stringSpace,
     1  spaceEnd 
        INTEGER names(1001),colors(1001),locations(1001),
     1  next(1001), previous(1001), start, spaceEnd, length
        CHARACTER stringSpace(300000), string(50)
 
        DO 15 I = 1, length
c            WRITE(*,*) 'space end now: ', spaceEnd + (I-1)
c            WRITE(*,*) I + start - 1
            stringSpace(spaceEnd + (I - 1)) = string(I + start - 1)
 15     CONTINUE
        spaceEnd = spaceEnd + length + 1
        stringSpace(spaceEnd - 1) = ' '
        END

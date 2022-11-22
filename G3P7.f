* Linked List Program
      program linkedList
      implicit none

* Creating needed variables
      character*30 :: inputFile, outputFile
      character*10 :: nameRead
      character*40 :: outputMsg
      character :: overwrite
      logical :: exists
      integer :: status, istat, countRead, dataRead, i, j, counter
      integer :: curCount, tempCount

* Create Node type
      type :: node
        character*10 :: name
        integer :: count
        type(node), pointer :: next, prev
      end type

* Create Node pointers
      type(node), pointer :: head, tail, p, print
      nullify(head, tail)

* Formats
1     format(" ", "Node ", I2, " - Name: ", A10, " - Count: ", I3)
2     format(" ", "Node Removed - Name: ", A10, " - Count: ", I3)
3     format(" ", "Start Node - Name: ", A10, " - Loaded Count: ", I3)
4     format(" ", "Final Node - Name: ", A10, " - Count: ", I3)
5     format(" ", "Data entry complete - Linked list created with ",
     C I2, " nodes")

* Prompt for input file
      print *, "Enter input file name:"
      read(*,"(A30)") inputFile
      inquire(file = inputFile, exist = exists)

* Keep prompting if file DNE or quit is entered
      do while (exists .neqv. .true. .and. inputFile .ne. "quit")
        print *, "File does not exist"
        print *, "Enter input file name:"
        read(*,"(A30)") inputFile
        inquire(file = inputFile, exist = exists)
      end do

* If quit is not entered, open file
      if (inputFile .ne. "quit") then
        open(1, file = inputFile, status = "OLD", iostat = status)
        if (status .eq. 0) then
          print *, "Input file opened"

* Prompt for output file
          print *, "Enter output file name:"
          read (*,"(A30)") outputFile
          inquire(file = outputFile, exist = exists)

* If exists and quit is not entered, prompt to overwrite.
          if (exists .and. outputFile .ne. "quit") then
            if (outputFile .eq. inputFile) then
              print *, "File already opened"
            else
              print *, "File already exists. Overwrite? (y/n)"
              read(*,"(A)") overwrite
            end if

* If overwrite is no & quit is not entered, keep prompting for file
            do while (outputFile .ne. "quit" .and. exists .eqv. .true.
     C .and. overwrite .ne. "y")
              print *, "Enter output file name:"
              read (*,"(A30)") outputFile
              inquire(file = outputFile, exist = exists)

              if (exists) then
                if (outputFile .eq. inputFile) then
                  print *, "File already opened"
                else
                  print *, "File already exists. Overwrite? (y/n)"
                  read(*,"(A)") overwrite
                end if
              end if
            end do
          end if

* If overwrite or valid file name is typed, continue
          if (outputFile .ne. "quit") then
            if (overwrite .eq. "y") then
* Open with replace
              open(2, file = outputFile, status = "REPLACE",
     C iostat = status)
              outputMsg = "Output file replaced and opened"
            else
* Open with new
              open(2, file = outputFile, status = "NEW",
     C iostat = status)
              outputMsg = "Output file created and opened"
            end if

* Print corresponding messages
            if (status .eq. 0) then
              print *, outputMsg

* Print output file heading
              write(2,*) "Output File:"

* Read data from file
              read(1,*, iostat = status) nameRead
              read(1,*, iostat = status) countRead

* If read error, keep dataRead = 0, if success, add 1 to data read
              dataRead = 0

* Loop will read data until a read error occurs or records is above 25
* Have while loop set to <= 24 as that is how the file is read
* 24 means 25 records is read

* If data is missing components like a name or a count, program
* will stop reading and complete data entry
              do while (status .eq. 0 .and. dataRead .le. 24)

* Make sure data is valid
              if (countRead .ne. 0) then

* Allocate p
                allocate(p, stat = istat)
                if (istat .eq. 0) then
                  nullify(p%next)
                  nullify(p%prev)

* Assign read data to p variables
                  p%name = nameRead
                  p%count = countRead

* Add node to end of link list
                  if (associated(head)) then
                    tail%next=>p
                    p%prev=>tail
                    tail=>p
* Add initial node to link list
                  else
                    head=>p
                    tail=>p
                  end if

* Read data again
                  dataRead = dataRead + 1
                  print *, ""
                  print *, "Node added to linked list"
                  write(2,*) ""
                  write (2,*) "Node added to linked list"
                end if

* If data is invalid, let user know
              else
                print *, ""
                write(2,*) ""
                if (countRead .eq. 0) then
                  print *, "Count is 0"
                  write(2,*) "Count is 0"
                end if
                  print *, "Insufficient data, node not added"
                  write(2,*) "Insufficient data, node not added"
              end if

* Read next lines of data
                read(1,*, iostat = status) nameRead
                read(1,*, iostat = status) countRead

              end do

* If there is data present, continue
              if (dataRead .ne. 0) then
                print *, ""
                print 5, dataRead
                print *, ""

                write(2,*) ""
                write(2,5) dataRead
                write(2,*) ""

* Point p to head
                p=>head
                curCount = p%count

* Continue looping until there is only 1 node left. No prev and no next
                do while(associated(p%prev) .or. associated(p%next))

* Print starting node
                  print 3, p%name, curCount
                  write(2,3) p%name, curCount

* Print list
                  counter = 1
                  print=>head
                  do while (associated(print))
                    print 1, counter, print%name, print%count
                    write(2,1) counter, print%name, print%count
                    counter = counter + 1
                    print=>print%next
                  end do

* Traverse backwards count times, if head is reached, goes back to tail
                  if (curCount .lt. 0) then
                    tempCount = -curCount
                    do i = 1, tempCount
                      if (.not. associated(p%prev)) then
                        p=>tail
                      else
                        p=>p%prev
                      end if
                    end do

* Traverse forwards count times, if tail is reached, goes back to head
                  else if (curCount .gt. 0) then
                    do i = 1, curCount
                      if (.not. associated(p%next)) then
                        p=>head
                      else
                        p=>p%next
                      end if
                    end do
                  end if

* Print node that is to be removed
                  print 2, p%name, p%count
                  print *, ""

                  write (2,2) p%name, p%count
                  write(2,*) ""

* Sets curCount to the count about to be removed
                  curCount = p%count

* Remove - If p is the head
                  if (.not. associated(p%prev)) then
                    if (associated(p%next)) then
                      head=>head%next
                      deallocate(head%prev)
                      nullify(head%prev)

* Chooses next node based on loaded count
                      if (curCount .lt. 0) then
                        p=>tail
                      else if (curCount .gt. 0) then
                        p=>head
                      end if
                    end if

* Remove - If p is the tail
                  else if (.not. associated(p%next)) then
                    if (associated(p%prev)) then
                      tail=>tail%prev
                      deallocate(tail%next)
                      nullify(tail%next)

* Chooses next node based on loaded count
                      if (curCount .lt. 0) then
                        p=>tail
                      else if (curCount .gt. 0) then
                        p=>head
                      end if
                    end if

* Remove - If p is in center
                  else
                    if (associated(p)) then
                      if (associated(p%prev)) then
                        p%prev%next=>p%next
                      end if
                      if (associated(p%next)) then
                        p%next%prev=>p%prev
                      end if

* Chooses next node based on loaded count
                      if (curCount .lt. 0) then
                        p=>p%prev
                      else if (curCount .gt. 0) then
                        p=>p%next
                      end if
                    end if
                  end if

                end do

* Print only node that is left
                print 4, p%name, p%count
                write(2,4) p%name, p%count

* If no readable data was found, throw error
              else
                print *, ""
                print *, "No data present"

                write(2,*) ""
                write(2,*) "No data present"
              end if

* Close files
              close(1)
              close(2)
              print *, ""
              print *, "Files closed"

* File error catches
            else
              print *, "Output file error"
            end if

          else
            print *, "Quit program"
          end if
        else
          print *, "Input file error"
        end if
      else
        print *, "Quit program"
      end if

      end program linkedList
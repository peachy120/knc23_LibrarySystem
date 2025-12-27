type Book = (String, String, String, String, String)    -- W2P5 n-tuples 
type User = (String, String, String)    -- W2P5 n-tuples

main :: IO ()
main = function [] []

function :: [Book] -> [User] -> IO ()
function  bookList userList = do
	putStrLn "Please enter the number referencing an action from below"
	putStrLn "1 - addBook"  -- W4P6 an IO action that prints its input to the terminal
        putStrLn "2 - removeBook"
        putStrLn "3 - addUser"
        putStrLn "4 - removeUser"
        putStrLn "5 - borrowBook"
        putStrLn "6 - returnBook"
        putStrLn "7 - availableBooks"
        putStrLn "8 - borrowedBooks"
        putStrLn "9 - listBooks"
        putStrLn "10 - listUsers"
        putStrLn "exit - Exit program"
        action <- getLine       -- W4P8 <- is an operator that links the retireved value to the identifier; getLine retrieve a line of input from the user
	if action == "1"
	        then do
                        putStrLn ("User selects: addBook")
                        
                        newBook <- addBook bookList     -- W4P9 calling pure function : addBook
                        print newBook

                        function newBook userList
        
        else if action == "2"
                then do
                        putStrLn ("User selects: removeBook")

                        oldBook <- removeBook bookList
                        print oldBook

                        function oldBook userList

        else if action == "3"
                        then do
                        putStrLn ("User selects: addUser")
                        
                        newUser <- addUser userList     -- W4P9 calling pure function : addUser
                        print newUser

                        function bookList newUser      

        else if action == "4"
                then do
                        putStrLn ("User selects: removeUser")  

                        oldUser <- removeUser userList bookList
                        print oldUser

                        function bookList oldUser

        else if action == "5"
                then do
                        putStrLn ("User selects: borrowBook")

                        borrowNewBook <- borrowBook bookList userList     -- W4P9 calling pure function : borrowBook
                        print borrowNewBook

                        function borrowNewBook userList

        else if action == "6"
                then do
                        putStrLn ("User selects: returnBook") 

                        returnABook <- returnBook bookList     -- W4P9 calling pure function : returnBook
                        print returnABook

                        function returnABook userList

        else if action == "7"
                then do
                        putStrLn ("User selects: availableBooks")

                        availableBooks bookList     -- W4P9 calling pure function : availableBooks

                        function bookList userList

        else if action == "8"
                then do
                        putStrLn ("User selects: borrowedBooks")     

                        borrowedBooks bookList     -- W4P9 calling pure function : borrowedBooks

                        function bookList userList

        else if action == "9"
                then do
                        putStrLn ("User selects: listBooks")

                        listBooks bookList     -- W4P9 calling pure function : listBooks

                        function bookList userList

        else if action == "10"
                then do
                        putStrLn ("User selects: listUser")

                        listUsers userList     -- W4P9 calling pure function : listUsers

                        function bookList userList

        else if action == "exit"
                then do
                        putStrLn ("Bye for now :D")
                    
	else do putStrLn "Invalide Input!"
                function bookList userList
        
currentMaxBookID :: [Book] -> Int
currentMaxBookID [] = 0
currentMaxBookID ((bookID, _, _, _, _):xs) = 
        let bkID = read bookID :: Int                   -- read is built-in haskell function that converts string into another type
            remainBookMaxID = currentMaxBookID xs               -- find the current largest bookID recursively
        in if bkID > remainBookMaxID
                then bkID 
                else remainBookMaxID


addBook :: [Book] -> IO [Book]
addBook book = do 
        let nextBookID = show (currentMaxBookID book + 1)

        putStrLn "Enter Book Title"             -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
        addBook_Title <- getLine                -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 8

        putStrLn "Enter Book Author"            -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
        addBook_Author <- getLine               -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 8

        let newBook = book ++ [(nextBookID, addBook_Title, addBook_Author, "available", "")]    -- CI505 Introduction to Programming Lecture PowerPoint Week 1 Page 29
        -- W4P11 let store the result of a pure function

        putStrLn "book added"
        return newBook  -- W4P12? return is a function that wraps up the input in an IO action

removeBook :: [Book] -> IO[Book]
removeBook book = do
        putStrLn "Enter the ID of the book you want to delete"  -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
        removeBook_ID <- getLine                                -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 8

        let checkBook = any(\(bookID, _, _, bookStatus, _) -> bookID == removeBook_ID) book

        if not checkBook
                then do
                        putStrLn "!!!Error!!! : Book does not exists"           -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
                        return book
        else do
                let checkBook = any(\(_, _, _, bookStatus, _) -> bookStatus == "borrowed") book
                
                if checkBook
                        then do
                                putStrLn "!!!Error!!! : This book is currently being borrowed"          -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
                                return book
                else do
                        let oldBook = filter (\(bookID, _, _, bookStatus, _) -> 
                                not (bookID == removeBook_ID && bookStatus == "available")) book
                        putStrLn ("BookID : " ++ show removeBook_ID ++ "has been removed")              -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6

                        return oldBook

currentMaxUserID :: [User] -> Int
currentMaxUserID [] = 0
currentMaxUserID ((userID, _, _):xs) = 
        let urID = read userID :: Int                   -- read is built-in haskell function that converts string into another type
            remainUserMaxID = currentMaxUserID xs               -- find the current largest userID recursively
        in if urID > remainUserMaxID
                then urID 
                else remainUserMaxID

addUser :: [User] -> IO [User]
addUser user = do 
        let nextUserID = show (currentMaxUserID user + 1)

        putStrLn "Enter User Firstname"         -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
        addUser_FName <- getLine                -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 8

        putStrLn "Enter User Lastname"          -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
        addUser_LName <- getLine                -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 8

        let newUser = user ++ [(nextUserID, addUser_FName, addUser_LName)]    -- W4P11 let store the result of a pure function

        putStrLn "User added"           -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
        return newUser

removeUser :: [User] -> [Book] -> IO[User]
removeUser user book = do
        putStrLn "Enter the ID of the user you want to delete"          -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
        removeUser_ID <- getLine                                        -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 8

        let checkUser = any(\(userID, _, _) -> userID == removeUser_ID) user

        if not checkUser
                then do
                        putStrLn "!!!Error!!! : User ID does not exists"        -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
                        return user
        else do
                let checkBook = any(\(_, _, _, _, prevCurUser) -> prevCurUser == removeUser_ID) book
                
                if checkBook
                        then do
                                putStrLn "!!!Error!!! : User has not return all the book"       -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
                                return user
                else do
                        let oldUser = filter(\(userID, _, _) -> userID /= removeUser_ID) user           -- CI505 Introduction to Programming Lecture PowerPoint Week 1 Page 37
                        putStrLn ("UserID : " ++ show removeUser_ID ++ "has been removed")              -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6

                        return oldUser


borrowBook :: [Book] -> [User] -> IO [Book]
borrowBook borrowBK borrowUR= do
        putStrLn "Enter Book ID"                -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
        borrowBook_bookID <- getLine            -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 8
        putStrLn "Enter User ID"                -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
        borrowBook_userID <- getLine            -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 8

        let checkBook = any(\(bookID, _, _, _, _) -> bookID == borrowBook_bookID) borrowBK

        if not checkBook
                then do
                        putStrLn "!!!Error!!! : Book does not exists"           -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
                        return borrowBK

        else do
                let checkBookStatus = any(\(bookID, _, _, bookStatus, _) -> bookID == borrowBook_bookID && bookStatus == "borrowed") borrowBK

                if checkBookStatus
                        then do 
                                putStrLn "!!!Error!!! : Book is currently being borrowed"               -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
                                return borrowBK

                else do
                        let checkUser = any(\(userID, _, _) -> userID == borrowBook_userID) borrowUR
                        
                        if not checkUser
                                then do
                                        putStrLn "!!!Error!!! : User does not exists"           -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
                                        return borrowBK
                        else do
                                let borrowBookaction = map (\(bookID, bookTitle, bookAuthor, bookStatus, prevCurUser) ->        -- CI505 Introduction to Programming Lecture PowerPoint Week 1 Page 36
                                                                if bookID == borrowBook_bookID
                                                                        then 
                                                                                let 
                                                                                        updateStatus = if bookStatus == "available" then "borrowed" else "available"
                                                                                in (bookID, bookTitle, bookAuthor, updateStatus, borrowBook_userID)
                                                                        else (bookID, bookTitle, bookAuthor, bookStatus, prevCurUser)
                                                        ) borrowBK
                                                        
                                putStrLn "Book set to borrowed!"        -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
                                return borrowBookaction

returnBook :: [Book] -> IO [Book]
returnBook returnBK = do        
        putStrLn "Enter Book ID"                -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
        returnBook_bookID <- getLine            -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 8
        
        let checkBook = any(\(bookID, _, _, _, _) -> bookID == returnBook_bookID) returnBK

        if not checkBook
                then do
                        putStrLn "!!!Error!!! : Book doesn't exists"
                        return returnBK
        else do 
                let returnBookaction = map (\(bookID, bookTitle, bookAuthor, bookStatus, prevCurUser) -> 
                        if bookID == returnBook_bookID
                                then
                                        let 
                                                updateStatus = if bookStatus == "borrowed" then "available" else "borrowed"
                                                updateCurUserID = if updateStatus == "borrowed" then prevCurUser else ""
                                        in (bookID, bookTitle, bookAuthor, updateStatus, updateCurUserID)
                                else (bookID, bookTitle, bookAuthor, bookStatus, prevCurUser)
                        ) returnBK
                                   
                putStrLn "Book set to available!"       -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
                return returnBookaction

availableBooks :: [Book] -> IO ()
availableBooks [] = putStrLn "There are no books in the system currently"       -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
availableBooks availBKList = do
        putStrLn "Books that are available in the system currently :"           -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
        mapM_ (\(bookID, bookTitle, bookAuthor, bookStatus, prevCurUser) ->     -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 19
                if bookStatus == "available"
                        then putStrLn (bookID ++ " , " ++ bookTitle ++ " , " ++ bookAuthor ++ " | " ++ bookStatus ++ " Previous / Current User ID: " ++ prevCurUser)
                        else return ()
              ) availBKList     -- W4P19 mapM_ encapsulate the common problem of mapping an IO action over an input list then sequencing it or performing all the actions, then throws away the result later

borrowedBooks :: [Book] -> IO ()
borrowedBooks [] = putStrLn "There are no books in the system currently"                -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
borrowedBooks borBKList = do
        putStrLn "Books that are borrowed in the system currently :"            -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
        mapM_ (\(bookID, bookTitle, bookAuthor, bookStatus, prevCurUser) ->     -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 19
                if bookStatus == "borrowed"
                        then putStrLn (bookID ++ " , " ++ bookTitle ++ " , " ++ bookAuthor ++ " | " ++ bookStatus ++ " , " ++ "Current User ID: " ++ prevCurUser)
                        else return ()
              ) borBKList     -- W4P19 mapM_ encapsulate the common problem of mapping an IO action over an input list then sequencing it or performing all the actions, then throws away the result later

listBooks :: [Book] -> IO ()
listBooks [] = putStrLn "There are not books in the system currently"           -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
listBooks bkList = do 
        putStrLn "Books stored in the system currently :"               -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
        mapM_ (\(bookID, bookTitle, bookAuthor, bookStatus, prevCurUser) -> 
                        putStrLn (bookID ++ " , " ++ bookTitle ++ " , " ++ bookAuthor ++ " | " ++ bookStatus ++ " Current User ID: " ++ prevCurUser)
              ) bkList     -- W4P19 mapM_ encapsulate the common problem of mapping an IO action over an input list then sequencing it or performing all the actions, then throws away the result later


listUsers :: [User] -> IO ()
listUsers [] = putStrLn "There are not Users in the system currently"           -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
listUsers uList = do 
        putStrLn "Users stored in the system currently :"               -- CI505 Introduction to Programming Lecture PowerPoint Week 4 Page 6
        mapM_ (\(userID, userFName, userLName) -> 
                        putStrLn (userID ++ " , " ++ userFName ++ " , " ++ userLName)
              ) uList     -- W4P19 mapM_ encapsulate the common problem of mapping an IO action over an input list then sequencing it or performing all the actions, then throws away the result later

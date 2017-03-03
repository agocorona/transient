



main2 = keep'   $ do
  top <- topState
  r <- group 9 $ do
     x <-   choose [1,2,3]
     labelState "async1"

     showThreads top
     liftIO $ threadDelay 1000000
     liftIO $ print x

     y <-   choose ['a','b','c']
     labelState "async2"
     showThreads top
--     liftIO $ threadDelay 3000000
     id <- liftIO myThreadId
     liftIO $ print (x,y,id)
  liftIO $ print r
--  liftIO $ threadDelay 1000000
  showThreads top



main3= keep $ do
     top <- topState
--     oneThread $ option "main" "to kill previous spawned processes and return to the main menu"   <|> return ""
--     liftIO $ putStrLn "MAIN MENU"
     showThreads top
     oneThread op1
     st <- getCont
     onFinish $ const $ do

       liftIO $ killChildren $ children st

     option "3" "3"
     liftIO $ print 3
     finish Nothing
     return ()

    where
    op1= do

       option "1" "1"
       labelState "1"
       liftIO $ print 1
    op2= do

       option "2" "2"
       labelState "2"
       liftIO $ print 2


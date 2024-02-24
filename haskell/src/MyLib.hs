module MyLib (someFunc) where

-- import Message (Message(Message))

someFunc :: IO ()
someFunc = putStrLn $ "someFunc" -- <> show testMessage

-- testMessage :: Message
-- testMessage = Message "src" "dst"

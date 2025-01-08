module Main where
import Text.Read()
import  Html

main :: IO ()
main = putStrLn (render_ myhtml)

myhtml :: Html
myhtml = 
   html_
    "About Me"
    (<>
      h1_ "Start of my blog")
      (<>
        p_ "I created this blog generator originally as a pseudo challenge from my friend Lucas to learn Haskell \
           \ and make an effort to embrace functional programming, or at the very least, acknowledge its importance \
           \ and balance my perspective between imperative programming and functional programming.")
        (p_ "Let the learning commence then!")
      
    



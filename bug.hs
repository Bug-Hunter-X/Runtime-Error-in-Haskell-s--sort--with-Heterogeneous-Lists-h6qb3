```haskell
import Data.List (sort)

main :: IO ()
main = do
  let xs = [1, 2, 3, 4, 5]
  let ys = sort xs
  print ys
```
This code will compile and run without errors.  However, if you try to use `sort` on a list containing elements of different types, you'll get a runtime error.  This is a common pitfall for beginners, and not always immediately obvious from the compiler's feedback.
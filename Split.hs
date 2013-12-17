module Split where

es = "user_name/pa$$wor*d@db"
dlm = "/@"

in' :: Char -> [Char] -> Bool
in' _ [] = False
in' c (x:xs) = x == c || c `in'` xs

type Delimiter = String 
type Accumulator = [String] 

rsplit :: String -> Delimiter -> Accumulator -> [String]
rsplit [] _ acc = acc
rsplit (x:xs) dlm acc
    | x `in'` dlm = sp ([]:acc)
    | null acc    = sp ([x]:[])
    | otherwise   = sp $ (x:head acc):tail acc
    where sp  = rsplit xs dlm

split :: String -> Delimiter -> [String]
split x dlm = map reverse $ reverse $ rsplit x dlm []


-- Find out whether a list is a palindrome. 
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = reverse xs == xs 
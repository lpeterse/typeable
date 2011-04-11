#!/usr/bin/runhaskell
module Main where

import Network.Curl
import System.Environment

main :: IO ()
main  = do args <- getArgs
           let host = if null args then "typeable.org" else args !! 0
           a <- curlGetString ("http://"++host++"/type") [] 
           case a of
             (CurlOK, s) -> do xs <- (download host) $ lines s
                               mapM_ (f host) $ zip (lines s) xs
                               b  <- readFile "typeable.cabal.tpl"
                               let ns = map (\n->"                  ,Typeable.T"++n) $ lines s 
                               writeFile "typeable.cabal" (b++(unlines ns))
                               print "Bootstrap successfull!"
             (x,      s) -> error $ "Can't download type listing: "++(show (x,s))
        where
          f h (n,x) = do writeFile ("src/Typeable/T"++n++".hs") x
                         putStr $ "Download bootfile of type "++n++": "
                         (c,y) <- curlGetString ("http://"++h++"/type/"++n++"?format=haskell-boot") []
                         case c of 
                           CurlOK -> writeFile ("src/Typeable/T"++n++".hs-boot") y >> putStrLn "OK"
                           _      -> putStrLn "Not available"

download   :: String -> [String] -> IO [String]
download h xs = do ys <- mapM (\x->let fu="http://"++h++"/type/"++x++"?format=haskell" in (putStrLn $ fu++": ") >> curlGetString (fu) []) xs
                   if all ((==CurlOK) . fst) ys 
                     then return $ map snd ys
                     else (mapM_ f $ filter ((/=CurlOK) . fst . fst) (zip ys xs)) >> error ""
                where
                  f (e,n) = putStrLn $ "Can't download type '"++n++"': "++(show e)

        

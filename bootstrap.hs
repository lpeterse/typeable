module Main where

import Network.Curl

main :: IO ()
main  = do a <- curlGetString "http://typeable.org/type" [] 
           case a of
             (CurlOK, s) -> do xs <- download $ lines s
                               mapM_ (\(n,x)->writeFile ("src/Typeable/T"++n++".hs") x)
                               b  <- readFile "typeable.cabal.tpl"
                               let ns = map (\(n,x)-> "                  ,Typeable.T"++n) xs
                               writeFile "typeable.cabal" (b++ns)
                               print "Bootstrap successfull!"
             (x,      s) -> error $ "Can't download type listing: "++(show (x,s))

download   :: [String] -> IO (String, String)
download xs = do ys <- mapM (\x->curlGetString ("http://typeable.org/type/"++x++"?format=haskell") []) xs
                 if all ((==CurlOK) . fst) ys 
                   then return $ zip xs $ map snd ys
                   else mapM_ f $ filter ((/=CurlOK) . fst . fst) (zip ys xs) >> error ""
              where
                f (e,n) = putStrLn $ "Can't download type '"++n++"': "++(show e)

        

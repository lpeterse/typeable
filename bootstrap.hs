module Main where

import Network.Curl

main :: IO ()
main  = do a <- curlGetString "http://typeable.org:5005/type" [] 
           case a of
             (CurlOK, s) -> do xs <- download $ lines s
                               mapM_ (\(n,x)->writeFile ("src/Typeable/T"++n++".hs") x) $ zip (lines s) xs
                               b  <- readFile "typeable.cabal.tpl"
                               let ns = map (\n->"                  ,Typeable.T"++n) $ lines s 
                               writeFile "typeable.cabal" (b++(unlines ns))
                               print "Bootstrap successfull!"
             (x,      s) -> error $ "Can't download type listing: "++(show (x,s))

download   :: [String] -> IO [String]
download xs = do ys <- mapM (\x->curlGetString ("http://typeable.org:5005/type/"++x++"?format=haskell") []) xs
                 if all ((==CurlOK) . fst) ys 
                   then return $ map snd ys
                   else (mapM_ f $ filter ((/=CurlOK) . fst . fst) (zip ys xs)) >> error ""
              where
                f (e,n) = putStrLn $ "Can't download type '"++n++"': "++(show e)

        

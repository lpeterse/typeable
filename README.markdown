# Installation

For installing the typeserver with the types included in the repository just type:

> cabal install

Maybe you first have to update cabal's package list. Do so by:

    cabal update

If everything compiled correctly you can now start a typeserver (i.e. on port 5000) by typing

    ~/.cabal/bin/typeserver 5000

# Manipulating Typedefinitions

Adding/Manipulating types is still a bit tricky, since the development of the system is still in progress.
The typeserver serves the types saved as `.ebf` in the directory `static/types`. Since there is no tool yet for comfortably working on `.ebf`-files we use an alternative way:
The file `scripts/generateTypes.hs` contains a list of types defined within Haskell itself. This form is meant to be persistent over changes of the binary format or slight changes in the type structure.
Do your manipulations to the file as you like. Maybe you even want to create a copy and use it as a template for creating your own collection.
If you have finished, execute the script from the current directory (don't cd into `scripts/`!). The script will now transform all typedefinitions to EBF and save them to where the typeserver can access them. You need to restart the typeserver in order to changes taking effect.

## Recompiling with the new typedefintions

In order to make the new typedefinitions available in Haskell they need to be 'haskellized' and the generated Haskell sourcefiles need to be compiled and registered.
We assume you have restarted the typeserver. For every type you can see with your browser what the generated Haskell code looks like.

The script `scripts/downloadHaskellizedTypes.hs` downloads all Haskell and Haskell-Boot files from a running typeserver and additionally fixes the cabal file by adding them.
Thus, what you need to do is:

    ./scripts/downloadHaskellizedTypes.hs localhost:5000
    cabal install

It might happen that your typedefinitions are invalid or trigger a bug in the system. The build then fails and you have to revert.

# Saving/Loading EBF-files

    let a = Left 123 :: Either Int String
    import Data.EBF
    let b = writeV00 a
    b
    > Chunk "\ESC[1;35mEBF00\r\n\SUB\n" (Chunk "&z\192g\197\\\230\172\144Z&\EOT\214\227*\176u,\ETXK \187oJ[\199\DC4\DLEg\254\159Z" (Chunk "\217\238\240\&8\180}\b \193`\206\184\182\168\153C\NUL\172.w\SI!2\172\237t\158\193\151\&8_\245R\SOH\NUL\v\168_?\DLE\t\156u\212\182\150\208\207\148N\t\NUL\SYN\244$]\243\204\vSO\STX\130\&5\255\138\174\SYN\SOH\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL{" Empty))
    readTypeV00 b
    > Node {rootLabel = d9eef038-b47d-0820-c160-ceb8b6a89943, subForest = [Node {rootLabel = ac2e770f-2132-aced-749e-c197385ff552, subForest = []},Node {rootLabel = 0ba85f3f-1009-9c75-d4b6-96d0cf944e09, subForest = [Node {rootLabel = 16f4245d-f3cc-0b53-4f02-8235ff8aae16, subForest = []}]}]}
    (readV00 b) :: Either Int String
    > Left 123

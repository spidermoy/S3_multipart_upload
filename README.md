
# AMAZON S3 AWS MULTI-PART UPLOAD
=======================

Upload files to S3 by using Haskell parallelism. By first, the file is split into several 
parts, and then, each part is uploaded in parallel.
If the upload of a part fails, the program will try to upload it automatically.
You need to have the S3-client configured.

This work takes the ideas from the following [tutorial](https://aws.amazon.com/es/premiumsupport/knowledge-center/s3-multipart-upload-cli/).

**Runs only on linux**

## Requeriments

   * GHC 8 or higher

cabal libraries:

   * Text.JSON (`cabal install json`)
   * Data.List.Utils (`cabal install MissingH`)


## How to use it

   1. Compile:

   `ghc -O2 --make -threaded S3PartsUpload.hs`

   2. Run the program splitting the input file into 10mb parts using two cores (minimum size is 5mb):
   
   `./S3PartsUpload input_file bucket_name 10M +RTS -N2`


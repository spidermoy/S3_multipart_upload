{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad(forM_, replicateM)
import Text.JSON
import Text.JSON.Generic
import System.Process(callCommand, readProcess)
import System.Environment(getArgs)
import System.IO.Error(catchIOError)
import Data.List(sort, sortBy)
import Data.List.Utils(replace)
import Control.Concurrent(forkIO, MVar, newEmptyMVar, putMVar, takeMVar)


(!)::(JSON a) => JSObject JSValue->String->Result a
(!) = flip valFromObj


data CreateMPU = CreateMPU 
    {
     bucket    :: String,
     key       :: String,
     upload_id :: String
    } deriving (Show, Data)

instance JSON CreateMPU where
    showJSON                = undefined
    readJSON (JSObject obj) =
        CreateMPU <$>
          obj ! "Bucket"
            <*>
          obj ! "Key"
            <*>
          obj ! "UploadId"


data UpPart = Part
    {
     etag_ :: String
    } deriving (Show, Data)

instance JSON UpPart where
    showJSON = undefined
    readJSON (JSObject obj) = case lookup "ETag" (fromJSObject obj) of
                                Nothing -> Ok $ Part {etag_ = ""}
                                _       -> Part <$> obj ! "ETag" 


data UpPartN = PartN 
    {
     partnumber :: Int,
     etag       :: String
    } deriving (Show, Data)

instance JSON UpPartN where
    showJSON = undefined
    readJSON (JSObject obj) = PartN <$> 
                                obj ! "PartNumber" 
                                  <*>
                                obj ! "ETag"


newtype Parts = Parts 
    {
     parts :: [UpPartN]
    } deriving (Show, Data)

instance JSON Parts where
    showJSON = undefined
    readJSON (JSObject obj) = Parts <$> 
                                obj ! "Parts" 


main::IO ()
main = do 
    [filepath, bucketS3, n_size] <- getArgs
    let input_file_name = change_ext (reverse $ takeWhile (/= '/') $ reverse filepath) ""
        parts_folder    = ".temp_parts_" ++ input_file_name
    catchIOError
        (do
    callCommand $ "mkdir " ++ parts_folder
    putStrLn $ "file: " ++ filepath
    putStrLn "splitting file..."
    split_output <- readProcess "split" ["--verbose","-b", n_size,"-d", filepath, 
                                          parts_folder ++ "/" ++ change_ext input_file_name "."] ""
    file_list    <- readProcess "find" [parts_folder, "-type", "f"] ""
    let n_parts    = length $ lines split_output
        files_path = (sort . lines) file_list
    putStrLn $ "splitted in " ++ show n_parts ++ " parts"
    putStrLn "creating multipart upload..."
    response <- readProcess "aws" ["s3api", "create-multipart-upload", "--bucket", bucketS3,
                                   "--key", filepath] ""
    let msj_create = decode response :: Result CreateMPU
    print msj_create
    let Ok json_create = msj_create
        key_           = key json_create
        upload_id_     = upload_id json_create
    putStrLn "\nuploading parts...\n"
    message <- newEmptyMVar
    forM_ [1 .. n_parts] (\i ->
                   forkIO $ upload_part files_path i bucketS3 key_ filepath upload_id_ message
        )
    _parts <- replicateM n_parts (takeMVar message)
    let parts_     = sortBy (\part' part'' -> compare (partnumber part') (partnumber part'')) _parts
        fileparts_ = parts_folder ++ "/" ++ input_file_name ++ "fileparts.json" 
    writeFile fileparts_ $ replace "\\\"" "" $ replace "parts" "Parts"           $ 
                                               replace "etag" "ETag"             $ 
                                               replace "partnumber" "PartNumber" $ 
                                               encodeJSON $ Parts {parts = parts_}
    response <- readProcess "aws" ["s3api", "complete-multipart-upload", "--multipart-upload", 
                                   "file://" ++ fileparts_, "--bucket", bucketS3, "--key", key_, 
                                   "--upload-id", upload_id_] "" 
    putStrLn $ "\n" ++ response
    putStrLn "file uploaded!"
    callCommand $ "rm -r " ++ parts_folder)
        (\e -> do
                callCommand $ "rm -r " ++ parts_folder
                print e)
    where
      change_ext::FilePath->String->FilePath
      change_ext file_name ext = let file_name' = dropWhile (/= '.') . reverse $ file_name in
                                 case file_name' of
                                   "" -> file_name ++  ext
                                   _  -> ((reverse . tail) file_name') ++ ext
      upload_part::[FilePath]->Int->String->String->FilePath->String->MVar UpPartN->IO ()
      upload_part files_path i bucketS3 key_ filepath upload_id_ message = catchIOError
          (do
          response <- readProcess "aws" ["s3api", "upload-part", "--bucket", bucketS3, "--key",
                                         key_, "--part-number", show i, "--body", 
                                         files_path !! (i-1), "--upload-id", upload_id_] ""
          case decode response :: Result UpPart of
            Ok json -> do
                        putStrLn $ "\tpart " ++ show i ++ " uploaded"
                        putMVar message $ PartN {partnumber = i, 
                                                 etag       = etag_ json} 
            Error _ -> do
                        putStrLn $ "upload part " ++ show i ++ " fail\nretry..."
                        upload_part files_path i bucketS3 key_ filepath upload_id_ message)
          (\_ -> do
                  putStrLn $ "upload part " ++ show i ++ " failed\nretry..."
                  upload_part files_path i bucketS3 key_ filepath upload_id_ message)


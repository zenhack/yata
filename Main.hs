{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where

import Data.String (fromString)

import Control.Monad.Trans.Class (lift)
import Text.Heredoc (here)
import Web.Scotty
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple (FromRow(..))

import qualified Data.Text.Lazy as T

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Blaze.Html.Renderer.Pretty (renderHtml)

data TODO = TODO Int Bool T.Text
    deriving(Show, Eq)

instance FromRow TODO where
    fromRow = TODO <$> DB.field <*> DB.field <*> DB.field

main :: IO ()
main = do
    conn <- DB.open "test.sqlite3" -- TODO: use a more
                                   -- reasonable path.
    DB.execute_ conn
        [here|CREATE TABLE IF NOT EXISTS todos (
            id INTEGER PRIMARY KEY,
            done BOOL NOT NULL,
            descr VARCHAR NOT NULL
        )|]

    scotty 8080 $ do
        get "/" $ do
            rows <- lift $ (DB.query_ conn [here|
                                SELECT id, done, descr
                                FROM "todos"
                                |] :: IO [TODO])
            -- TODO: go straight from html -> text; don't
            -- go through String.
            html $ T.pack $ renderHtml $ mainPage rows


mainPage :: [TODO] -> H.Html
mainPage todos = H.docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "utf-8"
        H.title "Yet Another TODO"
    H.body $ H.table $ do
        H.tr $ do
            H.th "Done?" -- TODO: make this a link/
                         -- set sorting order.
            H.th "Description" -- same thing.
        mapM_ todoTR todos

todoTR :: TODO -> H.Html
todoTR (TODO id done descr) = H.tr $ do
    H.td $ H.form ! A.id (fromString $ "donebox-" ++ show id)
                  ! A.method "post"
                  ! A.action (fromString $ "/todos/" ++ show id ++ "/done") $ do
                H.input ! A.name "done"
                        ! A.type_ "checkbox"
    H.td $ H.toHtml descr

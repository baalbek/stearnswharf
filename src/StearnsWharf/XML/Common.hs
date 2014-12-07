{-# LANGUAGE NamedFieldPuns,RecordWildCards #-}
module StearnsWharf.XML.Common where

import Data.Maybe (fromJust)
import qualified Text.XML.Light as X 
import qualified Data.List as LU

type AttValPair = (String,String)

toQName :: String -> X.QName
toQName s = X.QName s Nothing Nothing

xmlAttr :: String -> X.Element -> Maybe String 
xmlAttr s el = X.findAttr (toQName s) el

xmlElement :: String -> X.Element -> Maybe X.Element
xmlElement s doc = X.findElement (X.unqual s) doc

xmlElements :: String -> X.Element -> [X.Element]
xmlElements s doc = X.findElements (X.unqual s) doc

getAttValPair :: X.Attr -> AttValPair
getAttValPair attr = (k,v) 
    where k = X.qName $ X.attrKey attr
          v = X.attrVal attr

findAttVal :: [AttValPair] -> String -> String
findAttVal pairs aname = snd hit
    where (Just hit) = LU.find (\v -> (fst v) == aname) pairs

maybeFindAttVal :: [AttValPair] -> String -> Maybe String
maybeFindAttVal pairs aname = result
    where hit = LU.find (\v -> (fst v) == aname) pairs
          result | hit == Nothing = Nothing 
                 | otherwise = Just (snd $ fromJust hit) 

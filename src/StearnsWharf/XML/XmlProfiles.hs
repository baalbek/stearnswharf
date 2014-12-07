{-# LANGUAGE NamedFieldPuns,RecordWildCards #-}
module StearnsWharf.XML.XmlProfiles where

import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Control.Monad (mplus)
import Control.Monad.Reader (Reader,ask)
import qualified Text.XML.Light as X 

import qualified StearnsWharf.Beams as B
import qualified StearnsWharf.Nodes as N
import qualified StearnsWharf.Loads as L
import qualified StearnsWharf.XML.XmlLoads as XL
import qualified StearnsWharf.Steel.SteelProfiles as S
import qualified StearnsWharf.Wood.WoodProfiles as WP
import qualified StearnsWharf.XML.Common as XC


type ContextMaps = (N.NodeMap,XL.LoadMap)
type ProfileContext = Reader ContextMaps 

type ProfileParam =  (B.BeamId,
                      N.Node,
                      N.Node,
                      Maybe L.Load)
                   

getProfileParam :: N.NodeMap -> XL.LoadMap -> X.Element -> ProfileParam
getProfileParam nm lm el = (bid,n1,n2,myLoad)
    where Just bid = XC.xmlAttr "id" el
          myLoad = XC.xmlAttr "load" el >>=  flip Map.lookup lm
          Just n1id = XC.xmlAttr "n1" el
          Just n2id = XC.xmlAttr "n2" el
          Just n1 = Map.lookup n1id nm
          Just n2 = Map.lookup n2id nm


-- | DB Steel Profiles
createDbSteelProfile :: Maybe S.SteelProfile -- ^ Default SteelProfile in case dbid is not set in XML file
                        -> N.NodeMap 
                        -> XL.LoadMap 
                        -> X.Element 
                        -> B.Beam S.SteelProfile 
createDbSteelProfile sp nm lm el = B.Bjlk33 bid n1 n2 mySteel myLoad
    --where mySteel = maybe (fromJust sp) id $ XC.xmlAttr "dbid" el >>= S.steelProfileOf2  
    where Just mySteel = mplus (XC.xmlAttr "dbid" el >>= S.steelProfileOf2) sp
          (bid,n1,n2,myLoad) = getProfileParam nm lm el 

createDbSteelProfiles :: Maybe X.Element -> ProfileContext [B.Beam S.SteelProfile]
createDbSteelProfiles Nothing = return []
createDbSteelProfiles (Just el) = do 
    (nm,lm) <- ask
    let defaultSteel = XC.xmlAttr "default-dbid" el >>= S.steelProfileOf2 
    let createDbSteelProfile' = createDbSteelProfile defaultSteel nm lm
    return $ map createDbSteelProfile' $ XC.xmlElements "DBSteelProfile" el

createSteelProfiles :: X.Element -> ProfileContext [B.Beam S.SteelProfile]
createSteelProfiles doc = createDbSteelProfiles $ XC.xmlElement "SteelProfiles" doc 

-- | Wood Profiles
createWoodProfile :: Maybe String -> N.NodeMap -> XL.LoadMap -> X.Element -> B.Beam WP.WoodProfile
createWoodProfile defaultStc nm lm el = B.Bjlk33 bid n1 n2 myWood myLoad  --(WP.Wood w h glulam) myLoad
    where (bid,n1,n2,myLoad) = getProfileParam nm lm el
          w = read $ fromJust $ XC.xmlAttr "w" el
          h = read $ fromJust $ XC.xmlAttr "h" el
          Just stc = mplus (XC.xmlAttr "stclass" el) defaultStc
          myWood = WP.newWoodProfile stc w h 
          -- glulam = maybe True (\x -> if x == "true" then True else False) $ XC.xmlAttr "glulam" el 
                                                         
createWoodProfilesEl :: Maybe X.Element -> ProfileContext [B.Beam WP.WoodProfile ]
createWoodProfilesEl Nothing = return []
createWoodProfilesEl (Just el) = do
    (nm,lm) <- ask
    let defstc = XC.xmlAttr "default-stclass" el 
    let createWoodProfile' = createWoodProfile defstc nm lm
    return $ map createWoodProfile' $ XC.xmlElements "Wood" el

createWoodProfiles :: X.Element -> ProfileContext [B.Beam WP.WoodProfile ]
createWoodProfiles doc = woods
        where xmlprof = XC.xmlElement "WoodProfiles" doc
              woods = createWoodProfilesEl xmlprof



{-# LANGUAGE TemplateHaskell #-}

module Filter where

import Data.List

data Filter = Filter String

filterToSql :: Filter -> String
filterToSql (Filter f) = f

filtersToSql :: [Filter] -> String
filtersToSql [] = ""
filtersToSql filters = "WHERE " ++ (intercalate " AND " (fmap filterToSql filters))






-- lib/Glob/Database.hs
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glob.Database where


      import Data.Text
      import qualified Data.ByteString as B
      import Data.Time
      import Yesod



      share [mkPersist sqlSettings,mkMigrate "migrateAll"] [persistLowerCase|
        Nav json sql=table_nav
          Id sql=
          label Text sql=key_label
          order Int Maybe sql=key_order
          ref Text sql=key_ref
          Primary label
          deriving Eq Show
        Htm sql=table_html
          Id sql=
          index Text sql=key_index
          html Text sql=key_html
          title Text sql=key_title
          typ Text sql=key_content
          time Day sql=key_time
          Primary index
        Txt sql=table_txt
          Id sql=
          index Text sql=key_index
          txt Text sql=key_text
          content Text sql=key_content
          Primary index
        Bin sql=table_bin
          Id sql=
          index Text sql=key_index
          bin B.ByteString sql=key_binary
          content Text sql=key_content
          Primary index
        Qry sql=table_query
          Id sql=
          index Text sql=key_index
          txt Text sql=key_text
          Primary index
      |]

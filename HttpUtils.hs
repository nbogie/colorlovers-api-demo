module HttpUtils (getBody) where

import Network.HTTP
import Network.Browser

getBody :: String -> IO (Either String String)
getBody url = do
  (_, rsp) <- Network.Browser.browse $ do
           setAllowRedirects True -- handle HTTP redirects
           request $ getRequest url
  -- TODO: handle errors
  return $ Right (rspBody rsp)


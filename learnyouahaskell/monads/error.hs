-- Maybe does not allow use to save context of error.
-- So there is Either for that.
--
-- instance (Error e) => Monad (Either e) where
--     return x = Right x
--     Right x >>= f = f x
--     Left err >>= f = Left err
--     fail msg = Left (strMsg msg)
--
--   :t strMsg: strMsg :: Error a => String -> a

import qualified Db

-- while :: Monad m => m (Maybe a) -> (a -> m b) -> m [b]
-- while act f = do
--   ma <- act
--   case ma of
--     Nothing -> return []
--     Just a -> liftM2 (:) (f a) (while act f)

while_ :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
while_ act f = do
  ma <- act
  case ma of
    Nothing -> return ()
    Just a -> f a >> while_ act f

main :: IO ()
main =
  Db.withDb "/tmp/db.db" $ \db ->
    Db.withCursor db $ \cursor ->
      while_ (Db.nextKeyBS cursor) $ \(key, value) ->
        putStrLn $ show key ++ " = " ++ show value

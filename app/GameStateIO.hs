module GameStateIO (saveGameState, loadGameState) where

import LI12425 (Jogo)

-- | Guarda o jogo atual num ficheiro
saveGameState :: FilePath -> Jogo -> IO ()
saveGameState filePath jogo = writeFile filePath (show jogo)

-- | Carrega o jogo guardado num ficheiro
loadGameState :: FilePath -> IO (Maybe Jogo)
loadGameState filePath = do
  content <- readFile filePath
  return $ Just (read content)

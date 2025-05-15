module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Imagem (carregarImagem)
import ImmutableTowers
import LI12425
import Tempo

-- | Janela do jogo.
janela :: Display
janela = FullScreen

-- | Cor do fundo.
fundo :: Color
fundo = black

-- | Frames por segundo.
fr :: Int
fr = 600

desenhaIO :: ImmutableTowers -> IO Picture
desenhaIO it = return $ desenha it

reageTempoIO :: Tempo -> ImmutableTowers -> IO ImmutableTowers
reageTempoIO tempo it = return $ reageTempo tempo it

jogoInicio :: Jogo
jogoInicio =
  Jogo
    { baseJogo =
        Base
          { vidaBase = 0,
            posicaoBase = (0, 0),
            creditosBase = 0
          },
      portaisJogo =
        [],
      torresJogo =
        [],
      mapaJogo = [[]],
      mapaDirecaoJogo = [[]],
      inimigosJogo =
        [],
      lasersJogo = [],
      lojaJogo =
        []
    }

-- | Função principal que inicializa e executa o jogo.
main :: IO ()
main = do
  imagem <- carregarImagem 0
  playIO
    janela
    fundo
    fr
    ImmutableTowers {jogoAtual = jogoInicio, jogoInicial = jogoInicio, decorrerJogo = MenuPrincipal, imagens = imagem}
    desenhaIO
    reageEventosIO
    reageTempoIO
